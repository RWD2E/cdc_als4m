"""
================================================================================
RSEI Tobler Area-Weighted Interpolation Processor
================================================================================

Author:  Soliman Kan, Hamza Turabieh, Xing Song
Date:    June 2025
Version: 1.1

DESCRIPTION:
    Area-weighted spatial interpolation of EPA RSEI (Risk-Screening Environmental
    Indicators) pollution data from 810-meter grid cells to Census Block Group (BG)
    polygons using the Tobler library (https://pysal.org/tobler/).

    Adaptive strategy per state:
      - Small states (<= 1,000 block groups): process the whole state, chemical-batched
      - Large states (> 1,000 block groups):  process county-by-county, chemical-batched

INPUT DATA:
    - RSEI grid values:   EPA_DB.EPA_RSEI.RSEI_MICRO2022_<YEAR>
    - Census block groups: EPA_DB.CENSUS_SHAPEFILE.BG_SHAPE_<YEAR>
    - Grid geometries:    EPA_DB.EPA_RSEI.GRID_SHAPE_810M_V1   (built by Step 1)

OUTPUT DATA:
    - EPA_DB.EPA_RSEI.RSEI_BG_AREAL_TOBLER_<YEAR>
    - Variables: CONC, TOXCONC, SCORE, CCONC, NCCONC

CONFIGURATION:
    Requires config.yaml with a `snowflake:` section
    (user, password, account, warehouse, database, role).

USAGE:
    # set the year to process at the top of main(), then:
    python rsei_tobler_processor.py

    # inside main(), choose one:
    #   processor.process_all_states()
    #   processor.process_specific_states(['56', '50', '02'])   # WY, VT, AK (testing)
================================================================================
"""

import gc
import time
import logging
import yaml
import pandas as pd
import geopandas as gpd
import snowflake.connector
from typing import List, Dict, Tuple
from tobler.area_weighted import area_interpolate

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler("rsei_tobler_processing.log"),
        logging.StreamHandler(),
    ],
)
logger = logging.getLogger(__name__)


def load_config(config_path="config.yaml"):
    """Load configuration from YAML file."""
    with open(config_path, "r") as file:
        return yaml.safe_load(file)


def connect_to_snowflake(config):
    """Establish connection to Snowflake using config."""
    return snowflake.connector.connect(
        user=config["snowflake"]["user"],
        password=config["snowflake"]["password"],
        account=config["snowflake"]["account"],
        warehouse=config["snowflake"]["warehouse"],
        database=config["snowflake"]["database"],
        role=config["snowflake"]["role"],
    )


class RSEIToblerProcessor:
    def __init__(self, year: int, config_path="config.yaml"):
        """
        Initialize the processor for a single calendar year.

        year: the RSEI/Census year to process. Drives the input tables
              (RSEI_MICRO2022_<year>, BG_SHAPE_<year>) and the output table
              (RSEI_BG_AREAL_TOBLER_<year>).
        """
        self.year = year
        self.config = load_config(config_path)
        self.conn = None

        self.large_state_threshold = 1000   # BG count above which we go county-by-county
        self.county_bg_threshold = 500       # BG count flagged as a large county
        self.chemical_batch_size = 100       # chemicals per batch (memory management)
        self.target_crs = "EPSG:3857"        # Web Mercator — consistent area calculations

        # Table names derived from the year (single source of truth)
        self.bg_table = f"EPA_DB.CENSUS_SHAPEFILE.BG_SHAPE_{year}"
        self.rsei_micro_table = f"EPA_DB.EPA_RSEI.RSEI_MICRO2022_{year}"
        self.grid_table = "EPA_DB.EPA_RSEI.GRID_SHAPE_810M_V1"
        self.output_table = f"EPA_DB.EPA_RSEI.RSEI_BG_AREAL_TOBLER_{year}"

    # ------------------------------------------------------------------ #
    # Connection / utilities
    # ------------------------------------------------------------------ #
    def connect_snowflake(self) -> bool:
        try:
            self.conn = connect_to_snowflake(self.config)
            logger.info("Connected to Snowflake successfully")
            return True
        except Exception as e:
            logger.error(f"Failed to connect to Snowflake: {e}")
            return False

    def clean_memory(self):
        gc.collect()

    # ------------------------------------------------------------------ #
    # Geography / chemical discovery
    # ------------------------------------------------------------------ #
    def get_all_states(self) -> List[Tuple[str, int]]:
        """All states with their block-group counts, largest first."""
        query = f"""
        SELECT LEFT(GEOID, 2) AS STATE_FIPS, COUNT(*) AS BG_COUNT
        FROM {self.bg_table}
        GROUP BY LEFT(GEOID, 2)
        ORDER BY BG_COUNT DESC
        """
        cursor = self.conn.cursor()
        cursor.execute(query)
        results = cursor.fetchall()
        cursor.close()
        states = [(row[0], row[1]) for row in results]
        logger.info(f"Found {len(states)} states to process")
        return states

    def get_state_counties(self, state_fips: str) -> List[Tuple[str, int]]:
        """All counties in a state with BG counts, largest first."""
        query = f"""
        SELECT LEFT(GEOID, 5) AS COUNTY_FIPS, COUNT(*) AS BG_COUNT
        FROM {self.bg_table}
        WHERE LEFT(GEOID, 2) = '{state_fips}'
        GROUP BY LEFT(GEOID, 5)
        ORDER BY BG_COUNT DESC
        """
        cursor = self.conn.cursor()
        cursor.execute(query)
        results = cursor.fetchall()
        cursor.close()
        counties = [(row[0], row[1]) for row in results]
        logger.info(f"State {state_fips}: found {len(counties)} counties")
        return counties

    def _bounds_query(self, state_fips=None, county_fips=None) -> str:
        if county_fips:
            where = f"LEFT(GEOID, 5) = '{county_fips}'"
        elif state_fips:
            where = f"LEFT(GEOID, 2) = '{state_fips}'"
        else:
            raise ValueError("Must specify either state_fips or county_fips")
        return f"""
        SELECT
            MIN(ST_X(ST_CENTROID(ST_GEOGRAPHYFROMWKT(GEOMETRY_WKT)))) - 0.5 AS min_lon,
            MAX(ST_X(ST_CENTROID(ST_GEOGRAPHYFROMWKT(GEOMETRY_WKT)))) + 0.5 AS max_lon,
            MIN(ST_Y(ST_CENTROID(ST_GEOGRAPHYFROMWKT(GEOMETRY_WKT)))) - 0.5 AS min_lat,
            MAX(ST_Y(ST_CENTROID(ST_GEOGRAPHYFROMWKT(GEOMETRY_WKT)))) + 0.5 AS max_lat
        FROM {self.bg_table}
        WHERE {where}
        """

    def get_available_chemicals(self, state_fips=None, county_fips=None) -> List[int]:
        """Distinct chemicals present in the grid cells covering an area's bounding box."""
        cursor = self.conn.cursor()
        cursor.execute(self._bounds_query(state_fips, county_fips))
        min_lon, max_lon, min_lat, max_lat = cursor.fetchone()
        cursor.close()

        query = f"""
        SELECT DISTINCT r.CHEMICALNUMBER
        FROM {self.grid_table} g
        JOIN {self.rsei_micro_table} r
            ON g.CELLX = r.X AND g.CELLY = r.Y
        WHERE g.CLONG BETWEEN {min_lon} AND {max_lon}
          AND g.CLAT  BETWEEN {min_lat} AND {max_lat}
        ORDER BY r.CHEMICALNUMBER
        """
        cursor = self.conn.cursor()
        cursor.execute(query)
        results = cursor.fetchall()
        cursor.close()
        chemicals = [row[0] for row in results]
        logger.info(f"Found {len(chemicals)} unique chemicals in area")
        return chemicals

    def check_if_processed(self, identifier: str, is_state: bool = True) -> bool:
        """True if this state/county already has rows in the output table."""
        if is_state:
            query = (f"SELECT COUNT(*) FROM {self.output_table} "
                     f"WHERE STATE_FIPS = '{identifier}'")
        else:
            query = (f"SELECT COUNT(*) FROM {self.output_table} "
                     f"WHERE CENSUS_BLOCK_GROUP LIKE '{identifier}%'")
        cursor = self.conn.cursor()
        cursor.execute(query)
        count = cursor.fetchone()[0]
        cursor.close()
        return count > 0

    # ------------------------------------------------------------------ #
    # Data download
    # ------------------------------------------------------------------ #
    def get_block_groups(self, state_fips=None, county_fips=None) -> gpd.GeoDataFrame:
        """Download block-group geometries and reproject to the target CRS."""
        if county_fips:
            where = f"LEFT(GEOID, 5) = '{county_fips}'"
            logger.info(f"Downloading block groups for county {county_fips}")
        elif state_fips:
            where = f"LEFT(GEOID, 2) = '{state_fips}'"
            logger.info(f"Downloading block groups for state {state_fips}")
        else:
            raise ValueError("Must specify either state_fips or county_fips")

        query = f"SELECT GEOID, GEOMETRY_WKT FROM {self.bg_table} WHERE {where}"
        cursor = self.conn.cursor()
        cursor.execute(query)
        results = cursor.fetchall()
        cursor.close()

        df = pd.DataFrame(
            [{"GEOID": r[0], "geometry": r[1]} for r in results]
        )
        gdf = gpd.GeoDataFrame(df, geometry=gpd.GeoSeries.from_wkt(df["geometry"]))
        gdf.crs = "EPSG:4269"  # NAD83
        gdf = gdf.to_crs(self.target_crs)
        logger.info(f"Downloaded {len(gdf)} block groups")
        return gdf

    def get_rsei_grids(self, state_fips=None, county_fips=None,
                       chemical_batch: List[int] = None) -> gpd.GeoDataFrame:
        """Download grid geometries + pollution values for an area and chemical batch."""
        if county_fips:
            logger.info(f"Downloading RSEI grids for county {county_fips}")
        elif state_fips:
            logger.info(f"Downloading RSEI grids for state {state_fips}")
        else:
            raise ValueError("Must specify either state_fips or county_fips")

        cursor = self.conn.cursor()
        cursor.execute(self._bounds_query(state_fips, county_fips))
        min_lon, max_lon, min_lat, max_lat = cursor.fetchone()
        cursor.close()

        chemical_filter = ""
        if chemical_batch:
            chem_list = ",".join(map(str, chemical_batch))
            chemical_filter = f"AND r.CHEMICALNUMBER IN ({chem_list})"

        query = f"""
        SELECT
            g.CELLX, g.CELLY, g.CLAT, g.CLONG,
            ST_ASWKT(g.SHAPE) AS geometry_wkt,
            r.CHEMICALNUMBER, r.CONC, r.TOXCONC, r.SCORE, r.CCONC, r.NCCONC
        FROM {self.grid_table} g
        JOIN {self.rsei_micro_table} r
            ON g.CELLX = r.X AND g.CELLY = r.Y
        WHERE g.CLONG BETWEEN {min_lon} AND {max_lon}
          AND g.CLAT  BETWEEN {min_lat} AND {max_lat}
          {chemical_filter}
        """
        cursor = self.conn.cursor()
        cursor.execute(query)
        results = cursor.fetchall()
        cursor.close()

        if not results:
            logger.warning("No RSEI data found for specified area and chemicals")
            return gpd.GeoDataFrame()

        df = pd.DataFrame(
            [{
                "CELLX": r[0], "CELLY": r[1], "CLAT": r[2], "CLONG": r[3],
                "geometry": r[4], "CHEMICALNUMBER": r[5],
                "CONC": r[6], "TOXCONC": r[7], "SCORE": r[8],
                "CCONC": r[9], "NCCONC": r[10],
            } for r in results]
        )
        gdf = gpd.GeoDataFrame(df, geometry=gpd.GeoSeries.from_wkt(df["geometry"]))
        gdf.crs = "EPSG:4269"  # NAD83
        gdf = gdf.to_crs(self.target_crs)
        logger.info(f"Downloaded {len(gdf)} RSEI grid cells with pollution data")
        return gdf

    # ------------------------------------------------------------------ #
    # Interpolation
    # ------------------------------------------------------------------ #
    def interpolate(self, source_gdf: gpd.GeoDataFrame,
                    target_gdf: gpd.GeoDataFrame) -> pd.DataFrame:
        """Area-weighted interpolation, one chemical at a time."""
        intensive_vars = ["CONC", "TOXCONC", "SCORE", "CCONC", "NCCONC"]
        assert source_gdf.crs == target_gdf.crs, "Source and target CRS must match"

        start = time.time()
        chemicals = source_gdf["CHEMICALNUMBER"].unique()
        results = []

        for chemical in chemicals:
            chem_source = source_gdf[source_gdf["CHEMICALNUMBER"] == chemical].copy()
            if len(chem_source) == 0:
                continue
            try:
                interpolated = area_interpolate(
                    source_df=chem_source,
                    target_df=target_gdf,
                    intensive_variables=intensive_vars,
                )
                interpolated["CHEMICALNUMBER"] = chemical
                interpolated["YEAR"] = self.year

                # Tobler may return GEOID on the index; ensure it's a column.
                if "GEOID" not in interpolated.columns:
                    if interpolated.index.name == "GEOID":
                        interpolated = interpolated.reset_index()
                    elif len(interpolated) == len(target_gdf):
                        interpolated["GEOID"] = target_gdf["GEOID"].values
                    else:
                        logger.error(f"Cannot match results to BGs for chemical {chemical}")
                        continue

                results.append(interpolated)
            except Exception as e:
                logger.error(f"  Error interpolating chemical {chemical}: {e}")
                continue

        final = pd.concat(results, ignore_index=True) if results else pd.DataFrame()
        logger.info(f"Interpolation produced {len(final)} records "
                    f"in {time.time() - start:.1f}s")
        return final

    # ------------------------------------------------------------------ #
    # Upload
    # ------------------------------------------------------------------ #
    def upload_results(self, results_df: pd.DataFrame, state_fips: str) -> bool:
        """Upload interpolation results to the output table (zeros skipped)."""
        if "GEOID" not in results_df.columns:
            if results_df.index.name == "GEOID":
                results_df = results_df.reset_index()
            else:
                logger.error("Cannot find GEOID in results")
                return False

        upload_data = []
        skipped = 0
        for _, row in results_df.iterrows():
            if (row["CONC"] == 0 and row["TOXCONC"] == 0 and row["SCORE"] == 0
                    and row["CCONC"] == 0 and row["NCCONC"] == 0):
                skipped += 1
                continue
            upload_data.append({
                "CENSUS_BLOCK_GROUP": str(row["GEOID"]),
                "CHEMICAL_NUMBER": int(row["CHEMICALNUMBER"]),
                "CONC": float(row["CONC"]) if pd.notna(row["CONC"]) else 0.0,
                "TOXCONC": float(row["TOXCONC"]) if pd.notna(row["TOXCONC"]) else 0.0,
                "SCORE": float(row["SCORE"]) if pd.notna(row["SCORE"]) else 0.0,
                "CCONC": float(row["CCONC"]) if pd.notna(row["CCONC"]) else 0.0,
                "NCCONC": float(row["NCCONC"]) if pd.notna(row["NCCONC"]) else 0.0,
                "YEAR": int(row["YEAR"]),
                "STATE_FIPS": state_fips,
            })

        logger.info(f"Prepared {len(upload_data)} non-zero records "
                    f"(skipped {skipped} all-zero)")
        if not upload_data:
            return True

        temp_table = f"TEMP_RSEI_TOBLER_{int(time.time())}"
        try:
            cursor = self.conn.cursor()
            cursor.execute("USE SCHEMA EPA_DB.EPA_RSEI")
            cursor.execute(
                f"""
                CREATE TEMPORARY TABLE {temp_table} (
                    CENSUS_BLOCK_GROUP STRING,
                    CHEMICAL_NUMBER INTEGER,
                    CONC FLOAT, TOXCONC FLOAT, SCORE FLOAT,
                    CCONC FLOAT, NCCONC FLOAT,
                    YEAR INTEGER,
                    STATE_FIPS STRING
                )
                """
            )
            cursor.execute(f"USE WAREHOUSE {self.config['snowflake']['warehouse']}")

            df_upload = pd.DataFrame(upload_data)
            batch_size = 10000
            insert_sql = f"""
                INSERT INTO {temp_table}
                (CENSUS_BLOCK_GROUP, CHEMICAL_NUMBER, CONC, TOXCONC, SCORE,
                 CCONC, NCCONC, YEAR, STATE_FIPS)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)
            """
            for i in range(0, len(df_upload), batch_size):
                batch = df_upload.iloc[i:i + batch_size]
                cursor.executemany(insert_sql, [tuple(r) for r in batch.values])

            cursor.execute(
                f"INSERT INTO {self.output_table} SELECT * FROM {temp_table}"
            )
            self.conn.commit()
            cursor.close()
            logger.info(f"Uploaded {len(upload_data)} records")
            return True
        except Exception as e:
            logger.error(f"Failed to upload results: {e}")
            return False

    # ------------------------------------------------------------------ #
    # Per-area processing
    # ------------------------------------------------------------------ #
    def _process_area(self, target_gdf, available_chemicals,
                      state_fips, area_label, download_kwargs) -> bool:
        """Shared chemical-batched processing loop for a state or county."""
        total_batches = ((len(available_chemicals) + self.chemical_batch_size - 1)
                         // self.chemical_batch_size)
        for i in range(0, len(available_chemicals), self.chemical_batch_size):
            chemical_batch = available_chemicals[i:i + self.chemical_batch_size]
            batch_num = (i // self.chemical_batch_size) + 1
            logger.info(f"{area_label}: chemical batch {batch_num}/{total_batches} "
                        f"({len(chemical_batch)} chemicals)")

            source_gdf = self.get_rsei_grids(chemical_batch=chemical_batch,
                                             **download_kwargs)
            if len(source_gdf) == 0:
                logger.warning(f"No RSEI data for batch {batch_num}")
                continue

            batch_results = self.interpolate(source_gdf, target_gdf)
            if len(batch_results) > 0:
                if not self.upload_results(batch_results, state_fips):
                    logger.error(f"Failed to upload batch {batch_num}")
                    return False

            del source_gdf, batch_results
            self.clean_memory()
        return True

    def process_small_state(self, state_fips: str, bg_count: int) -> bool:
        logger.info(f"Processing small state {state_fips} ({bg_count:,} BGs)")
        try:
            target_gdf = self.get_block_groups(state_fips=state_fips)
            chemicals = self.get_available_chemicals(state_fips=state_fips)
            if not chemicals:
                logger.warning(f"No RSEI data for state {state_fips}")
                return True
            return self._process_area(target_gdf, chemicals, state_fips,
                                      f"State {state_fips}",
                                      {"state_fips": state_fips})
        except Exception as e:
            logger.error(f"Error processing state {state_fips}: {e}")
            return False

    def process_county(self, county_fips: str, bg_count: int, state_fips: str) -> bool:
        logger.info(f"Processing county {county_fips} ({bg_count:,} BGs)")
        try:
            target_gdf = self.get_block_groups(county_fips=county_fips)
            chemicals = self.get_available_chemicals(county_fips=county_fips)
            if not chemicals:
                logger.warning(f"No RSEI data for county {county_fips}")
                return True
            return self._process_area(target_gdf, chemicals, state_fips,
                                      f"County {county_fips}",
                                      {"county_fips": county_fips})
        except Exception as e:
            logger.error(f"Error processing county {county_fips}: {e}")
            return False

    # ------------------------------------------------------------------ #
    # Orchestration
    # ------------------------------------------------------------------ #
    def process_specific_states(self, state_list: List[str]):
        logger.info(f"Processing states: {state_list}")
        all_states = self.get_all_states()
        target_states = [(f, c) for f, c in all_states if f in state_list]
        if not target_states:
            logger.error(f"No valid states found from list: {state_list}")
            return

        completed, failed = 0, []
        for state_fips, bg_count in target_states:
            logger.info(f"\nState {state_fips}: {bg_count:,} block groups")
            start = time.time()

            if self.check_if_processed(state_fips, is_state=True):
                logger.info(f"State {state_fips} already processed, skipping")
                completed += 1
                continue

            if bg_count <= self.large_state_threshold:
                success = self.process_small_state(state_fips, bg_count)
            else:
                counties = self.get_state_counties(state_fips)
                county_ok = 0
                for n, (county_fips, county_bg) in enumerate(counties, 1):
                    logger.info(f"State {state_fips}: county {n}/{len(counties)} "
                                f"({county_fips})")
                    if self.check_if_processed(county_fips, is_state=False):
                        logger.info(f"County {county_fips} already processed, skipping")
                        county_ok += 1
                        continue
                    if self.process_county(county_fips, county_bg, state_fips):
                        county_ok += 1
                    self.clean_memory()
                    time.sleep(1)
                success = (county_ok == len(counties))
                logger.info(f"State {state_fips}: {county_ok}/{len(counties)} counties ok")

            duration = time.time() - start
            self._print_state_status(state_fips, bg_count, success, duration)
            if success:
                completed += 1
            else:
                failed.append(state_fips)
            self.clean_memory()
            logger.info(f"Progress: {completed}/{len(target_states)} states")

        logger.info(f"\nPROCESSING COMPLETE: {completed}/{len(target_states)} states")
        if failed:
            logger.info(f"Failed states: {failed}")
        stats = self.get_processing_stats()
        logger.info(f"Final totals: {stats['total_records']:,} records across "
                    f"{stats['block_groups_completed']:,} block groups")

    def process_all_states(self):
        self.process_specific_states([f for f, _ in self.get_all_states()])

    # ------------------------------------------------------------------ #
    # Reporting
    # ------------------------------------------------------------------ #
    def _print_state_status(self, state_fips, bg_count, success, duration):
        status = "COMPLETED" if success else "FAILED"
        print("\n" + "=" * 60)
        print(f"STATE {state_fips} - {status} (year {self.year})")
        print(f"Block Groups: {bg_count:,}")
        print(f"Duration: {duration:.1f}s ({duration / 60:.1f} min)")
        if success:
            stats = self.get_state_stats(state_fips)
            print(f"Records Created: {stats['total_records']:,}")
            print(f"Chemicals Found: {stats['unique_chemicals']}")
        print("=" * 60)

    def get_state_stats(self, state_fips: str) -> Dict:
        query = f"""
        SELECT COUNT(*),
               COUNT(DISTINCT CENSUS_BLOCK_GROUP),
               COUNT(DISTINCT CHEMICAL_NUMBER)
        FROM {self.output_table}
        WHERE STATE_FIPS = '{state_fips}'
        """
        cursor = self.conn.cursor()
        cursor.execute(query)
        r = cursor.fetchone()
        cursor.close()
        return {"total_records": r[0], "unique_block_groups": r[1],
                "unique_chemicals": r[2]}

    def get_processing_stats(self) -> Dict:
        query = f"""
        SELECT COUNT(DISTINCT STATE_FIPS),
               COUNT(DISTINCT LEFT(CENSUS_BLOCK_GROUP, 5)),
               COUNT(DISTINCT CENSUS_BLOCK_GROUP),
               COUNT(*)
        FROM {self.output_table}
        """
        cursor = self.conn.cursor()
        cursor.execute(query)
        r = cursor.fetchone()
        cursor.close()
        return {"states_completed": r[0], "counties_completed": r[1],
                "block_groups_completed": r[2], "total_records": r[3]}

    def create_output_table(self):
        cursor = self.conn.cursor()
        cursor.execute(
            f"""
            CREATE TABLE IF NOT EXISTS {self.output_table} (
                CENSUS_BLOCK_GROUP VARCHAR(12),
                CHEMICAL_NUMBER NUMBER(38,0),
                CONC FLOAT, TOXCONC FLOAT, SCORE FLOAT,
                CCONC FLOAT, NCCONC FLOAT,
                YEAR NUMBER(38,0),
                STATE_FIPS VARCHAR(2)
            )
            """
        )
        self.conn.commit()
        cursor.close()
        logger.info(f"Output table {self.output_table} created/verified")


def main():
    # ---- set the year to process here ----
    YEAR = 2010

    processor = RSEIToblerProcessor(year=YEAR, config_path="config.yaml")
    try:
        if not processor.connect_snowflake():
            logger.error("Failed to connect to Snowflake")
            return
        processor.create_output_table()

        # Choose one:
        # processor.process_specific_states(['56', '50', '02'])   # WY, VT, AK — testing
        processor.process_all_states()

    except Exception as e:
        logger.error(f"Main processing error: {e}")
        import traceback
        traceback.print_exc()
    finally:
        if processor.conn:
            processor.conn.close()
            logger.info("Connection closed")


if __name__ == "__main__":
    main()