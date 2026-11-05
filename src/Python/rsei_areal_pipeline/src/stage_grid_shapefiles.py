"""
================================================================================
Stage RSEI 810m Grid Shapefiles into Snowflake
================================================================================

Loads the EPA RSEI 810-meter CONUS grid shapefiles (BOTTOM and TOP halves) into
Snowflake as a GEOGRAPHY table in WGS84 (EPSG:4326), producing:

    <Database>.<schema>.GRID_SHAPE_810M_V1

This is STEP 1 of the RSEI areal interpolation pipeline and must be run before
rsei_tobler_processor.py. It replaces the grid table that downstream steps join
against, so it rebuilds the table on each run (DROP + CREATE).

Reads shapefile paths and Snowflake credentials from config.yaml:

    paths:
      shapefile_directory: /path/to/RSEI/SHAP
      bottom_shapefile: poly_gc14_conus_810m_bottom.shp
      top_shapefile: poly_gc14_conus_810m_top.shp

Authors: Soliman Kan, Hamza Turabieh, Xing Song
================================================================================
"""

import os
import yaml
import pandas as pd
import geopandas as gpd
import snowflake.connector
from snowflake.connector.pandas_tools import write_pandas

TARGET_SCHEMA = ""
TARGET_TABLE = "GRID_SHAPE_810M_V1"
WGS84 = "EPSG:4326"  # required for Snowflake GEOGRAPHY


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


def process_shapefile_with_correct_crs(shp_path, layer_name):
    """
    Read a shapefile and return a DataFrame with a WGS84 WKT column, ready for
    Snowflake's GEOGRAPHY type.
    """
    print(f"Processing {shp_path} ...")

    gdf = gpd.read_file(shp_path)
    print(f"  Original CRS: {gdf.crs}")
    print(f"  Features: {len(gdf):,}")

    if gdf.crs is None:
        # No CRS defined — decide based on coordinate magnitude.
        bounds = gdf.total_bounds  # [minx, miny, maxx, maxy]
        if bounds[0] < -180 or bounds[2] > 180:
            # Projected coordinates — assume Albers CONUS, then convert.
            print("  No CRS; coordinates look projected -> assuming Albers CONUS")
            gdf = gdf.set_crs("ESRI:102003").to_crs(WGS84)
        else:
            print("  No CRS; coordinates look geographic -> setting WGS84")
            gdf = gdf.set_crs(WGS84)
    elif gdf.crs.is_geographic:
        if str(gdf.crs) != WGS84:
            gdf = gdf.to_crs(WGS84)
            print(f"  Transformed geographic CRS -> {WGS84}")
        else:
            print("  Already WGS84")
    else:
        gdf = gdf.to_crs(WGS84)
        print(f"  Transformed projected CRS -> {WGS84}")

    gdf["GRID_LAYER"] = layer_name
    gdf["WKT"] = gdf["geometry"].apply(lambda geom: geom.wkt)
    df = gdf.drop(columns=["geometry"])

    b = gdf.total_bounds
    print(f"  Final WGS84 bounds: lon {b[0]:.6f}..{b[2]:.6f}, lat {b[1]:.6f}..{b[3]:.6f}")
    if -180 <= b[0] <= 180 and -90 <= b[1] <= 90:
        print("  Coordinates valid for GEOGRAPHY")
    else:
        print("  WARNING: coordinates may be invalid for GEOGRAPHY")

    return df


def create_final_table(conn, schema_name, table_name):
    """Drop and recreate the final grid table."""
    cursor = conn.cursor()
    cursor.execute(f"DROP TABLE IF EXISTS {schema_name}.{table_name}")
    print(f"Dropped existing table {schema_name}.{table_name}")
    cursor.execute(
        f"""
        CREATE TABLE {schema_name}.{table_name} (
            CELLX INTEGER,
            CELLY INTEGER,
            CLAT FLOAT,
            CLONG FLOAT,
            CX INTEGER,
            CY INTEGER,
            GRID_LAYER VARCHAR(16),
            WKT VARCHAR(16777216),
            SHAPE GEOGRAPHY
        )
        """
    )
    print(f"Created table {schema_name}.{table_name}")
    cursor.close()


def load_data_to_snowflake(conn, df, schema_name, table_name, chunk_size=50000):
    """Load DataFrame to Snowflake in chunks, converting WKT -> GEOGRAPHY."""
    total_rows = 0
    num_chunks = (len(df) + chunk_size - 1) // chunk_size

    for chunk_num, start in enumerate(range(0, len(df), chunk_size)):
        chunk = df.iloc[start:start + chunk_size]
        print(f"Loading chunk {chunk_num + 1}/{num_chunks} ({len(chunk):,} rows) ...")

        cursor = conn.cursor()
        temp_table = f"TEMP_LOAD_{chunk_num}"

        cursor.execute(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE {schema_name}.{temp_table} (
                CELLX INTEGER,
                CELLY INTEGER,
                CLAT FLOAT,
                CLONG FLOAT,
                CX INTEGER,
                CY INTEGER,
                GRID_LAYER VARCHAR(16),
                WKT VARCHAR(16777216)
            )
            """
        )

        success, _, _, _ = write_pandas(
            conn=conn,
            df=chunk,
            table_name=temp_table,
            schema=schema_name,
            quote_identifiers=False,
        )
        if not success:
            cursor.close()
            raise Exception(f"Failed to load chunk {chunk_num}")

        cursor.execute(
            f"""
            INSERT INTO {schema_name}.{table_name}
            (CELLX, CELLY, CLAT, CLONG, CX, CY, GRID_LAYER, WKT, SHAPE)
            SELECT
                CELLX, CELLY, CLAT, CLONG, CX, CY, GRID_LAYER, WKT,
                ST_GEOGRAPHYFROMWKT(WKT) AS SHAPE
            FROM {schema_name}.{temp_table}
            """
        )
        rows_inserted = cursor.rowcount
        total_rows += rows_inserted

        cursor.execute(f"DROP TABLE {schema_name}.{temp_table}")
        cursor.close()
        print(f"  Inserted {rows_inserted:,} rows")

    return total_rows


def verify_data(conn, schema_name, table_name):
    """Basic sanity checks on the loaded data."""
    cursor = conn.cursor()
    cursor.execute(
        f"""
        SELECT
            COUNT(*) AS total_rows,
            COUNT(DISTINCT GRID_LAYER) AS layers,
            MIN(CLAT) AS min_lat, MAX(CLAT) AS max_lat,
            MIN(CLONG) AS min_long, MAX(CLONG) AS max_long
        FROM {schema_name}.{table_name}
        """
    )
    s = cursor.fetchone()
    print("\n=== VERIFICATION ===")
    print(f"Total rows: {s[0]:,}")
    print(f"Layers: {s[1]} (expected 2: BOTTOM, TOP)")
    print(f"Latitude range:  {s[2]:.4f} to {s[3]:.4f}")
    print(f"Longitude range: {s[4]:.4f} to {s[5]:.4f}")

    cursor.execute(
        f"SELECT COUNT(*) FROM {schema_name}.{table_name} WHERE SHAPE IS NULL"
    )
    null_count = cursor.fetchone()[0]
    print("No null geometries" if null_count == 0
          else f"WARNING: {null_count:,} null geometries")
    cursor.close()


def main():
    try:
        config = load_config("config.yaml")
        print("Configuration loaded")
    except Exception as e:
        print(f"Failed to load config.yaml: {e}")
        print("Make sure config.yaml exists with 'snowflake' and 'paths' sections.")
        return

    shp_dir = config["paths"]["shapefile_directory"]
    bottom_shp = os.path.join(shp_dir, config["paths"]["bottom_shapefile"])
    top_shp = os.path.join(shp_dir, config["paths"]["top_shapefile"])

    conn = None
    try:
        print("\nStep 1: Processing shapefiles ...")
        df_bottom = process_shapefile_with_correct_crs(bottom_shp, "BOTTOM")
        df_top = process_shapefile_with_correct_crs(top_shp, "TOP")
        df_combined = pd.concat([df_bottom, df_top], ignore_index=True)
        print(f"Combined dataset: {len(df_combined):,} features")

        print("\nStep 2: Connecting to Snowflake ...")
        conn = connect_to_snowflake(config)
        print("Connected")

        print(f"\nStep 3: Creating table {TARGET_SCHEMA}.{TARGET_TABLE} ...")
        create_final_table(conn, TARGET_SCHEMA, TARGET_TABLE)

        print("\nStep 4: Loading data ...")
        total_rows = load_data_to_snowflake(conn, df_combined, TARGET_SCHEMA, TARGET_TABLE)
        print(f"Loaded {total_rows:,} rows")

        print("\nStep 5: Verifying ...")
        verify_data(conn, TARGET_SCHEMA, TARGET_TABLE)

        print(f"\nSUCCESS: {TARGET_SCHEMA}.{TARGET_TABLE} created in WGS84")

    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
    finally:
        if conn:
            conn.close()
            print("\nConnection closed")


if __name__ == "__main__":
    main()
