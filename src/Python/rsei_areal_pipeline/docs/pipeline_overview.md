# RSEI Areal Interpolation - Pipeline Overview

This document explains how the pipeline works, why key design choices were made,
and what to watch out for when maintaining or extending it. For quick setup and
run commands, see the top-level `README.md`.

---

## 1. Purpose

EPA's RSEI model reports environmental risk on a fixed **810-meter national grid**.
Most downstream analyses (including ALS4M) need those values at the **Census Block
Group (BG)** level instead. This pipeline redistributes each grid cell's pollution
values onto the block groups it overlaps, weighted by the area of overlap - i.e.
area-weighted *areal interpolation*- using the [`tobler`](https://pysal.org/tobler/)
library.

Output: one row per (block group × chemical × year) with area-weighted
`CONC`, `TOXCONC`, `SCORE`, `CCONC`, `NCCONC`.

---

## 2. Data flow

```
810m grid shapefiles (BOTTOM + TOP, CONUS)
        |  stage_grid_shapefiles.py   (Step 1)
        v
<Database>.<schema>.GRID_SHAPE_810M_V1        <- grid geometry, WGS84 GEOGRAPHY
        |
        |   joined on CELLX=X, CELLY=Y to
        |   <Database>.<schema>.RSEI_MICRO2022_<year>   <- grid-cell pollution values
        |
        |  rsei_tobler_processor.py       (Step 2)
        |   + <Database>.CENSUS_SHAPEFILE.BG_SHAPE_<year>   <- target polygons
        v
<Database>.<schema>.RSEI_BG_AREAL_TOBLER_<year>   <- block-group pollution values
```

The downstream ALS exposure join (patient address-years → RSEI) is maintained
separately and not included in this public repo.

---

## 3. The three steps

### Step 1 -`src/stage_grid_shapefiles.py`

Loads the two 810 m CONUS grid shapefiles (a BOTTOM and a TOP half) into Snowflake
as a single GEOGRAPHY table, `GRID_SHAPE_810M_V1`.

- Reads shapefile paths from `config.yaml` (`paths:` section).
- Reprojects geometry to **WGS84 (EPSG:4326)**, which Snowflake's `GEOGRAPHY`
  type requires.
- Loads in chunks via a temp table, converting WKT → `GEOGRAPHY` with
  `ST_GEOGRAPHYFROMWKT`.
- **Rebuilds the table on each run** (DROP + CREATE). Run it **once**; there is no
  reason to re-run it unless the source grid changes.

### Step 2 - `src/rsei_tobler_processor.py`

The core interpolation. For the configured year:

1. Discovers all states and their block-group counts.
2. For each state, downloads block-group polygons and the grid cells + pollution
   values covering that area.
3. Runs Tobler area-weighted interpolation, one chemical at a time.
4. Uploads results to `RSEI_BG_AREAL_TOBLER_<year>`.

Output schema:

| Column | Notes |
|--------|-------|
| `CENSUS_BLOCK_GROUP` | BG GEOID |
| `CHEMICAL_NUMBER` | RSEI chemical id |
| `CONC, TOXCONC, SCORE, CCONC, NCCONC` | area-weighted values |
| `YEAR` | processed year |
| `STATE_FIPS` | state of the block group |

### Step 3 - `sql/als_exposure_windows.sql`

Downstream consumer (ALS4M). Builds patient address-year records for the ALS and
control cohorts, flags short/medium/long-term exposure windows relative to each
patient's index date, and joins them to the Step 2 output across 2011–2022.
This is a **reference SQL script**, not an automated job - the trailing SELECTs
are spot-checks with `LIMIT`s.

---

## 4. Design decisions (and the reasoning behind them)

### Coordinate reference systems

Three CRSs appear, deliberately:

- **WGS84 (EPSG:4326)** - how grid geometry is *stored* in Snowflake (GEOGRAPHY
  requirement).
- **NAD83 (EPSG:4269)** - the native CRS of the Census block-group geometries;
  set explicitly on download so the data is labeled correctly before reprojection.
- **Web Mercator (EPSG:3857)** - both layers are reprojected to this **before
  interpolation**. Area-weighting needs a projected (not geographic) CRS so that
  overlap *areas* are meaningful; doing area math directly in lat/lon degrees is
  wrong.

> **Why this matters:** an earlier version mismatched CRSs between the grid and the
> block groups, so the two layers did not overlay and interpolation produced
> empty/incorrect results. Always confirm both `GeoDataFrame`s share
> `EPSG:3857` immediately before `area_interpolate` (the code asserts this).

### The `GRID_SHAPE_810M_V1` table

Step 2 joins against `GRID_SHAPE_810M_V1` specifically - **not** any earlier
"processed grid" table.

> **History / caution:** a previous grid-processing approach wrote geometries that
> came out with coordinates outside the U.S. (a projection/encoding problem in how
> the geometry was converted to text). `GRID_SHAPE_810M_V1`, produced by Step 1
> with explicit WGS84 handling, is the validated source. If you ever see
> interpolated points landing in the wrong location, re-verify the grid table
> before anything else - check that `CLAT`/`CLONG` fall within CONUS and that
> `ST_ASWKT(SHAPE)` agrees with them.

### Adaptive state/county processing

Loading all ~20.7M grid cells into memory at once is not feasible on a typical
workstation (it exhausted a 16 GB machine). Instead:

- Small states (≤ 1,000 block groups) are processed whole.
- Large states are processed **county-by-county**.
- Within each area, only the grid cells inside that area's bounding box are
  downloaded.

This keeps peak memory bounded regardless of total dataset size.

### Chemical batching + explicit GC

There are ~900 distinct chemicals. Interpolation runs per chemical, batched
(default 100 per batch), with `gc.collect()` between batches and results uploaded
incrementally. This bounds memory further and means a failure part-way through
doesn't lose everything already uploaded.

### Resume safety

Before processing a state or county, the code checks whether rows already exist in
the output table for it and skips if so. A re-run after an interruption continues
where it stopped rather than duplicating work.

---

## 5. Processing one year at a time

`rsei_tobler_processor.py` processes a **single year per run**, set by the `YEAR`
variable in `main()`. The year drives the input tables (`RSEI_MICRO2022_<year>`,
`BG_SHAPE_<year>`) and the output table (`RSEI_BG_AREAL_TOBLER_<year>`).

To produce the full 2011–2022 range that Step 3 consumes, run Step 2 once per year,
changing `YEAR` each time. (Automating the year loop is a natural future
improvement.)

---

## 6. Runtime expectations

Driven by state size; rough per-year guidance:

| State size | Example | Approx time |
|-----------|---------|-------------|
| Large | TX, CA | ~15–17 h each |
| Medium | KS, MO, UT | ~4–5 h each |
| Small | WY, VT, AK | 15 min – 1 h each |

A full U.S. year is on the order of tens of hours. **Test with a few small states
first** (`process_specific_states(['56','50','02'])`) before committing to a full run.

---

## 7. Troubleshooting

| Symptom | Likely cause | Where to look |
|---------|--------------|---------------|
| Interpolated values land outside the U.S. | wrong/old grid table | re-verify `GRID_SHAPE_810M_V1` CLAT/CLONG vs `ST_ASWKT(SHAPE)` |
| Empty interpolation output | CRS mismatch before `area_interpolate` | confirm both layers are EPSG:3857 |
| `0 records` for an area | no RSEI chemicals in that bounding box | expected for some rural areas; check `get_available_chemicals` |
| Connector / `pyarrow` warning on connect | `pyarrow` version outside supported range | pin `pyarrow` per `requirements.txt` |
| Memory pressure | area too large to hold at once | lower `chemical_batch_size`, or confirm large states route to county-by-county |
| Step 3 returns no rows | Step 2 output for those years not yet built | confirm `RSEI_BG_AREAL_TOBLER_<year>` is populated |

---

## 8. File map

| Path | Role |
|------|------|
| `src/stage_grid_shapefiles.py` | Step 1 - build `GRID_SHAPE_810M_V1` |
| `src/rsei_tobler_processor.py` | Step 2 - areal interpolation |
| `sql/als_exposure_windows.sql` | Step 3 - ALS exposure joins |
| `config.yaml.example` | credential/path template (copy to `config.yaml`) |
| `requirements.txt` | Python dependencies |
