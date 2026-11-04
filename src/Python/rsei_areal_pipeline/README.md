# RSEI Areal Interpolation Pipeline

Area-weighted spatial interpolation of EPA **RSEI** (Risk-Screening Environmental
Indicators) pollution data from 810-meter grid cells to U.S. **Census Block Groups**,
using the [`tobler`](https://pysal.org/tobler/) library. Built to support the ALS4M
project and reusable for other analyses needing RSEI exposure at the block-group level.

> **Authors:** Soliman Kan, Hamza Turabieh, Xing Song

---

## What the pipeline does

For each Census Block Group polygon, it finds every overlapping 810 m RSEI grid cell,
computes how much of each cell falls inside the block group, and distributes the pollution
values (`CONC`, `TOXCONC`, `SCORE`, `CCONC`, `NCCONC`) by that overlap area. All geometry is
projected to **EPSG:3857 (Web Mercator)** first so area ratios are consistent.

---

The scripts are **dependent** and must run in this order:

| Step | File | Purpose | Produces |
|------|------|---------|----------|
| 1 | `src/stage_grid_shapefiles.py` | Loads 810 m grid shapefiles into Snowflake with correct WGS84 geometry | `<Database>.<schema>.GRID_SHAPE_810M_V1` |
| 2 | `src/rsei_tobler_processor.py` | Area-weighted interpolation, grid → block group | `<Database>.<schema>.RSEI_BG_AREAL_TOBLER_<year>` |

> The downstream ALS exposure join (patient address-years → RSEI) is maintained separately and not included in this public repo.

> **Step 1 builds the corrected grid table Step 2 depends on.** Do not point Step 2 at an
> older grid table - earlier grid tables had a coordinate-projection problem that placed
> cells outside the U.S. `GRID_SHAPE_810M_V1` is the validated one.

---

## Inputs & outputs

**Inputs**
- RSEI microdata: `<Database>.<schema>.RSEI_MICRO2022_<year>`
- Census block groups: `<Database>.CENSUS_SHAPEFILE.BG_SHAPE_<year>`
- Grid geometries: `<Database>.<schema>.GRID_SHAPE_810M_V1` (from Step 1)

**Output** - `RSEI_BG_AREAL_TOBLER_<year>`:

| Column | Description |
|--------|-------------|
| `CENSUS_BLOCK_GROUP` | Block group GEOID |
| `CHEMICAL_NUMBER` | RSEI chemical identifier |
| `CONC`, `TOXCONC`, `SCORE`, `CCONC`, `NCCONC` | Area-weighted pollution measures |
| `YEAR` | Calendar year |
| `STATE_FIPS` | State FIPS code |

---

## Processing strategy

- **Adaptive geography:** small states (≤ 1,000 block groups) processed whole; large states
  processed county-by-county to keep memory bounded.
- **Chemical batching:** chemicals processed in batches (default 100) with explicit garbage
  collection between batches.
- **Resume-safe:** areas already present in the output table are skipped on re-run.
- **Incremental upload:** results written to Snowflake after each batch.

> **Note on year:** `rsei_tobler_processor.py` currently targets a **single hardcoded year**.
> To produce another year, update the year reference(s) and the output table name, then
> re-run. (See *Known limitations*.)

---

## Setup

```bash
# 1. Create and activate a virtual environment
python -m venv rsei_env
source rsei_env/bin/activate      # Windows: rsei_env\Scripts\activate

# 2. Install dependencies
pip install -r requirements.txt

# 3. Create your config from the template and fill in credentials
cp config.yaml.example config.yaml
```

`config.yaml` holds Snowflake credentials and shapefile paths. **It is git-ignored and must
never be committed.** See `config.yaml.example` for the required structure.

---

## Usage

```bash
# Step 1 - stage the grid (run once; rebuilds GRID_SHAPE_810M_V1)
python src/stage_grid_shapefiles.py

# Step 2 - run the interpolation
python src/rsei_tobler_processor.py
```

Inside `rsei_tobler_processor.py`, `main()` exposes two options:
- `process_all_states()` - full U.S. run
- `process_specific_states([...])` - a subset of state FIPS codes, useful for testing

Step 3 is run as SQL in Snowflake.

---

## Runtime expectations

A full U.S. run is on the order of **tens of hours per year**, driven by state size (large
states such as TX/CA dominate). Test with a few small states first
(e.g. Wyoming `56`, Vermont `50`, Alaska `02`).

---

## Requirements

- Python 3.11
- Snowflake access with a role permitted to read the EPA_RSEI / CENSUS_SHAPEFILE schemas and
  write to `EPA_DB.EPA_RSEI`
- See `requirements.txt` for Python packages

---

## Known limitations / planned improvements

- **Year is hardcoded** in `rsei_tobler_processor.py`; producing 2011–2022 currently means
  editing the year per run. Parameterizing (e.g. `--year`) is the top planned improvement.
- `pyarrow` must satisfy the Snowflake connector's supported version range.

---

## Repository layout

```
rsei-areal-pipeline/
├── README.md
├── .gitignore
├── config.yaml.example
├── requirements.txt
├── src/
│   ├── stage_grid_shapefiles.py
│   └── rsei_tobler_processor.py
├── sql/
│   └── als_exposure_windows.sql
└── docs/
    └── pipeline_overview.md
```
