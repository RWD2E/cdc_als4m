# CDISC YAML SQL Compiler

## Files

- `cdisc_yaml_sql_compiler.py`: generic YAML-to-SQL compiler.
- `cdisc_sql_environment.example.yaml`: deployment-specific object names, joins, filters, and value-set relations.
- `pcornet_als_natural_history_to_sdtm.yaml`: PCORnet-to-SDTM transformation specification.

## Requirements

```bash
pip install pyyaml
```

## List available domain mappings

```bash
python cdisc_yaml_sql_compiler.py \
  pcornet_als_natural_history_to_sdtm.yaml \
  --list
```

## Generate one Snowflake domain

```bash
python cdisc_yaml_sql_compiler.py \
  pcornet_als_natural_history_to_sdtm.yaml \
  --environment cdisc_sql_environment.example.yaml \
  --domain DM \
  --dialect snowflake \
  --mode create_table \
  --out-dir generated_sql \
  --validations
```

## Generate all domains

```bash
python cdisc_yaml_sql_compiler.py \
  pcornet_als_natural_history_to_sdtm.yaml \
  --environment cdisc_sql_environment.example.yaml \
  --dialect snowflake \
  --mode create_table \
  --out-dir generated_sql \
  --validations
```

## Generate multiple SQL operation types

```bash
python cdisc_yaml_sql_compiler.py \
  pcornet_als_natural_history_to_sdtm.yaml \
  --environment cdisc_sql_environment.example.yaml \
  --domain QS \
  --mode all \
  --dialect postgres
```

Supported modes:

- `select`
- `create_table`
- `create_view`
- `insert`
- `merge`
- `all`

Supported dialects:

- `ansi`
- `snowflake`
- `postgres`
- `bigquery`
- `sqlserver`
- `spark`
- `duckdb`

## Strict and non-strict behavior

Strict mode is the default. It fails when a Required target variable or physical join predicate cannot be resolved.

Use `--non-strict` during specification development. The generated SQL will contain comments such as:

```sql
NULL /* UNMAPPED:DMDTC */
1 = 1 /* TODO: configure join predicate */
/* LOAD_VALUESET:file.csv */
```

Do not run non-strict output in production without resolving these markers.

## Environment YAML

The transformation specification describes logical entities. The environment file supplies deployment-specific details:

```yaml
objects:
  PCORNET_CDM.DEMOGRAPHIC: RAW.PCORNET.DEMOGRAPHIC
  ALS_SDTM.DM: ANALYTICS.SDTM.DM

joins:
  PCORNET_DEMOGRAPHIC_TO_DM:
    - src.PATID = dth.PATID
    - src.PATID = coh.PATID
    - src.PATID = tl.PATID

filters:
  PCORNET_DEMOGRAPHIC_TO_DM: coh.IN_COHORT = 1

value_sets:
  PCORNET_SEX_TO_SDTM_SEX.csv: REFERENCE.CDISC.PCORNET_SEX_TO_SDTM_SEX
```

Join predicates are positional after the first source relation. For example, when the mapping relation order is `src`, `dth`, `coh`, `tl`, the three configured predicates join `dth`, then `coh`, then `tl`.

## YAML expression rules

Placeholders:

- `{s0}`, `{s1}`, ...: source columns
- `{b0}`, `{b1}`, ...: bridge columns
- `{t0}`, `{t1}`, ...: target columns

Multiple target expressions are separated by top-level commas:

```yaml
sources: [{entity: src, column: DEATH_DATE}]
targets:
  - {entity: tgt, column: DTHDTC}
  - {entity: tgt, column: DTHFL}
transform: >
  TO_ISO8601_DATE({s0}),
  IFF({s0} IS NOT NULL, 'Y', NULL)
```

The parser respects commas inside function calls, quoted strings, and nested parentheses.

## Value-set mappings

For executable SQL, load each CSV into a database relation and register it under `value_sets` in the environment YAML. The compiler uses the alias declared in the mapping rule.

For mappings where source and value-set column names differ, add an explicit join expression:

```yaml
valueset_map:
  file: PCORNET_SEX_TO_SDTM_SEX.csv
  alias: sexct
  join_on: "{s0} = sexct.SOURCE_SEX"
```

## Generated validation SQL

With `--validations`, the compiler generates checks for:

- null Required variables;
- duplicate declared dataset keys.

These checks supplement, but do not replace, CDISC CORE, FDA business-rule validation, controlled-terminology validation, Define-XML validation, or scientific review.
