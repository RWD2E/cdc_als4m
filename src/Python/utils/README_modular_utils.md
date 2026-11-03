# Modular ALS CDE Python utilities

`gen_vs_json_utils_modular.py` replaces the original single-array loader with a filesystem `ConceptRegistry`. The previous `QueryFromJson`, `JsonBlockVS`, `json2ref`, and `expand_range` entry points remain available (some calling conventions change as documented below).

## Installation

Python 3.9+; install `pandas` to use `json2ref`. All other functions use the standard library.

Extract `als-cde-modular-prototype.zip` so you have a directory containing `concepts/`, `value-sets/` and `manifest.json`. Import from `gen_vs_json_utils_modular`.

## Query generation — noninteractive

```python
from gen_vs_json_utils_modular import QueryFromJson
q = QueryFromJson(
    url='als-cde-modular-prototype',
    sqlty='snow', cd_field='SOURCE_CODE',
    date_fields=['EVENT_DATE'], other_fields=['PERSON_ID'],
    srctbl_name='MY_DB.MEDICATIONS',
    sel_keys=['RILUZOLE'], sel_domain='rx',
    cdtype_field='CODE_SYSTEM',
    cdtype_map={'rxnorm': 'RXNORM', 'ndc': 'NDC'},
)
sql = q.gen_qry()
print(sql)
```

`sel_keys` accepts **semantic IDs or legacy names**. Output CD_GRP is now the stable semantic ID (not the legacy name). `cdtype_map` is recommended for reproducible production runs; if omitted the older interactive prompt is used. A concept with `valueSetRef` loads its coding list automatically. Inline `compose` still works.

**Semantics caution:** a `codePrecision/descendent-of` filter is currently implemented as a string prefix match (as in the previous script), **not** a true terminology closure. For SNOMED CT, and often ICD semantics, use a terminology service to pre-expand descendants. Do not treat this prefix approximation as clinical cohort validation. The original clinical mapping errors are preserved; review them independently. SQL field names and table names are trusted caller-supplied expressions: never accept these from untrusted web inputs.

`use_value_range=True` applies low/high **interpretation thresholds** with `<=`/`>=` per the legacy source convention, and joins them using OR if both are present; whether they apply to a given clinical concept needs clinical review. Disable via `gen_qry(use_value_range=False)` to retrieve all coded occurrences. For strict correctness, supply a value field only when threshold filtering is needed. Date fields are used in COALESCE for multi-date sources.

## Add a new concept

```python
from gen_vs_json_utils_modular import JsonBlockVS
editor = JsonBlockVS('als-cde-modular-prototype')
editor.add_json_block({
    'id': 'FVC-PCT-PREDICTED',
    'name': 'FVC percent predicted',
    'description': 'Percent of predicted forced vital capacity',
    'topic': 'finding', 'purpose': 'physiological markers',
    'relatedArtifact': {'class':'core', 'valueType':'decimal',
                        'valueRange': {'min':0,'max':150}},
    'compose':{'include':[{'system':'loinc','concept':[{'code':'19872-1'}]}]}
}, domain='respiratory-and-vital-measures')
```

This creates an independently managed JSON file and regenerates `manifest.json` without positional editing. When a compose definition contains 20+ code entries, it is externalized under `value-sets/` automatically. If manually maintaining response scales, use `valueRangeRef` pointing to a file under the registry root. All additions validate ID uniqueness; they do not silently overwrite source records.

## Commands

```bash
python gen_vs_json_utils_modular.py validate als-cde-modular-prototype
python gen_vs_json_utils_modular.py reindex als-cde-modular-prototype
python gen_vs_json_utils_modular.py export als-cde-modular-prototype expanded.json
python gen_vs_json_utils_modular.py refcsv als-cde-modular-prototype codes.csv
python gen_vs_json_utils_modular.py add als-cde-modular-prototype treatment/medications new_concept.json
python -m unittest test_modular_utils.py -v
```

## Compatibility and caveats

- Modular `ConceptRegistry.load()` expands `valueSetRef`, so existing downstream code can receive legacy-style `compose` definitions.
- Original legacy JSON arrays can still be read for querying or exporting; **writing** is deliberately limited to the modular registry.
- Source `name`, `description`, `relatedArtifact`, code lists, and metadata are preserved. Duplicate original names remain for review. The generated registry records have unique semantic `id` values.
- Code-system alias normalization is supported (`snomed`/`snomedct`, `hpc`/`hcpcs`, `cpt4`/`cpt`). Aliases are query normalization, not ontology equivalence validation.
- SQL queries are not assumed to prove clinical evidence (e.g. an NDC code in orders does not establish medication ingestion).
- Does not automatically fetch terminology versions, infer FHIR or CDISC mappings, or validate clinical coding accuracy.
