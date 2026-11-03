"""ALS modular CDE registry utilities. Python >=3.9; pandas only for json2ref.

Supports the modular prototype (manifest.json, concepts/, value-sets/), and
legacy monolithic arrays for read-only query generation/migration.
No code system/value-set clinical mappings are changed by this module.
"""
from __future__ import annotations

import json
import re
import urllib.request
from pathlib import Path
from collections import Counter

SYSTEM_ALIASES = {'snomed':'snomedct','cpt4':'cpt','hpc':'hcpcs','npiTaxonomy':'nucc'}
DOMAIN_SYSTEMS = {
    'dx': ['icd9cm','icd10cm','snomedct','drg'],
    'px': ['icd9proc','icd10pcs','cpt','hcpcs'],
    'lab': ['loinc'],
    'rx': ['rxnorm','ndc','multum','atc'],
}

def _read_json(source):
    s = str(source)
    if s.startswith(('https://','http://')):
        with urllib.request.urlopen(s, timeout=30) as response:
            return json.load(response)
    return json.loads(Path(s).read_text(encoding='utf-8'))

def _write_json(path, obj):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name+'.tmp')
    tmp.write_text(json.dumps(obj, indent=2, ensure_ascii=False)+'\n', encoding='utf-8')
    tmp.replace(path)

def _safe_ref(root, ref):
    if not isinstance(ref, str) or not ref or '\\' in ref:
        raise ValueError('Invalid registry reference')
    relative = Path(ref)
    if relative.is_absolute() or '..' in relative.parts:
        raise ValueError(f'Unsafe registry reference: {ref}')
    full = (root / relative).resolve()
    if not full.is_relative_to(root.resolve()):
        raise ValueError(f'Reference escapes registry: {ref}')
    return full

def expand_range(expression):
    """Expand inclusive literal ranges while preserving numeric zero padding."""
    m = re.fullmatch(r'([A-Za-z]*)(\d+)-([A-Za-z]*)(\d+)', str(expression))
    if not m or m.group(1).upper() != m.group(3).upper():
        raise ValueError(f'Unsupported code range: {expression}')
    p, start, _, end = m.groups()
    a, b = int(start), int(end)
    if b < a or b-a > 50000:
        raise ValueError(f'Invalid or overly large code range: {expression}')
    return [p+str(n).zfill(len(start)) for n in range(a,b+1)]

class ConceptRegistry:
    """A filesystem-based registry; recursive discovery means no manual reordering."""
    def __init__(self, source):
        self.source = str(source)
        self.legacy = not Path(self.source).is_dir()
        self.root = Path(self.source).resolve() if not self.legacy else None
        if self.root and not (self.root/'concepts').is_dir():
            raise FileNotFoundError(f'No concepts/ directory at {self.root}')

    def load(self):
        if self.legacy:
            data = _read_json(self.source)
            if not isinstance(data, list):
                raise ValueError('Legacy input must be a JSON array')
            return data
        result = []
        for file in sorted((self.root/'concepts').rglob('*.json')):
            concept = _read_json(file)
            if not isinstance(concept, dict) or not concept.get('id'):
                raise ValueError(f'Invalid concept file {file}')
            concept = dict(concept)
            ref = concept.get('valueSetRef')
            if ref:
                vs = _read_json(_safe_ref(self.root, ref))
                if 'compose' in concept and concept['compose'] != vs.get('compose'):
                    raise ValueError(f'Ambiguous compose definitions for {concept["id"]}')
                concept['compose'] = vs['compose']
            vrref = concept.get('valueRangeRef') or concept.get('responseScaleRef')
            if vrref:
                scale = _read_json(_safe_ref(self.root, vrref))
                ra = dict(concept.get('relatedArtifact', {}))
                if 'valueRange' not in ra:
                    ra['valueRange'] = scale.get('valueRange', scale.get('values', {}))
                concept['relatedArtifact'] = ra
            result.append(concept)
        return result

    def validate(self):
        entries = self.load()
        ids = Counter(x.get('id') for x in entries)
        names = Counter(x.get('name') for x in entries)
        return {'count':len(entries),
                'duplicateIds':[k for k,v in ids.items() if v>1],
                'duplicateNames':[k for k,v in names.items() if v>1]}

    def add(self, concept, domain, *, externalize_code_threshold=20):
        if self.legacy: raise ValueError('Adding requires a modular directory')
        if not isinstance(concept,dict): raise TypeError('Concept must be a dictionary')
        cid = concept.get('id')
        if not isinstance(cid,str) or not re.fullmatch(r'[A-Z][A-Z0-9]*(?:-[A-Z0-9]+)*',cid):
            raise ValueError('ID must be a meaningful uppercase acronym, e.g. ALSFRSR-Q1-SPEECH')
        if not concept.get('name'): raise ValueError('Concept name is required')
        domain_parts = domain.split('/')
        if not domain_parts or any(not re.fullmatch(r'[a-z0-9]+(?:-[a-z0-9]+)*', p) for p in domain_parts):
            raise ValueError('Domain must be a safe folder path, e.g. treatment/medications')
        if cid in {x['id'] for x in self.load()}:
            raise ValueError(f'Concept ID already exists: {cid}')
        filename = cid.lower()+'.json'
        relative = Path('concepts').joinpath(*domain_parts,filename)
        destination = _safe_ref(self.root, relative.as_posix())
        if destination.exists(): raise FileExistsError(destination)
        entry = dict(concept)
        compose = entry.get('compose',{}).get('include',[])
        count = sum(len(s.get('concept',[]))+sum(len(f.get('value',[])) for f in s.get('filter',[])) for s in compose)
        if count >= externalize_code_threshold and compose:
            ref = Path('value-sets').joinpath(*domain_parts,filename).as_posix()
            if _safe_ref(self.root,ref).exists(): raise FileExistsError(ref)
            entry.pop('compose',None)
            entry['valueSetRef'] = ref
            _write_json(_safe_ref(self.root,ref), {'id':cid+'-VALUESET','compose':{'include':compose}})
        _write_json(destination,entry)
        self.rebuild_manifest()
        return destination

    def rebuild_manifest(self):
        if self.legacy: raise ValueError('Only modular registries have manifests')
        manifest_path = self.root/'manifest.json'
        previous = _read_json(manifest_path) if manifest_path.exists() else {}
        rows = []
        for f in sorted((self.root/'concepts').rglob('*.json')):
            obj = _read_json(f)
            rows.append({'id':obj['id'],'name':obj.get('name'),
                         'legacyId':obj.get('legacyId'),
                         'domain':f.parent.relative_to(self.root/'concepts').as_posix(),
                         'file':f.relative_to(self.root).as_posix(),
                         'valueSetRef':obj.get('valueSetRef'),
                         'needsReview':obj.get('needsReview',False)})
        previous['sourceCount'] = len(rows)
        previous['concepts'] = sorted(rows,key=lambda x:(x['domain'],x['id']))
        _write_json(manifest_path,previous)
        return manifest_path

    def export_legacy(self, output):
        """Generate a single expanded JSON array for old consumers."""
        _write_json(output,self.load())
        return Path(output)


def json2ref(json_url,save_csv_to):
    """Flatten literal codes and filter patterns to CSV (does not expand ontology hierarchy)."""
    import pandas as pd
    rows=[]
    for item in ConceptRegistry(json_url).load():
        for inc in item.get('compose',{}).get('include',[]):
            sys=SYSTEM_ALIASES.get(inc.get('system'),inc.get('system'))
            base = {'id':item['id'],'name':item.get('name',''),
                    'description':item.get('description',''),'codesystem':sys}
            for concept in inc.get('concept',[]):
                rows.append({**base,'code':str(concept['code']),'op':concept.get('op','exists')})
            for fil in inc.get('filter',[]):
                prop, op=fil['property'],fil['op']
                for code in fil.get('value',[]):
                    if prop=='codeRange' and op=='in':
                        rows.extend({**base,'code':v,'op':'exists'} for v in expand_range(code))
                    else:
                        rows.append({**base,'code':str(code),'op':op})
    pd.DataFrame(rows,columns=['id','name','description','codesystem','code','op']).to_csv(save_csv_to,index=False)
    return 'new valueset saved as ref csv'


def _sql_quote(x):
    return "'"+str(x).replace("'","''")+"'"

def _sql_codes(column,values):
    values=list(dict.fromkeys(str(v) for v in values))
    if not values:return None
    return f'{column} IN ('+', '.join(_sql_quote(v) for v in values)+')'

def _sql_prefix(column, code, sqltype):
    # A prefix match is intentionally anchored; alternatives should not match elsewhere.
    if sqltype=='sqlserver':
        return f'LEFT({column}, {len(code)}) = {_sql_quote(code)}'
    return f'SUBSTR({column}, 1, {len(code)}) = {_sql_quote(code)}' if sqltype in ('spark','oracle','mysql') else f'SUBSTRING({column}, 1, {len(code)}) = {_sql_quote(code)}'

class QueryFromJson:
    """Compatible query API; url can be a registry directory or a legacy JSON file."""
    def __init__(self,url,sqlty,cd_field,date_fields,other_fields,srctbl_name,
                 sel_keys=None,sel_domain='',cdtype_field='',val_field='',cdtype_map=None,
                 strict_filters=True):
        self.url=url; self.sqlty=sqlty.lower()
        if self.sqlty not in ('snow','postgres','spark','mysql','sqlserver','oracle'):
            raise ValueError('Unsupported SQL dialect')
        self.cd_field=cd_field;self.date_fields=list(date_fields)
        self.other_fields=list(other_fields);self.srctbl_name=srctbl_name
        self.sel_keys=list(sel_keys or []);self.sel_domain=sel_domain
        self.cdtype_field=cdtype_field;self.val_field=val_field
        self.cdtype_map={SYSTEM_ALIASES.get(k,k):str(v) for k,v in (cdtype_map or {}).items()}
        self.strict_filters=strict_filters
        if not self.date_fields:raise ValueError('At least one date field is needed')

    def gen_cdtype_encoder(self):
        if self.cdtype_map:return self.cdtype_map
        codes = DOMAIN_SYSTEMS.get(self.sel_domain)
        if codes is None:
            if self.sel_domain:raise ValueError(f'Unknown sel_domain {self.sel_domain}')
            codes=sorted(set(sum(DOMAIN_SYSTEMS.values(),[])))
        return {code:input(f'Enter Code Type Value for {code} (blank to skip): ') for code in codes if True}

    def _filter_predicates(self, inc):
        parts=[]
        codes=[c['code'] for c in inc.get('concept',[])]
        if codes: parts.append(_sql_codes(self.cd_field,codes))
        for f in inc.get('filter',[]):
            prop,op=f['property'],f['op']; vals=f.get('value',[])
            if prop=='codeList' and op=='exists':
                parts.append(_sql_codes(self.cd_field, vals))
            elif prop=='codeRange' and op=='in':
                expanded=[]
                for r in vals:expanded.extend(expand_range(r))
                parts.append(_sql_codes(self.cd_field,expanded))
            elif prop=='codePrecision' and op in ('descendent-of','child-of','descendent-leaf'):
                # Approximation: hierarchical prefix searches require terminology expansion
                # for true descendant semantics. Fail closed on unsupported hierarchy modes.
                if op!='descendent-of':
                    if self.strict_filters: raise ValueError(f'Hierarchy operator {op} requires terminology server expansion')
                    continue
                parts.extend(_sql_prefix(self.cd_field,str(v),self.sqlty) for v in vals)
            elif self.strict_filters:
                raise ValueError(f'Unsupported filter {prop}/{op}')
        return ' OR '.join('('+p+')' for p in parts if p)

    def _range_clause(self,item,value_range_side):
        if not self.val_field:return ''
        ra=item.get('relatedArtifact',{})
        if ra.get('valueType') not in ('continuous','decimal','integer','discrete'):
            return ''
        vr=ra.get('valueRange') or {}
        # Existing source conventions: 'low' compares value <= threshold;
        # 'high' compares value >= threshold. These are INTERPRETATION thresholds.
        side=value_range_side.lower()
        if side not in ('both','high','low'):
            raise ValueError('value_range_side must be both, low, or high')
        terms=[]
        for key,operator in [('low','<'),('high','>')]:
            if side not in ('both',key) or not isinstance(vr.get(key),dict):continue
            bound=vr[key]
            val=bound.get('value')
            if not isinstance(val,(float,int)):continue
            inclusive=bool(bound.get('incld',0))
            terms.append(f'{self.val_field} {operator+("=" if inclusive else "")} {val}')
        # If both opposite interpretation conditions are present, they are
        # alternatives rather than an AND interval (e.g. low risk OR high risk).
        return '('+' OR '.join(terms)+')' if terms else ''

    def gen_qry_ref(self,use_value_range=True,value_range_side='both'):
        mapped=self.gen_cdtype_encoder()
        out={}
        for item in ConceptRegistry(self.url).load():
            if self.sel_keys and item.get('id') not in self.sel_keys and item.get('name') not in self.sel_keys:
                continue
            alternatives=[]
            for inc in item.get('compose',{}).get('include',[]):
                system=SYSTEM_ALIASES.get(inc.get('system'),inc.get('system'))
                if system not in mapped or not mapped[system]:continue
                predicate=self._filter_predicates(inc)
                if not predicate:continue
                if self.cdtype_field:
                    predicate=f'({self.cdtype_field} = {_sql_quote(mapped[system])} AND ({predicate}))'
                else:
                    predicate=f'({predicate})'
                alternatives.append(predicate)
            if not alternatives:continue
            expr='('+' OR '.join(alternatives)+')'
            if use_value_range:
                threshold=self._range_clause(item,value_range_side)
                if threshold:expr=f'({expr} AND {threshold})'
            out[item['id']]=expr
        return out

    def gen_qry(self,use_value_range=True,value_range_side='both'):
        expressions=self.gen_qry_ref(use_value_range,value_range_side)
        dates=(self.date_fields[0] if len(self.date_fields)==1 else 'COALESCE('+', '.join(self.date_fields)+')')
        selected=[]
        for cid,predicate in expressions.items():
            fields=self.other_fields+[self.cd_field]
            if self.cdtype_field:fields.append(self.cdtype_field)
            selected.append('SELECT '+', '.join(fields)+f', {dates} AS CD_DATE, '+_sql_quote(cid)+
                            f' AS CD_GRP\nFROM {self.srctbl_name}\nWHERE {predicate}')
        return '\nUNION ALL\n'.join(selected)

class JsonBlockVS:
    """Create concepts in the modular repository; no positional insertion required."""
    def __init__(self,filepath,idstarter=None,idlength=None):
        self.registry=ConceptRegistry(filepath)
        if self.registry.legacy:raise ValueError('Use a modular registry directory for writing')

    def add_json_block(self,concept=None,domain=None):
        if concept is None:
            cid=input('Semantic ID (e.g. ALSFRSR-Q1-SPEECH): ').strip().upper()
            name=input('Concept name: ').strip()
            description=input('Description: ').strip()
            domain=domain or input('Domain path (e.g. functional-status/alsfrs-r): ').strip()
            concept={'id':cid,'name':name,'description':description,
                     'topic':input('Topic: ').strip(),
                     'purpose':input('Purpose: ').strip(),
                     'relatedArtifact':{'class':input('CDE class: ').strip(),
                         'valueType':input('Value type: ').strip(),'valueRange':{}},
                     'compose':{'include':[]}}
        elif not domain:
            raise ValueError('domain is required when providing a concept dict')
        path=self.registry.add(concept,domain)
        return f'Added {concept["id"]} to {path}'

if __name__=='__main__':
    import argparse
    ap=argparse.ArgumentParser(description='Modular ALS CDE registry tools')
    sub=ap.add_subparsers(dest='command',required=True)
    for op in ('validate','reindex'):
        p=sub.add_parser(op);p.add_argument('registry')
    p=sub.add_parser('export');p.add_argument('registry');p.add_argument('output')
    p=sub.add_parser('refcsv');p.add_argument('registry');p.add_argument('output')
    p=sub.add_parser('add');p.add_argument('registry');p.add_argument('domain');p.add_argument('concept_json')
    args=ap.parse_args()
    r=ConceptRegistry(args.registry)
    if args.command=='validate':print(json.dumps(r.validate(),indent=2))
    elif args.command=='reindex':print(r.rebuild_manifest())
    elif args.command=='export':print(r.export_legacy(args.output))
    elif args.command=='refcsv':print(json2ref(args.registry,args.output))
    elif args.command=='add':print(r.add(_read_json(args.concept_json),args.domain))
