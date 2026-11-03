#!/usr/bin/env python3
"""Compile YAML-based clinical-data mappings into SQL.

Designed for mapping specifications with source/bridge/target entities,
preserve_columns, transform_columns, reconcile rules, target metadata,
value-set maps, and placeholder expressions such as {s0}, {b0}, and {t0}.

Supported output modes:
  select, create_table, create_view, insert, merge, all

Supported dialects:
  ansi, snowflake, postgres, bigquery, sqlserver, spark, duckdb

The compiler is intentionally metadata-driven. Physical object names and join
predicates are supplied through an optional environment YAML file so the same
mapping can generate SQL for different deployments and SQL engines.
"""
from __future__ import annotations

import argparse
import csv
import json
import logging
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Iterable, Mapping, Sequence

import yaml

LOG = logging.getLogger("cdisc_sql_compiler")


class SpecError(ValueError):
    """Raised when the transformation specification is invalid or ambiguous."""


@dataclass(frozen=True)
class EntityRef:
    model: str | None
    entity: str
    alias: str

    @classmethod
    def from_dict(cls, raw: Mapping[str, Any], default_alias: str) -> "EntityRef":
        entity = str(raw.get("entity") or "").strip()
        if not entity:
            raise SpecError(f"Entity reference is missing 'entity': {raw}")
        return cls(
            model=str(raw.get("model")) if raw.get("model") is not None else None,
            entity=entity,
            alias=str(raw.get("alias") or default_alias),
        )


@dataclass
class Environment:
    objects: dict[str, str] = field(default_factory=dict)
    joins: dict[str, list[str]] = field(default_factory=dict)
    filters: dict[str, str] = field(default_factory=dict)
    value_sets: dict[str, str] = field(default_factory=dict)
    parameters: dict[str, Any] = field(default_factory=dict)

    @classmethod
    def load(cls, path: Path | None) -> "Environment":
        if path is None:
            return cls()
        with path.open("r", encoding="utf-8") as fh:
            raw = yaml.safe_load(fh) or {}
        return cls(
            objects=dict(raw.get("objects") or {}),
            joins={k: list(v or []) for k, v in (raw.get("joins") or {}).items()},
            filters=dict(raw.get("filters") or {}),
            value_sets=dict(raw.get("value_sets") or {}),
            parameters=dict(raw.get("parameters") or {}),
        )


class Dialect:
    """Small SQL-dialect adapter without requiring a SQL parser dependency."""

    def __init__(self, name: str):
        name = name.lower()
        allowed = {"ansi", "snowflake", "postgres", "bigquery", "sqlserver", "spark", "duckdb"}
        if name not in allowed:
            raise SpecError(f"Unsupported dialect {name!r}; choose from {sorted(allowed)}")
        self.name = name

    def quote(self, identifier: str) -> str:
        if identifier == "*":
            return identifier
        if self.name == "sqlserver":
            return f"[{identifier.replace(']', ']]')}]"
        if self.name == "bigquery":
            return f"`{identifier.replace('`', '``')}`"
        return f'"{identifier.replace(chr(34), chr(34) * 2)}"'

    def create_table(self, target: str, select_sql: str, replace: bool = True) -> str:
        if self.name in {"snowflake", "bigquery", "spark", "duckdb"}:
            verb = "CREATE OR REPLACE TABLE" if replace else "CREATE TABLE"
            return f"{verb} {target} AS\n{select_sql};"
        if self.name == "postgres":
            if replace:
                return f"DROP TABLE IF EXISTS {target};\nCREATE TABLE {target} AS\n{select_sql};"
            return f"CREATE TABLE {target} AS\n{select_sql};"
        if self.name == "sqlserver":
            if replace:
                return f"IF OBJECT_ID(N'{target}', N'U') IS NOT NULL DROP TABLE {target};\n{self._select_into(target, select_sql)}"
            return self._select_into(target, select_sql)
        return f"CREATE TABLE {target} AS\n{select_sql};"

    def _select_into(self, target: str, select_sql: str) -> str:
        return re.sub(r"^SELECT\s+", f"SELECT\n  ", select_sql, count=1, flags=re.I).replace("\nFROM ", f"\nINTO {target}\nFROM ", 1) + ";"

    def create_view(self, target: str, select_sql: str, replace: bool = True) -> str:
        if self.name == "sqlserver":
            return f"CREATE OR ALTER VIEW {target} AS\n{select_sql};" if replace else f"CREATE VIEW {target} AS\n{select_sql};"
        verb = "CREATE OR REPLACE VIEW" if replace else "CREATE VIEW"
        return f"{verb} {target} AS\n{select_sql};"

    def insert(self, target: str, columns: Sequence[str], select_sql: str) -> str:
        cols = ", ".join(self.quote(c) for c in columns)
        return f"INSERT INTO {target} ({cols})\n{select_sql};"

    def merge(self, target: str, select_sql: str, keys: Sequence[str], columns: Sequence[str]) -> str:
        if not keys:
            raise SpecError("MERGE output requires target_metadata.keys")
        if self.name == "postgres":
            nonkeys = [c for c in columns if c not in keys]
            cols = ", ".join(self.quote(c) for c in columns)
            updates = ",\n    ".join(f"{self.quote(c)} = EXCLUDED.{self.quote(c)}" for c in nonkeys)
            conflicts = ", ".join(self.quote(k) for k in keys)
            return (
                f"INSERT INTO {target} ({cols})\n{select_sql}\n"
                f"ON CONFLICT ({conflicts}) DO UPDATE SET\n    {updates};"
            )
        on = " AND ".join(f"t.{self.quote(k)} = s.{self.quote(k)}" for k in keys)
        nonkeys = [c for c in columns if c not in keys]
        updates = ",\n    ".join(f"t.{self.quote(c)} = s.{self.quote(c)}" for c in nonkeys)
        insert_cols = ", ".join(self.quote(c) for c in columns)
        insert_vals = ", ".join(f"s.{self.quote(c)}" for c in columns)
        return (
            f"MERGE INTO {target} AS t\nUSING (\n{indent(select_sql, 2)}\n) AS s\nON {on}\n"
            f"WHEN MATCHED THEN UPDATE SET\n    {updates}\n"
            f"WHEN NOT MATCHED THEN INSERT ({insert_cols})\nVALUES ({insert_vals});"
        )


def indent(text: str, spaces: int) -> str:
    pad = " " * spaces
    return "\n".join(pad + line if line else line for line in text.splitlines())


def split_top_level_csv(expression: str) -> list[str]:
    """Split comma-separated SQL expressions while respecting nesting and quotes."""
    out: list[str] = []
    buf: list[str] = []
    depth = 0
    quote: str | None = None
    i = 0
    while i < len(expression):
        ch = expression[i]
        if quote:
            buf.append(ch)
            if ch == quote:
                if i + 1 < len(expression) and expression[i + 1] == quote:
                    buf.append(expression[i + 1])
                    i += 1
                else:
                    quote = None
        else:
            if ch in {"'", '"', '`'}:
                quote = ch
                buf.append(ch)
            elif ch == "(":
                depth += 1
                buf.append(ch)
            elif ch == ")":
                depth = max(0, depth - 1)
                buf.append(ch)
            elif ch == "," and depth == 0:
                out.append("".join(buf).strip())
                buf = []
            else:
                buf.append(ch)
        i += 1
    if buf or expression.strip():
        out.append("".join(buf).strip())
    return out


def normalize_expression(expr: str) -> str:
    # Repair a common accidental placeholder typo: {s0}} -> {s0}
    return re.sub(r"(\{[sbt]\d+\})\}", r"\1", str(expr).strip())


def substitute_placeholders(expr: str, rule: Mapping[str, Any]) -> str:
    expr = normalize_expression(expr)
    source_cols = [f"{x['entity']}.{x['column']}" for x in (rule.get("sources") or [])]
    bridge_cols = [f"{x['entity']}.{x['column']}" for x in (rule.get("bridges") or rule.get("bridge") or [])]
    target_cols = [f"{x['entity']}.{x['column']}" for x in (rule.get("targets") or [])]
    replacements: dict[str, str] = {}
    replacements.update({f"s{i}": col for i, col in enumerate(source_cols)})
    replacements.update({f"b{i}": col for i, col in enumerate(bridge_cols)})
    replacements.update({f"t{i}": col for i, col in enumerate(target_cols)})

    def repl(match: re.Match[str]) -> str:
        key = match.group(1)
        if key not in replacements:
            raise SpecError(f"Unresolved placeholder {{{key}}} in rule {rule.get('rule_id')}: {expr}")
        return replacements[key]

    return re.sub(r"\{([sbt]\d+)\}", repl, expr)


def sql_literal(value: Any) -> str:
    if value is None:
        return "NULL"
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, (int, float)):
        return str(value)
    return "'" + str(value).replace("'", "''") + "'"


class Compiler:
    def __init__(self, spec: Mapping[str, Any], env: Environment, dialect: Dialect, strict: bool = True):
        self.spec = spec
        self.env = env
        self.dialect = dialect
        self.strict = strict
        self.study_params = dict((spec.get("specification") or {}).get("study_parameters") or {})
        self.study_params.update(env.parameters)

    def domain_maps(self) -> list[Mapping[str, Any]]:
        maps = self.spec.get("domain_maps") or []
        if not isinstance(maps, list):
            raise SpecError("domain_maps must be a list")
        return maps

    def find_map(self, selector: str) -> Mapping[str, Any]:
        selector_u = selector.upper()
        matches = []
        for mapping in self.domain_maps():
            rule_id = str(mapping.get("rule_id", ""))
            targets = [str(t.get("entity", "")) for t in mapping.get("target") or []]
            if rule_id.upper() == selector_u or selector_u in {x.upper() for x in targets}:
                matches.append(mapping)
        if len(matches) != 1:
            raise SpecError(f"Expected one domain map for {selector!r}, found {len(matches)}")
        return matches[0]

    def object_name(self, ref: EntityRef) -> str:
        candidates = [
            f"{ref.model}.{ref.entity}" if ref.model else None,
            ref.entity,
        ]
        for key in candidates:
            if key and key in self.env.objects:
                return self.env.objects[key]
        return f"{ref.model}.{ref.entity}" if ref.model else ref.entity

    def value_set_relation(self, descriptor: Mapping[str, Any]) -> tuple[str, str]:
        file_name = str(descriptor.get("file") or "")
        alias = str(descriptor.get("alias") or Path(file_name).stem.lower() or "vsm")
        relation = self.env.value_sets.get(file_name) or self.env.value_sets.get(Path(file_name).stem)
        if relation:
            return relation, alias
        # A CSV cannot be used as a SQL relation without loading/external-table setup.
        return f"/* LOAD_VALUESET:{file_name} */ {Path(file_name).stem}", alias

    def _refs(self, mapping: Mapping[str, Any], key: str) -> list[EntityRef]:
        return [EntityRef.from_dict(x, f"{key[:1]}{i}") for i, x in enumerate(mapping.get(key) or [])]

    def _all_refs(self, mapping: Mapping[str, Any]) -> list[EntityRef]:
        return self._refs(mapping, "source") + self._refs(mapping, "bridge")

    def _projection_map(self, mapping: Mapping[str, Any]) -> dict[str, str]:
        projection: dict[str, str] = {}
        rules = list(mapping.get("preserve_columns") or []) + list(mapping.get("transform_columns") or [])
        for rule in rules:
            targets = rule.get("targets") or []
            if not targets:
                continue
            target_names = [str(t["column"]) for t in targets]
            transform = rule.get("transform")
            valueset = rule.get("valueset_map")
            if transform is not None:
                expressions = split_top_level_csv(substitute_placeholders(str(transform), rule))
                if len(expressions) != len(target_names):
                    raise SpecError(
                        f"Rule {rule.get('rule_id')} has {len(target_names)} targets but "
                        f"its transform yields {len(expressions)} top-level expressions"
                    )
                projection.update(dict(zip(target_names, expressions)))
            elif valueset:
                alias = str(valueset.get("alias") or "vsm")
                # Prefer same-name target column in value-set relation. This is explicit and overridable.
                for target in target_names:
                    projection[target] = f"{alias}.{target}"
            else:
                sources = rule.get("sources") or []
                bridges = rule.get("bridges") or rule.get("bridge") or []
                # A bridge value is the reconciled/staged value and therefore takes
                # precedence over the raw source when both are declared.
                inputs = bridges if bridges else sources
                if len(inputs) == len(target_names):
                    for target, inp in zip(target_names, inputs):
                        projection[target] = f"{inp['entity']}.{inp['column']}"
                elif len(inputs) == 1 and len(target_names) == 1:
                    inp = inputs[0]
                    projection[target_names[0]] = f"{inp['entity']}.{inp['column']}"
                else:
                    raise SpecError(
                        f"Rule {rule.get('rule_id')} requires transform because input/target cardinalities differ"
                    )
        return projection

    def _metadata_columns(self, mapping: Mapping[str, Any]) -> list[str]:
        variables = ((mapping.get("target_metadata") or {}).get("variables") or [])
        return [str(v["name"]) for v in variables]

    def _value_set_joins(self, mapping: Mapping[str, Any]) -> list[tuple[str, str, str]]:
        """Return (relation, alias, ON expression) tuples."""
        joins: list[tuple[str, str, str]] = []
        for rule in list(mapping.get("preserve_columns") or []) + list(mapping.get("transform_columns") or []):
            descriptor = rule.get("valueset_map")
            if not descriptor:
                continue
            relation, alias = self.value_set_relation(descriptor)
            explicit = descriptor.get("join_on") or rule.get("valueset_join_on")
            if explicit:
                on = substitute_placeholders(str(explicit), rule)
            else:
                srcs = rule.get("sources") or []
                if not srcs:
                    on = "1 = 1 /* TODO: define value-set join */"
                else:
                    # Default mapping-table convention: source columns have identical names.
                    on = " AND ".join(
                        f"{src['entity']}.{src['column']} = {alias}.{src['column']}" for src in srcs
                    )
            joins.append((relation, alias, on))
        # preserve order, eliminate duplicates
        seen: set[tuple[str, str, str]] = set()
        return [j for j in joins if not (j in seen or seen.add(j))]

    def _from_clause(self, mapping: Mapping[str, Any]) -> str:
        refs = self._all_refs(mapping)
        if not refs:
            # Protocol-only domains TI/TS may be generated without a physical source.
            base = "(SELECT 1 AS seed) AS seed"
        else:
            first, *rest = refs
            base = f"{self.object_name(first)} AS {first.alias}"
            configured = list(self.env.joins.get(str(mapping.get("rule_id"))) or [])
            for idx, ref in enumerate(rest):
                if idx < len(configured):
                    predicate = configured[idx]
                elif self.strict:
                    raise SpecError(
                        f"Missing join predicate #{idx + 1} for {mapping.get('rule_id')}. "
                        f"Add it under joins.{mapping.get('rule_id')} in the environment YAML."
                    )
                else:
                    predicate = "1 = 1 /* TODO: configure join predicate */"
                base += f"\nLEFT JOIN {self.object_name(ref)} AS {ref.alias}\n  ON {predicate}"
        for relation, alias, predicate in self._value_set_joins(mapping):
            base += f"\nLEFT JOIN {relation} AS {alias}\n  ON {predicate}"
        return base

    def compile_select(self, mapping: Mapping[str, Any]) -> tuple[str, list[str]]:
        projection = self._projection_map(mapping)
        metadata_cols = self._metadata_columns(mapping)
        if not metadata_cols:
            metadata_cols = list(projection)
        expressions: list[str] = []
        missing: list[str] = []
        for col in metadata_cols:
            expr = projection.get(col)
            if expr is None:
                missing.append(col)
                expr = f"NULL /* UNMAPPED:{col} */"
            expressions.append(f"{expr} AS {self.dialect.quote(col)}")
        if self.strict and missing:
            required = {
                str(v["name"])
                for v in ((mapping.get("target_metadata") or {}).get("variables") or [])
                if str(v.get("core", "")).lower() == "req"
            }
            missing_required = sorted(required.intersection(missing))
            if missing_required:
                raise SpecError(
                    f"{mapping.get('rule_id')} has unmapped Required target variables: {missing_required}"
                )
        where = self.env.filters.get(str(mapping.get("rule_id"))) or mapping.get("where")
        distinct = bool(mapping.get("distinct"))
        sql = "SELECT " + ("DISTINCT\n" if distinct else "\n")
        sql += ",\n".join("  " + x for x in expressions)
        sql += "\nFROM " + self._from_clause(mapping)
        if where:
            sql += f"\nWHERE {where}"
        return sql, metadata_cols

    def target_name(self, mapping: Mapping[str, Any]) -> str:
        targets = mapping.get("target") or []
        if len(targets) != 1:
            raise SpecError(f"Expected exactly one target in {mapping.get('rule_id')}")
        ref = EntityRef.from_dict(targets[0], "tgt")
        return self.object_name(ref)

    def compile(self, mapping: Mapping[str, Any], mode: str, replace: bool = True) -> str:
        select_sql, columns = self.compile_select(mapping)
        target = self.target_name(mapping)
        if mode == "select":
            return select_sql + ";"
        if mode == "create_table":
            return self.dialect.create_table(target, select_sql, replace)
        if mode == "create_view":
            return self.dialect.create_view(target, select_sql, replace)
        if mode == "insert":
            return self.dialect.insert(target, columns, select_sql)
        if mode == "merge":
            keys = list((mapping.get("target_metadata") or {}).get("keys") or [])
            return self.dialect.merge(target, select_sql, keys, columns)
        raise SpecError(f"Unsupported mode: {mode}")

    def compile_validation_sql(self, mapping: Mapping[str, Any]) -> str:
        target = self.target_name(mapping)
        meta = mapping.get("target_metadata") or {}
        variables = meta.get("variables") or []
        required = [str(v["name"]) for v in variables if str(v.get("core", "")).lower() == "req"]
        keys = list(meta.get("keys") or [])
        checks: list[str] = []
        for col in required:
            checks.append(
                f"SELECT '{mapping.get('rule_id')}' AS map_id, 'REQUIRED_NULL:{col}' AS check_id, "
                f"COUNT(*) AS violation_count FROM {target} WHERE {self.dialect.quote(col)} IS NULL"
            )
        if keys:
            key_expr = ", ".join(self.dialect.quote(k) for k in keys)
            checks.append(
                f"SELECT '{mapping.get('rule_id')}' AS map_id, 'DUPLICATE_KEY' AS check_id, "
                f"COUNT(*) AS violation_count FROM (SELECT {key_expr} FROM {target} "
                f"GROUP BY {key_expr} HAVING COUNT(*) > 1) q"
            )
        return "\nUNION ALL\n".join(checks) + (";" if checks else "")


def load_spec(path: Path) -> Mapping[str, Any]:
    with path.open("r", encoding="utf-8") as fh:
        raw = yaml.safe_load(fh)
    if not isinstance(raw, Mapping):
        raise SpecError("Top-level YAML value must be a mapping")
    return raw


def write_outputs(
    compiler: Compiler,
    maps: Sequence[Mapping[str, Any]],
    out_dir: Path,
    mode: str,
    replace: bool,
    validations: bool,
) -> list[Path]:
    out_dir.mkdir(parents=True, exist_ok=True)
    paths: list[Path] = []
    modes = ["select", "create_table", "create_view", "insert", "merge"] if mode == "all" else [mode]
    for mapping in maps:
        domain = str((mapping.get("target_metadata") or {}).get("domain") or (mapping.get("target") or [{}])[0].get("entity"))
        for actual_mode in modes:
            try:
                sql = compiler.compile(mapping, actual_mode, replace)
            except SpecError as exc:
                if actual_mode == "merge" and mode == "all":
                    LOG.warning("Skipping MERGE for %s: %s", domain, exc)
                    continue
                raise
            path = out_dir / f"{domain.lower()}__{actual_mode}.sql"
            path.write_text(
                f"-- Generated from {mapping.get('rule_id')}\n"
                f"-- Dialect: {compiler.dialect.name}\n\n{sql}\n",
                encoding="utf-8",
            )
            paths.append(path)
        if validations:
            path = out_dir / f"{domain.lower()}__validate.sql"
            path.write_text(compiler.compile_validation_sql(mapping) + "\n", encoding="utf-8")
            paths.append(path)
    return paths


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("spec", type=Path, help="Transformation YAML specification")
    p.add_argument("--environment", "-e", type=Path, help="Physical objects, joins, filters, and value-set relations")
    p.add_argument("--dialect", default="snowflake", choices=["ansi", "snowflake", "postgres", "bigquery", "sqlserver", "spark", "duckdb"])
    p.add_argument("--domain", action="append", help="Target domain or mapping rule_id; repeat to compile several")
    p.add_argument("--mode", default="create_table", choices=["select", "create_table", "create_view", "insert", "merge", "all"])
    p.add_argument("--out-dir", type=Path, default=Path("generated_sql"))
    p.add_argument("--no-replace", action="store_true", help="Do not use CREATE OR REPLACE where supported")
    p.add_argument("--non-strict", action="store_true", help="Emit TODO joins/NULL projections rather than fail")
    p.add_argument("--validations", action="store_true", help="Also generate basic Required/null and duplicate-key checks")
    p.add_argument("--list", action="store_true", help="List mappings and exit")
    p.add_argument("--verbose", action="store_true")
    return p


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO, format="%(levelname)s: %(message)s")
    try:
        spec = load_spec(args.spec)
        env = Environment.load(args.environment)
        compiler = Compiler(spec, env, Dialect(args.dialect), strict=not args.non_strict)
        if args.list:
            for mapping in compiler.domain_maps():
                domain = (mapping.get("target_metadata") or {}).get("domain")
                print(f"{domain:>3}  {mapping.get('rule_id')}")
            return 0
        maps = [compiler.find_map(x) for x in args.domain] if args.domain else compiler.domain_maps()
        paths = write_outputs(
            compiler,
            maps,
            args.out_dir,
            args.mode,
            replace=not args.no_replace,
            validations=args.validations,
        )
        print(f"Generated {len(paths)} SQL file(s) in {args.out_dir}")
        return 0
    except (OSError, yaml.YAMLError, SpecError) as exc:
        LOG.error("%s", exc)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
