#!/usr/bin/env python3
"""Generate Cloudy.Data log modules from vendor/cloudy-openapi.json."""

from __future__ import annotations

import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
VENDOR = ROOT / "vendor" / "cloudy-openapi.json"
OUT_LOG = ROOT / "src" / "Cloudy" / "Data" / "Log"
OUT_REPORT = ROOT / "src" / "Cloudy" / "Data" / "Report"

# One Elm module per schema; helicopter shares plane_dgac.
LOG_MODULES: dict[str, str] = {
    "plane_dgac": "PlaneDgac",
    "helicopter_dgac": "PlaneDgac",
    "glider_dgac": "GliderDgac",
    "plane_fcl1080": "PlaneFcl1080",
    "ulm_ffplum": "UlmFfplum",
    "balloon_dgac": "BalloonDgac",
    "parachute_cloudy": "ParachuteCloudy",
}

READONLY = frozenset(
    {
        "id",
        "logbook_id",
        "created_at",
        "updated_at",
        "visible",
        "deleted",
        "files",
    }
)

PATCH_TIME_STRINGS = frozenset({"departure_time", "arrival_time"})


def cat_to_module(cat: str) -> str:
    return LOG_MODULES[cat]


def elm_type(prop: dict) -> str:
    t = prop.get("type")
    if t == "integer":
        return "Int"
    if t == "boolean":
        return "Bool"
    if t == "array":
        items = prop.get("items", {})
        if items.get("type") == "string":
            return "List String"
        return "List String"
    if prop.get("format") == "date-time":
        return "String"
    return "String"


def to_camel(s: str) -> str:
    return s


def module_header(module_name: str) -> str:
    return f"""module Cloudy.Data.Log.{module_name} exposing (Log, Patch, decoder, encodeCreate, encodeReplace, encodePatch)

{{-| Generated from Cloudy OpenAPI. Do not edit by hand; run scripts/generate_elm.py. |-}}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Cloudy.Data.AttachedFile as AttachedFile
"""


def report_module_header(module_name: str) -> str:
    return f"""module Cloudy.Data.Report.{module_name} exposing (Report, decoder, encodeReplace)

{{-| Generated from Cloudy OpenAPI. Do not edit by hand; run scripts/generate_elm.py. |-}}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
"""


def required_decoder(field: str, prop: dict, prefix: str = "") -> str:
    f = field
    t = elm_type(prop)
    if field == "files":
        return f'|> required "{f}" (Decode.list AttachedFile.decoder)'
    if t == "List String":
        return f'|> required "{f}" (Decode.list Decode.string)'
    if t == "Int":
        return f'|> required "{f}" Decode.int'
    if t == "Bool":
        return f'|> required "{f}" Decode.bool'
    return f'|> required "{f}" Decode.string'


def optional_decoder(field: str, prop: dict) -> str:
    t = elm_type(prop)
    inner = "Decode.string"
    if t == "Int":
        inner = "Decode.int"
    elif t == "Bool":
        inner = "Decode.bool"
    elif t == "List String":
        inner = "Decode.list Decode.string"
    return f'|> optional "{field}" {inner} Nothing'


def generate_log_module(module_name: str, read_props: dict, write_props: dict) -> str:
    read_fields = list(read_props.items())
    if "files" not in read_props:
        read_fields.append(("files", {"type": "array"}))

    lines = [module_header(module_name), "", "type alias Log =", "    {"]
    for i, (name, prop) in enumerate(read_fields):
        comma = "," if i < len(read_fields) - 1 else ""
        t = "List AttachedFile.AttachedFile" if name == "files" else elm_type(prop)
        lines.append(f"        {name} : {t}{comma}")
    lines.append("    }")
    lines.append("")

    writable = [(n, p) for n, p in write_props.items() if n not in READONLY]
    lines.append("type alias Patch =")
    lines.append("    {")
    for i, (name, prop) in enumerate(writable):
        comma = "," if i < len(writable) - 1 else ""
        lines.append(f"        {name} : Maybe ({elm_type(prop)}){comma}")
    lines.append("    }")
    lines.append("")

    lines.append("decoder : Decoder Log")
    lines.append("decoder =")
    lines.append("    Decode.succeed Log")
    for name, prop in read_fields:
        if name == "files":
            lines.append('    |> optional "files" (Decode.list AttachedFile.decoder) []')
        else:
            line = required_decoder(name, prop)
            lines.append("    " + (line if line.startswith("|") else "|> " + line))
    lines.append("")

    lines.extend(
        [
            "required : String -> Decoder a -> Decoder (a -> b) -> Decoder b",
            "required field dec builder =",
            "    Decode.map2 (\\v f -> f v) (Decode.field field dec) builder",
            "",
            "optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b",
            "optional field dec default builder =",
            "    Decode.map2",
            "        (\\mf f ->",
            "            case mf of",
            "                Just v -> f v",
            "                Nothing -> f default",
            "        )",
            "        (Decode.maybe (Decode.field field dec))",
            "        builder",
            "",
        ]
    )

    def encode_fields(fields: list[tuple[str, dict]], use_maybe: bool = False) -> str:
        enc = []
        for name, prop in fields:
            t = elm_type(prop)
            if use_maybe:
                enc.append(
                    f'        maybeField "{name}" ({encode_value(name, prop, "v")}) record.{name}'
                )
            else:
                enc.append(f'        ( "{name}", {encode_value(name, prop, f"record.{name}")} )')
        return "\n".join(enc)

    def encode_value(name: str, prop: dict, var: str) -> str:
        t = elm_type(prop)
        if t == "Int":
            return f"Encode.int {var}"
        if t == "Bool":
            return f"Encode.bool {var}"
        if t == "List String":
            return f"Encode.list Encode.string {var}"
        return f"Encode.string {var}"

    lines.append("encodeCreate : Log -> Value")
    lines.append("encodeCreate record =")
    lines.append("    encodeReplace record")
    lines.append("")
    lines.append("encodeReplace : Log -> Value")
    lines.append("encodeReplace record =")
    lines.append("    Encode.object")
    lines.append("        [")
    enc_lines = []
    for name, prop in writable:
        enc_lines.append(f'        ( "{name}", {encode_value(name, prop, f"record.{name}")} )')
    lines.append(",\n".join(enc_lines))
    lines.append("        ]")
    lines.append("")
    lines.append("encodePatch : Patch -> Value")
    lines.append("encodePatch record =")
    lines.append("    Encode.object")
    lines.append("        (List.filterMap identity")
    lines.append("            [")
    patch_lines = []
    for name, prop in writable:
        ev = encode_value(name, prop, "v")
        patch_lines.append(f'            maybeMap record.{name} (\\v -> ( "{name}", {ev} ))')
    lines.append(",\n".join(patch_lines))
    lines.append("            ]")
    lines.append("        )")
    lines.append("")
    lines.append("maybeMap : Maybe a -> (a -> b) -> Maybe b")
    lines.append("maybeMap m f =")
    lines.append("    case m of")
    lines.append("        Just v -> Just (f v)")
    lines.append("        Nothing -> Nothing")
    lines.append("")
    return "\n".join(lines) + "\n"


def generate_report_module(module_name: str, props: dict) -> str:
    lines = [report_module_header(module_name), "", "type alias Report =", "    {"]
    prop_items = list(props.items())
    for i, (name, prop) in enumerate(prop_items):
        comma = "," if i < len(prop_items) - 1 else ""
        lines.append(f"        {name} : {elm_type(prop)}{comma}")
    lines.append("    }")
    lines.append("")
    lines.append("decoder : Decoder Report")
    lines.append("decoder =")
    lines.append("    Decode.succeed Report")
    for name, prop in props.items():
        lines.append(f'    |> required "{name}" {inner_decoder(prop)}')
    lines.extend(
        [
            "",
            "required : String -> Decoder a -> Decoder (a -> b) -> Decoder b",
            "required field dec builder =",
            "    Decode.map2 (\\v f -> f v) (Decode.field field dec) builder",
            "",
            "encodeReplace : Report -> Value",
            "encodeReplace record =",
            "    Encode.object",
            "        [",
        ]
    )
    report_enc = []
    for name, prop in props.items():
        t = elm_type(prop)
        if t == "Int":
            report_enc.append(f'        ( "{name}", Encode.int record.{name} )')
        else:
            report_enc.append(f'        ( "{name}", Encode.string record.{name} )')
    lines.append(",\n".join(report_enc))
    lines.append("        ]")
    lines.append("")
    return "\n".join(lines) + "\n"


def inner_decoder(prop: dict) -> str:
    t = elm_type(prop)
    if t == "Int":
        return "Decode.int"
    if t == "Bool":
        return "Decode.bool"
    return "Decode.string"


def main() -> None:
    data = json.loads(VENDOR.read_text())
    schemas = data["components"]["schemas"]
    seen: set[str] = set()
    OUT_LOG.mkdir(parents=True, exist_ok=True)
    OUT_REPORT.mkdir(parents=True, exist_ok=True)

    for cat, module in LOG_MODULES.items():
        if module in seen:
            continue
        read_key = f"LogRead_{cat}"
        write_key = f"LogWrite_{cat}"
        if read_key not in schemas or write_key not in schemas:
            continue
        seen.add(module)
        read_props = schemas[read_key]["properties"]
        write_props = schemas[write_key]["properties"]
        path = OUT_LOG / f"{module}.elm"
        path.write_text(generate_log_module(module, read_props, write_props))

    for cat, module in LOG_MODULES.items():
        report_key = f"Report_{cat}"
        if report_key not in schemas:
            continue
        props = schemas[report_key]["properties"]
        if not props:
            continue
        path = OUT_REPORT / f"{module}.elm"
        path.write_text(generate_report_module(module, props))

    print(f"Generated log modules: {sorted(seen)}")


if __name__ == "__main__":
    main()
