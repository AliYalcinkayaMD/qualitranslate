"""Versioned prompts for translating qualitative interview data.

The text lives in prompts.json, which the R package ships unchanged, so both
implementations send identical instructions.
"""
import json
from pathlib import Path

_DATA = json.loads((Path(__file__).with_name("prompts.json")).read_text(encoding="utf-8"))

PROMPT_VERSION = _DATA["prompt_version"]
COMMON = _DATA["common"]
FORWARD = COMMON + _DATA["forward"]
BACK = COMMON + _DATA["back"]
RECONCILE = COMMON + _DATA["reconcile"]
CATEGORIES = tuple(_DATA["categories"])
REPLY_FORMAT = _DATA["reply_format"]

PROMPTS = {"forward": FORWARD, "back": BACK, "reconcile": RECONCILE}
