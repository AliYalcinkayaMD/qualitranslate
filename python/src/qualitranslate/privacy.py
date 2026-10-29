"""Local check for common identifier patterns before text is sent anywhere.

It only warns. It never removes or changes text, and it cannot find names, places or
other indirect identifiers, so it does not replace pseudonymisation by the research team.
"""
from __future__ import annotations

import re

KINDS = {
    "email": ("e-mail address", "e-mail addresses"),
    "url": ("web address", "web addresses"),
    "cpr": ("CPR-like number (DDMMYY-XXXX)", "CPR-like numbers (DDMMYY-XXXX)"),
    "phone": ("phone-like number", "phone-like numbers"),
    "date": ("full date (could be a birth date)", "full dates (could be birth dates)"),
}


def _phrase(kind, n):
    return str(n) + " " + KINDS[kind][0 if n == 1 else 1]

_EMAIL = re.compile(r"[\w.+-]+@[\w-]+(?:\.[\w-]+)+")
_URL = re.compile(r"\b(?:https?://|www\.)[^\s<>\"']+", re.I)
_CPR = re.compile(r"(?<![\d:])([0-3]\d)([01]\d)(\d{2})[- ]?(\d{4})(?![\d:])")
_PHONE = re.compile(r"(?<![\w:.])(?:\+\d{1,3}[ -]?)?\(?\d{2,4}\)?(?:[ -]?\d{2,4}){1,4}(?![\w:])")
_DATE = re.compile(r"(?<![\d:])(?:[0-3]?\d[./-][01]?\d[./-](?:19|20)\d{2}|(?:19|20)\d{2}-[01]\d-[0-3]\d)(?![\d:])")


def find_identifiers(text):
    """Return a list of (kind, start, end) matches in one piece of text."""
    found, taken = [], []

    def free(a, b):
        return all(b <= x or a >= y for x, y in taken)

    def add(kind, m):
        if free(m.start(), m.end()):
            found.append((kind, m.start(), m.end()))
            taken.append((m.start(), m.end()))

    for m in _EMAIL.finditer(text):
        add("email", m)
    for m in _URL.finditer(text):
        add("url", m)
    for m in _CPR.finditer(text):
        day, month = int(m.group(1)), int(m.group(2))
        if 1 <= day <= 31 and 1 <= month <= 12:
            add("cpr", m)
    for m in _DATE.finditer(text):
        add("date", m)
    for m in _PHONE.finditer(text):
        digits = sum(c.isdigit() for c in m.group(0))
        if 8 <= digits <= 15:
            add("phone", m)
    return sorted(found, key=lambda x: x[1])


def identifier_scan(segments):
    """Summarise possible identifiers in the source sections of a project."""
    counts, rows = {}, []
    for seg in segments:
        hits = find_identifiers(seg.get("source", ""))
        kinds = sorted({k for k, _, _ in hits})
        for k, _, _ in hits:
            counts[k] = counts.get(k, 0) + 1
        if kinds:
            rows.append({"id": seg["id"], "kinds": kinds})
    return {"total": sum(counts.values()), "counts": counts, "segments": rows,
            "labels": {k: KINDS[k][0] for k in counts}, "phrases": [_phrase(k, n) for k, n in sorted(counts.items())]}


def identifier_summary(scan):
    if not scan["total"]:
        return "none of the checked patterns found"
    return ", ".join(scan["phrases"]) + " in " + str(len(scan["segments"])) + " section(s): " + ", ".join(r["id"] for r in scan["segments"][:40]) + ("…" if len(scan["segments"]) > 40 else "")
