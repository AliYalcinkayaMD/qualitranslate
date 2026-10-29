#!/usr/bin/env python3
"""Copy the shared files (prompts, Copilot page adapter, web interface) into both packages.

shared/ is the single source. Run this after editing anything in shared/, then commit the
copies. `python3 tools/sync_shared.py --check` exits with status 1 if a copy differs
(used by the tests and CI).
"""
import filecmp
import shutil
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SHARED = ROOT / "shared"
TARGETS = {
    ROOT / "python" / "src" / "qualitranslate": {"prompts.json": "prompts.json",
                                                 "web/index.html": "web/index.html", "web/app.js": "web/app.js", "web/style.css": "web/style.css"},
    ROOT / "r" / "inst": {"prompts.json": "prompts.json",
                          "web/index.html": "www/index.html", "web/app.js": "www/app.js", "web/style.css": "www/style.css"},
}


def main(check=False):
    different = []
    for base, mapping in TARGETS.items():
        for source, target in mapping.items():
            src, dst = SHARED / source, base / target
            if dst.exists() and filecmp.cmp(src, dst, shallow=False):
                continue
            different.append(str(dst.relative_to(ROOT)))
            if not check:
                dst.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(src, dst)
    if check and different:
        print("Out of date (run python3 tools/sync_shared.py):\n  " + "\n  ".join(different))
        return 1
    print(("Updated: " + ", ".join(different)) if different else "All shared copies are up to date.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(check="--check" in sys.argv))
