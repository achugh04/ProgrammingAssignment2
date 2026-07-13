#!/usr/bin/env python3
"""
new_scene.py — scaffold a new video scene by copying an existing template.

    python .claude/skills/math-equation-video/scripts/new_scene.py <name> --from <template>

<name>      snake_case name for the new scene file (mathviz/scenes/<name>.py)
--from      template stem to copy (default: taylor_sine). Any file in
            mathviz/scenes/ without extension, e.g. fourier_square.

It copies the template, swaps in a fresh PascalCase class name, and prints the
line to add to render.sh's SCENES map. Then edit the math + TITLE/FORMULA.
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[4]
SCENES = REPO / "mathviz" / "scenes"


def pascal(name: str) -> str:
    return "".join(p.capitalize() for p in re.split(r"[_\-\s]+", name) if p)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("name", help="snake_case scene name")
    ap.add_argument("--from", dest="template", default="taylor_sine",
                    help="template stem to copy (default: taylor_sine)")
    args = ap.parse_args()

    name = re.sub(r"[^0-9a-zA-Z]+", "_", args.name).strip("_").lower()
    if not name:
        print("!! invalid name", file=sys.stderr)
        return 1

    src = SCENES / f"{args.template}.py"
    dst = SCENES / f"{name}.py"
    if not src.exists():
        avail = ", ".join(p.stem for p in SCENES.glob("*.py") if p.stem != "__init__")
        print(f"!! no template '{args.template}'. available: {avail}", file=sys.stderr)
        return 1
    if dst.exists():
        print(f"!! {dst} already exists — pick another name", file=sys.stderr)
        return 1

    text = src.read_text()

    # Find the template's Scene subclass name and replace with the new one.
    m = re.search(r"class\s+(\w+)\s*\(\s*ShortsScene\s*\)", text)
    new_class = pascal(name)
    if m:
        text = text.replace(m.group(1), new_class)

    banner = (
        f'"""\nScene: {new_class}  (scaffolded from {args.template}).\n'
        f"Edit the math functions, TITLE, and FORMULA to match your equation,\n"
        f"then:  ./render.sh {name} --preview   (see the math-equation-video skill).\n"
        f'"""\n'
    )
    # Replace the template's leading module docstring with our banner.
    text = re.sub(r'\A\s*""".*?"""\s*', banner, text, count=1, flags=re.DOTALL)

    dst.write_text(text)
    print(f"created {dst.relative_to(REPO)}  (class {new_class})")
    print("add this to the SCENES map in render.sh:")
    print(f'  [{name}]="mathviz/scenes/{name}.py:{new_class}"')
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
