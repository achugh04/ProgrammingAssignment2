---
name: math-equation-video
description: >-
  Turn any mathematical equation into a vertical short (1080x1920, 24/30/60fps,
  Instagram Reels / YouTube Shorts ready) in the house style: a flat 2D build-up where
  every component of the series/equation animates SIMULTANEOUSLY, then the camera tilts
  and it "turns 3D". Pure visual — no text overlays. Use when the user uploads or names
  an equation/series/function and wants it animated — "visualize this equation", "make
  a reel of this formula", "animate this series". Routes to fourier-epicycles-short or
  taylor-series-short when they match; composes a new manim scene otherwise.
---

# Math Equation → Vertical Video (umbrella)

You are turning a math equation (often an uploaded photo) into a rendered short.
Two ready-made visuals are installed as their **own skills** — route to them first:

| If the equation is… | Use |
|---|---|
| Fourier-type series `Σ aₙ sin/cos(nx)` (square/saw/triangle wave, epicycles) | **`fourier-epicycles-short`** |
| Taylor / Maclaurin / power series `Σ cₙxⁿ` approximating a function | **`taylor-series-short`** |
| Anything else | continue below and compose a new scene |

## House style (applies to every video)

- **Pure visual: zero text.** No title, no creator handle, no formula card, no labels.
  (`ShortsScene` still has an overlay system — leave it off unless explicitly asked.)
- **Everything animates simultaneously.** All series components / partial sums /
  traces build at the same time, not one after another — that's the signature.
- **Flat 2D build → `reveal_3d()` tilt → orbit hold.** Author in the xy-plane, offset
  components along +z so they nest when flat and fan into a receding stack after the tilt.
- Output: **1080×1920, H.264 yuv420p +faststart**, `--fps 24|30|60`.

## Pipeline for a new equation

1. **Toolchain**: if `./render.sh` fails on missing manim/latex/ffmpeg → `./setup.sh` once.
2. **Read the equation**: open the uploaded image, transcribe to LaTeX exactly, state
   in one line what it is. Ask one concise question if a symbol is ambiguous — don't guess.
3. **Classify**: `references/classify.md` maps equation shapes (parametric paths,
   single functions/calculus, surfaces, numeric series…) to visual ideas.
4. **Author**: scaffold from the closest template —
   `python .claude/skills/math-equation-video/scripts/new_scene.py <name> --from <fourier_square|taylor_sine>`
   Edit the math functions and layout constants; register the scene in `render.sh`'s
   `SCENES` map. Never hard-code colors — use `mathviz.style.harmonic_color(i)` etc.
   Read `references/templates.md`, `references/style-and-camera.md`, and
   `references/manim-gotchas.md` before writing scene code.
5. **Verify (mandatory loop)**:
   ```bash
   ./render.sh <name> --preview
   bash .claude/skills/math-equation-video/scripts/frames.sh out/<name>_preview.mp4
   ```
   Read several frames (early 2D, the tilt, late 3D) and check: zero text; components
   animating simultaneously; nothing clipped in either phase; strokes not filled blobs.
   Iterate until clean — never deliver unverified video.
6. **Final render**: `./render.sh <name> --fps 30` (or 24/60 per the ask).
7. **Deliver**: `SendUserFile` the MP4 (`display: "render"`). Rendered videos are
   git-ignored — commit only scene code.

## Guardrails

- One equation → one focused scene.
- Keep the 2D→3D grammar and the simultaneity; they are the channel's signature.
- Prefer adapting an existing template — they encode fixes already learned.
