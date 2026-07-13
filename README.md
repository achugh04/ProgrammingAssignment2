# Interesting Math Series / Equations Visuals

Turn a mathematical equation into a **pure-visual vertical short** — 1080×1920,
24 / 30 / 60 fps, H.264 (yuv420p, faststart), directly uploadable to **Instagram
Reels** and **YouTube Shorts**. No titles, no watermarks, no text — just the math.

Every video follows one signature grammar — and it's **3D-native**: every component
is authored at its true depth from the first frame, viewed through an orthographic
camera.

1. Head-on, depth is invisible: the opening reads as a **flat 2D build-up** in which
   *every* component of the equation animates **simultaneously** (all partial sums
   trace at once). Then
2. a **pure camera tilt** reveals the dimension that was always there — the
   components were a receding, orbiting stack all along, and the animation keeps
   drawing straight through the reveal. Nothing shifts, nothing fades, nothing is staged.

Rendered with [**manim**](https://www.manim.community/), the best-in-class engine for
this kind of 2D→3D mathematical animation.

## Quick start

```bash
./setup.sh                     # one-time: installs ffmpeg, LaTeX subset, manim (in .venv)
./render.sh fourier --fps 60   # -> out/fourier_60fps.mp4  (1080x1920, Reels/Shorts ready)
./render.sh taylor  --fps 30
./render.sh fourier --preview  # fast 540x960 pass while iterating
```

`--fps` accepts `24`, `30`, or `60`. Output lands in `out/<scene>_<fps>fps.mp4`.

## The skills

Three skills are installed in `.claude/skills/` (available automatically in Claude
Code sessions on this repo):

| Skill | What it renders |
|---|---|
| **`fourier-epicycles-short`** | Rotating epicycles trace *all* partial sums of a Fourier series simultaneously, then explode into a 3D harmonic stack. Adaptable to square/saw/triangle or any sum of sines. |
| **`taylor-series-short`** | All Taylor/Maclaurin partial sums draw at once, converging on the target function, then each term explodes onto its own depth layer. Adaptable to eˣ, cos, ln(1+x), … |
| **`math-equation-video`** | The umbrella: **upload a photo of any equation** and ask to visualize it — it classifies the math, routes to one of the skills above or composes a new scene, renders, self-verifies frame-by-frame, and returns the MP4. |

## What's in here

```
mathviz/                  reusable engine
  style.py                palette + (optional, off-by-default) text/formula overlays
  base.py                 ShortsScene: 9:16 framing + the 2D→3D camera move
  scenes/
    fourier_square.py     FourierSquareWave  — simultaneous partial-sum epicycles → 3D stack
    taylor_sine.py        TaylorSine         — simultaneous convergence → 3D term explosion
render.sh                 render a scene to a phone-ready MP4 (fps / preview / normalize)
setup.sh                  bootstrap the toolchain on a fresh box
manim.cfg                 vertical 1080x1920 defaults
.claude/skills/           fourier-epicycles-short · taylor-series-short · math-equation-video
```

## Add your own scene

```bash
python .claude/skills/math-equation-video/scripts/new_scene.py my_series --from fourier_square
# edit mathviz/scenes/my_series.py (the math), add it to render.sh's SCENES map
./render.sh my_series --preview
```

Every scene subclasses `ShortsScene`: vertical framing, the orthographic camera and
`set_front_view() → reveal_3d() → orbit()` come free. House rules: **zero text**,
**all components animate simultaneously**, **author everything at its true depth
from frame one** — the reveal is a camera move, never a rearrangement.

## Notes

- Rendered videos (`out/`, `media/`) are **git-ignored** — the repo tracks the code
  that generates them, not the heavy files.
- Requirements: Python 3.11+, ffmpeg, LaTeX (only needed if you turn the optional
  formula overlay back on). `setup.sh` handles everything on Ubuntu.
