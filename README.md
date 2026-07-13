# Interesting Math Series / Equations Visuals

Turn a mathematical equation into a polished **vertical short** — 1080×1920, 24 / 30 / 60 fps,
H.264 (yuv420p, faststart), directly uploadable to **Instagram Reels** and **YouTube Shorts**.

Every video follows one signature grammar: a clean **flat 2D build-up** that then
**"turns 3D"** with a camera tilt — nested pieces exploding into a receding, orbiting
stack. Rendered with [**manim**](https://www.manim.community/) (the best-in-class engine
for exactly this kind of 2D→3D mathematical animation).

<p align="center"><em>Flat epicycles draw the wave → the camera tilts → the harmonics fan out into 3D.</em></p>

## Quick start

```bash
./setup.sh                     # one-time: installs ffmpeg, LaTeX subset, manim (in .venv)
./render.sh fourier --fps 30   # -> out/fourier_30fps.mp4  (1080x1920, Reels/Shorts ready)
./render.sh taylor  --fps 60
./render.sh fourier --preview  # fast 540x960 pass while iterating
```

`--fps` accepts `24`, `30`, or `60`. Output lands in `out/<scene>_<fps>fps.mp4`.

## The skill: any equation → a video

The real point of this repo is the **`math-equation-video` skill** (in
`.claude/skills/`). In Claude Code, **upload a photo of an equation** and ask to
visualize it — Claude reads the equation, classifies it, picks or composes a scene
template, renders it in the house style, self-verifies the frames, and hands back the MP4.

It knows how to map, e.g.:

- a **Fourier series** → rotating epicycles drawing the wave, exploding into a harmonic stack
- a **Taylor / power series** → partial sums converging, then each term on its own depth layer
- **parametric / complex paths**, **single functions** (tangent/area), **surfaces** `z=f(x,y)`, …

See `.claude/skills/math-equation-video/SKILL.md` and its `references/`.

## What's in here

```
mathviz/                  reusable engine
  style.py                palette, serif type, bordered formula card
  base.py                 ShortsScene: 9:16 framing + the 2D→3D camera move
  scenes/
    fourier_square.py     FourierSquareWave  — square-wave epicycles → 3D harmonic stack
    taylor_sine.py        TaylorSine         — sin x convergence → 3D term explosion
render.sh                 render a scene to a phone-ready MP4 (fps / preview / normalize)
setup.sh                  bootstrap the toolchain on a fresh box
manim.cfg                 vertical 1080x1920 defaults
.claude/skills/math-equation-video/   the skill (workflow, references, scaffolder, frame tool)
```

## Add your own scene

```bash
python .claude/skills/math-equation-video/scripts/new_scene.py my_series --from taylor_sine
# edit mathviz/scenes/my_series.py (math + TITLE + FORMULA), add it to render.sh's SCENES map
./render.sh my_series --preview
```

Every scene subclasses `ShortsScene`, so you get the vertical framing, the title/handle/
formula overlay, and `set_front_view() → reveal_3d() → orbit()` for free. Keep the
flat-then-tilt grammar — it's the channel's signature.

## Notes

- Rendered videos (`out/`, `media/`) are **git-ignored** — the repo tracks the code that
  generates them, not the heavy files.
- Formula cards use LaTeX (`MathTex`); `setup.sh` installs the needed texlive subset.
- Requirements: Python 3.11+, ffmpeg, a LaTeX distribution. `setup.sh` handles all three on Ubuntu.
