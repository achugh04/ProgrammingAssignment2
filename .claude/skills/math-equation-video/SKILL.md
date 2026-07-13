---
name: math-equation-video
description: >-
  Turn a mathematical equation into a polished vertical short (1080x1920, 24/30/60fps,
  Instagram Reels / YouTube Shorts ready) in the house "series-visual" style: a flat
  2D build-up that then "turns 3D" with a camera tilt. Use this whenever the user
  uploads or names an equation/series/function and wants it animated or visualized as
  a video — e.g. "visualize this equation", "make a reel of this Fourier series",
  "animate this Taylor expansion", "turn this formula into a short". Built on manim.
---

# Math Equation → Vertical Video

You are turning a math equation (often an uploaded photo/screenshot) into a short,
rendered with **manim** in this repo's house style: a **flat 2D build-up** that
**tilts into 3D** — the signature "…watch till the end, this turns 3D" beat.

Work the pipeline below **in order**. The self-verify step (5) is mandatory — it is
how visual bugs (clipped titles, off-screen curves, filled-in strokes) get caught.

## 0. Orient in the repo

Everything lives at the repo root:

- `mathviz/` — the reusable engine. `style.py` (palette/type/formula-card),
  `base.py` (`ShortsScene`: vertical framing, title/handle/formula overlay, and the
  `set_front_view()` → `reveal_3d()` camera move).
- `mathviz/scenes/` — one file per video. **Copy the closest one as your starting point.**
- `render.sh` — renders a scene to a phone-ready MP4. `setup.sh` — installs the toolchain.
- Read `references/templates.md`, `references/classify.md`, `references/style-and-camera.md`,
  and `references/manim-gotchas.md` in this skill folder before writing a scene.

## 1. Ensure the toolchain

Manim + ffmpeg + a LaTeX subset are required. If `./render.sh` fails with a missing
`manim`/`latex`/`ffmpeg`, run once (takes a few minutes on a fresh container):

```bash
./setup.sh
```

Then every command runs inside the venv (`render.sh` activates it automatically).

## 2. Read the equation

If the user uploaded an image, **open it** and transcribe the math to LaTeX exactly.
State back, in one line, what it is (e.g. "square-wave Fourier series", "Maclaurin
series of eˣ", "logistic map", "a parametric rose r=cos(kθ)"). If the equation is
ambiguous or you can't read a symbol, ask **one** concise clarifying question — don't guess.

## 3. Classify → pick a template

Use `references/classify.md` to map the equation to a visualization + camera pattern,
and `references/templates.md` for what already exists. Rule of thumb:

| Equation shape | Template to copy | Visual |
|---|---|---|
| Series that reconstructs a periodic function `Σ aₙ sin/cos(nx)` | `fourier_square.py` | rotating epicycles draw the wave → explode into a 3D harmonic stack |
| Power/Taylor/Maclaurin series `Σ cₙ xⁿ` approximating `f` | `taylor_sine.py` | partial sums converge on `f` → explode terms onto depth layers |
| Geometric / numeric series converging to a value | `taylor_sine.py` (adapt) | partial-sum curve/bars approach the limit → 3D term stack |
| Parametric / complex `z(t)`, orbits, roses | `fourier_square.py` (adapt the chain) | traced path → tilt to reveal it as a 3D curve |
| Single `f(x)`, derivative, integral/area | `taylor_sine.py` (adapt) | graph + tangent/area build → tilt for depth |
| Surface `z=f(x,y)` | new `ThreeDScene` on `ShortsScene` | `Surface` + `reveal_3d` + orbit |

If nothing fits, **compose a new scene** on `ShortsScene` — keep the grammar:
front view → build in 2D → `reveal_3d(added_anims=[…explode…])` → `orbit()` + hold.

## 4. Author the scene

```bash
python .claude/skills/math-equation-video/scripts/new_scene.py <name> --from <template>
# e.g. new_scene.py cosine_fourier --from fourier_square
```

This copies a template to `mathviz/scenes/<name>.py` with a fresh class name, and
prints the `render.sh` registry line to add. Then edit:

- The **math functions** (the `_term`/`_chain`/`_value`/plotted `f`) to match the equation.
- `TITLE`, `FORMULA` (raw LaTeX for the bottom card), `HANDLE`.
- Domain/scale constants so the drawing fills the column without clipping.
- The **explode** step so each component lands on its own `+z` depth layer.

Never hard-code colors — use `mathviz.style` (`harmonic_color(i)`, `CYAN`, `GOLD`, …).
Add your scene to the `SCENES` map in `render.sh`.

## 5. Preview & self-verify  (MANDATORY loop)

```bash
./render.sh <name> --preview                          # fast 540x960 pass
.claude/skills/math-equation-video/scripts/frames.sh out/<name>_preview.mp4
```

`frames.sh` drops inspection JPEGs in a temp dir; **open several** (an early 2D frame,
the moment of the tilt, and a late 3D frame). Check, and fix in `mathviz/scenes/<name>.py`:

- Title fully on-screen (not clipped left/right).
- Every curve/label inside the frame; nothing collides with the title or formula card.
- Strokes are **lines, not filled blobs** (see manim-gotchas: use
  `.set_fill(opacity=0).set_stroke(...)`, never `set_opacity` on a `ParametricFunction`).
- The 2D→3D tilt reads clearly and the held 3D shot is legible.

Re-preview until it looks right. **Do not render final or deliver an unverified video.**

## 6. Final render

```bash
./render.sh <name> --fps 30      # or --fps 24 / --fps 60 per the user's ask
```

Output: `out/<name>_<fps>fps.mp4` — **1080×1920, H.264 yuv420p, +faststart**, directly
uploadable to Reels/Shorts. If the user wants multiple frame rates, render each.

## 7. Deliver

Send the file with `SendUserFile` (status `proactive`, `display: "render"`) and one line
on what it shows. Rendered MP4s live under `out/`/`media/` which are git-ignored — commit
only the **scene code**, never the heavy video, unless the user asks otherwise.

## Guardrails

- Keep the 2D→3D camera grammar — it is the channel's signature (`references/style-and-camera.md`).
- One equation → one focused scene. Don't cram unrelated ideas into a single short.
- Prefer adapting an existing template to writing from scratch; they encode the fixes already.
