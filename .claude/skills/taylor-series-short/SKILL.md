---
name: taylor-series-short
description: >-
  Render the Taylor/Maclaurin convergence visual as a vertical short (1080x1920,
  24/30/60fps, Reels/Shorts-ready): every polynomial partial sum lives on its own
  depth layer from frame one (crudest deepest, best hugging the target up front) and
  they all draw simultaneously; the flat front view reads as classic 2D convergence,
  then a camera-only tilt reveals the staircase of approximations. Pure visual — no
  text overlays. Use when the user wants a Taylor / Maclaurin / power-series
  approximation video (sin, cos, e^x, ln(1+x), …), or names this skill.
---

# Taylor Series Short

Renders `TaylorSine` (or a variant) from `mathviz/scenes/taylor_sine.py`. **3D-native:**
partial sum m is authored at its own depth from the first frame — depth encodes
approximation order (degree-1 line deepest, the best approximation white on the front
layer, hugging the faint true curve). All of them **draw simultaneously**; the
orthographic front view collapses the stack into the classic flat convergence
picture, and the reveal is a **pure camera move** that exposes the depth staircase,
held with a slow orbit. **No text anywhere.**

## Render as-is (sin x)

```bash
./setup.sh                      # only if the toolchain is missing
./render.sh taylor --fps 60     # or 24 / 30 → out/taylor_<fps>fps.mp4  (1080x1920)
```

## Adapt to a different function

Edit `mathviz/scenes/taylor_sine.py`:

- `_term(n, x)` — the n-th series term. Common swaps:
  - `eˣ`: `x**n / math.factorial(n)` (every degree, not just odd)
  - `cos x`: `(-1)**n * x**(2n) / math.factorial(2n)`
  - `ln(1+x)`: `(-1)**(n+1) * x**n / n` (domain |x|<1 — shrink the span!)
  - `1/(1-x)`: `x**n` (domain |x|<1)
- The faint reference curve (`true_sin`) → plot the actual target function.
- `N_TERMS`, `SX`/`SY`, `ORG`, `DZ`, and the per-partial `span` clipping (low-degree
  partials diverge fastest near the edges — clip so they stay on-screen).

For a keeper variant, scaffold a copy:
`python .claude/skills/math-equation-video/scripts/new_scene.py <name> --from taylor_sine`
and register it in `render.sh`'s `SCENES` map.

## Verify before delivering (mandatory)

```bash
./render.sh taylor --preview
bash .claude/skills/math-equation-video/scripts/frames.sh out/taylor_preview.mp4
```

Open several frames and check: zero text; multiple partial sums mid-draw in the same
frame; the degree-1 straight line not clipping corners; a clean layered 3D reveal
(gotchas: `../math-equation-video/references/manim-gotchas.md`). Then final-render
and send the MP4 with `SendUserFile` (`display: "render"`). Don't commit rendered videos.
