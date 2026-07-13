---
name: taylor-series-short
description: >-
  Render the Taylor/Maclaurin convergence visual as a vertical short (1080x1920,
  24/30/60fps, Reels/Shorts-ready): all polynomial partial sums draw simultaneously,
  hugging the target function, then the camera tilts 3D and each term explodes onto
  its own depth layer. Pure visual — no text overlays. Use when the user wants a
  Taylor / Maclaurin / power-series approximation video (sin, cos, e^x, ln(1+x), …),
  or names this skill.
---

# Taylor Series Short

Renders `TaylorSine` (or a variant) from `mathviz/scenes/taylor_sine.py`: the target
function sits faint in the background; **all partial sums draw simultaneously** in
the house colors, converging onto it; then the signature tilt — each term `cₙxⁿ`
explodes onto its own `+z` layer; orbit hold. **No text anywhere.**

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
