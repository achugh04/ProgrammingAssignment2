---
name: fourier-epicycles-short
description: >-
  Render the Fourier epicycles visual as a vertical short (1080x1920, 24/30/60fps,
  Reels/Shorts-ready): nested rotating circles simultaneously trace every partial sum
  of a Fourier series in a flat 2D view, then the camera tilts 3D and the harmonics
  explode into a receding orbiting stack. Pure visual — no text overlays. Use when the
  user wants a Fourier-series / epicycle / harmonic-circles video (square wave, sawtooth,
  triangle, or any sum of sines), or names this skill.
---

# Fourier Epicycles Short

Renders `FourierSquareWave` (or a variant) from `mathviz/scenes/fourier_square.py`:
epicycles rotate; **every chain joint traces its own partial sum simultaneously**
(cyan → gold → orange → white full sum, each with a dashed connector); then the
signature tilt — circles explode into a 3D harmonic stack with per-layer component
sines; orbit hold. **No text anywhere** — no title, handle, or formula card.

## Render as-is (square wave)

```bash
./setup.sh                       # only if the toolchain is missing
./render.sh fourier --fps 60     # or 24 / 30 → out/fourier_<fps>fps.mp4  (1080x1920)
```

## Adapt to a different series

Edit the constants at the top of `mathviz/scenes/fourier_square.py`:

- `KS` — which harmonics (list of n), `AMPS` — their coefficients. Common targets:
  - Square wave: odd n, `4/(πn)` (the default)
  - Sawtooth: all n, `2/(πn)` with alternating sign `(-1)^(n+1)`
  - Triangle: odd n, `8/(π²n²)` with alternating sign — converges fast, use ≥4 terms
  - Arbitrary `f`: compute `bₙ = (2/π)∫₀^π f(x)sin(nx)dx` (scipy) and take the top 4–6 terms
- Scale/layout knobs: `SY` (amplitude), `ST` (time→down), `CENTER`, `DOMAIN`, `DZ`
  (3D layer spacing). The partial-sum trace colors live in the scene body.

For a variant you want to keep, scaffold a copy instead of editing in place:
`python .claude/skills/math-equation-video/scripts/new_scene.py <name> --from fourier_square`
and add it to the `SCENES` map in `render.sh`.

## Verify before delivering (mandatory)

```bash
./render.sh fourier --preview
bash .claude/skills/math-equation-video/scripts/frames.sh out/fourier_preview.mp4
```

Open several frames and check: zero text; all partial-sum traces drawing
simultaneously with dashed connectors; nothing clipped in either the flat or 3D
phase; strokes not filled blobs (see
`../math-equation-video/references/manim-gotchas.md`). Then final-render and send
the MP4 with `SendUserFile` (`display: "render"`). Don't commit rendered videos.
