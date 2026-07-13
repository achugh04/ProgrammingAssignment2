---
name: fourier-epicycles-short
description: >-
  Render the Fourier epicycles visual as a vertical short (1080x1920, 24/30/60fps,
  Reels/Shorts-ready): a 3D-native epicycle machine — each harmonic circle on its own
  depth layer — traces every partial sum simultaneously; the flat front view reads as
  classic 2D, then a camera-only tilt reveals the receding harmonic stack while the
  machine keeps drawing. Pure visual — no text overlays. Use when the user wants a
  Fourier-series / epicycle / harmonic-circles video (square wave, sawtooth, triangle,
  or any sum of sines), or names this skill.
---

# Fourier Epicycles Short

Renders `FourierSquareWave` (or a variant) from `mathviz/scenes/fourier_square.py`.
The machine is **authored in 3D from the first frame**: harmonic circle i lives on its
own z-plane, each tip drops one layer to the next circle's center, and every joint
traces its partial sum on its own layer (cyan → gold → orange; the white full sum on
the front layer). The orthographic front view collapses this to the classic 2D
picture; the reveal is a **pure camera move** — the tilt exposes the depth that was
always there, and the machine keeps spinning and drawing through the tilt and the
closing orbit. **No text anywhere.**

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
