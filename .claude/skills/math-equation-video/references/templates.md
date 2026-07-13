# Template catalog

Each scene subclasses `mathviz.ShortsScene`. To make a new video, **copy the closest
template** (via `scripts/new_scene.py`) and edit the math + layout. All share the
grammar: `set_front_view()` → build in 2D **with every component animating
simultaneously** → `reveal_3d(added_anims=[…])` → `orbit()` + `wait()`. No text overlays.

The two shipped templates are also installed as standalone skills:
**`fourier-epicycles-short`** and **`taylor-series-short`** — route to those skills
directly when the equation matches; use this catalog when composing something new.

## `fourier_square.py` → `FourierSquareWave`   (skill: fourier-epicycles-short)
Square-wave Fourier series `f(x)=(4/π)Σ sin((2n-1)x)/(2n-1)`.
- **Signature motion:** every joint k of the epicycle chain traces its own k-term
  partial sum simultaneously (cyan/gold/orange, white for the full sum), each with a
  dashed connector from joint to trace front; then circles explode onto +z layers
  with per-layer component sines.
- **Knobs:** `KS` (harmonic list), `AMPS` (coefficients), `SY` (amplitude scale),
  `ST` (time→downward scale), `DOMAIN` (periods drawn), `CENTER` (assembly anchor),
  `DZ` (depth between exploded layers).
- **Reuse for:** any `Σ aₙ sin/cos(nx)` — change `KS`/`AMPS`. Sawtooth: all n,
  `2/(πn)·(-1)^{n+1}`. Triangle: odd n, `8/(π²n²)` alternating.

## `taylor_sine.py` → `TaylorSine`   (skill: taylor-series-short)
Maclaurin series of `sin x`.
- **Signature motion:** all partial-sum curves draw at the same time, converging on
  the faint target; then each term `cₙxⁿ` explodes onto its own +z layer.
- **Knobs:** `N_TERMS`, `_term(n,x)` (the summand), `SX`/`SY` (scale), `ORG` (origin),
  `DZ` (layer depth), per-partial `span` (domain clipping so low orders stay on-screen).
- **Reuse for:** any power/Taylor series or single-function build — change `_term`,
  the reference curve, and the domain.

## Shared machinery on `ShortsScene` (see `mathviz/base.py`)
- `FRONT` / `REVEAL` — the two camera orientations. Override `REVEAL` per-scene only
  if a different tilt clearly reads better; keep a flat→tilted move.
- `reveal_3d(added_anims=…, run_time=…)` — the signature tilt; pass the "explode"
  animations so depth separation happens *as* the camera swings.
- `orbit(rate=…)` / `stop_orbit()` — gentle azimuth drift for the held 3D shot.
- `build_overlay()` — optional title/handle/formula overlay. **Off by default** (house
  style is pure visual); only call it if the user explicitly asks for on-video text.

## Timing recipe (≈13–16 s short)
fade-in ~1s · simultaneous 2D build ~5–7s · reveal tilt ~2.5–2.8s · per-layer detail
~1.5s · orbit hold ~3s · settle ~0.4s. Keep it under ~20s for Reels/Shorts.
