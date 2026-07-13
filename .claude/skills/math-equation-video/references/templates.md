# Template catalog

Each scene subclasses `mathviz.ShortsScene`. To make a new video, **copy the closest
template** (via `scripts/new_scene.py`) and edit the math + layout. All share the
grammar: **author every component at its true z from frame one** (3D-native; the
orthographic front view collapses depth) → `set_front_view()` → flat build with
**every component animating simultaneously** → camera-only reveal (`reveal_3d()` /
`move_camera`, animation keeps running through it) → `orbit()` + finish. No text overlays.

The two shipped templates are also installed as standalone skills:
**`fourier-epicycles-short`** and **`taylor-series-short`** — route to those skills
directly when the equation matches; use this catalog when composing something new.

## `fourier_square.py` → `FourierSquareWave`   (skill: fourier-epicycles-short)
Square-wave Fourier series `f(x)=(4/π)Σ sin((2n-1)x)/(2n-1)`.
- **3D-native machine:** circle i lives on plane `Z[i]` (fundamental on top); each
  tip drops one `DZ` straight down in z to the next circle's center (invisible
  head-on, the vertical dashed links once tilted). Joint n traces its n-term partial
  sum on its own layer (cyan/gold/orange; white full sum on the front layer z=0),
  all simultaneously, each with an in-plane dashed connector and pen dot. The reveal
  is `move_camera(**REVEAL, added_anims=[xt…])` — the machine draws through it.
- **Knobs:** `KS` (harmonic list), `AMPS` (coefficients), `SY` (amplitude scale),
  `ST` (time→downward scale), `DOMAIN` (periods drawn), `CENTER` (machine anchor),
  `DZ` (layer spacing, sets `Z`).
- **Reuse for:** any `Σ aₙ sin/cos(nx)` — change `KS`/`AMPS`. Sawtooth: all n,
  `2/(πn)·(-1)^{n+1}`. Triangle: odd n, `8/(π²n²)` alternating.

## `taylor_sine.py` → `TaylorSine`   (skill: taylor-series-short)
Maclaurin series of `sin x`.
- **3D-native staircase:** partial sum m is authored at depth `Z[m]` from frame one —
  crudest deepest, best (white) hugging the faint true curve on the front layer. All
  draw simultaneously in one shared play; the reveal is a pure camera move.
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
