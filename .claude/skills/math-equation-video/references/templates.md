# Template catalog

Each scene subclasses `mathviz.ShortsScene`. To make a new video, **copy the closest
template** (via `scripts/new_scene.py`) and edit the math + copy. All share the grammar:
`set_front_view()` → build in 2D → `reveal_3d(added_anims=[…])` → `orbit()` + `wait()`.

## `fourier_square.py` → `FourierSquareWave`
Square-wave Fourier series `f(x)=(4/π)Σ sin((2n-1)x)/(2n-1)`.
- **Knobs:** `KS` (harmonic list), `AMPS` (coefficients), `SY` (amplitude scale),
  `ST` (time→downward scale), `DOMAIN` (how many periods), `CENTER` (assembly anchor),
  `DZ` (depth between exploded layers).
- **Reuse for:** any `Σ aₙ sin/cos(nx)`. Change `KS`/`AMPS`, the `FORMULA` card, and `TITLE`.
- **Structure:** `_chain(x)` builds the nested epicycle vertices; `_value(x)` is the
  wave value; `_wave_point`/`_harmonic_point` place the drawn curves. The reveal shifts
  each circle by `OUT*z` and creates per-harmonic component curves + dashed projections.

## `taylor_sine.py` → `TaylorSine`
Maclaurin series of `sin x`.
- **Knobs:** `N_TERMS`, `_term(n,x)` (the summand), `SX`/`SY` (scale), `ORG` (origin),
  `DZ` (layer depth), per-partial `span` (domain clipping so low orders stay on-screen).
- **Reuse for:** any power/Taylor series or single-function graph. Change `_term`, the
  `FORMULA`, `TITLE`, and the plotted reference curve.
- **Structure:** `_partial(m,x)` sums terms; Phase 1 `Create`s partial sums low→high
  degree; the reveal explodes each `_term` onto a `+z` layer with the sum staying at z=0.

## Shared knobs on `ShortsScene` (see `mathviz/base.py`)
- `TITLE` — serif overlay pinned top (auto-fit to frame width).
- `HANDLE` — small handle pinned bottom (default `@mathviz`; change to the user's).
- `FORMULA` — raw LaTeX for the bordered bottom card (`FORMULA_SCALE`, `FORMULA_WIDTH`).
- `FRONT` / `REVEAL` — the two camera orientations. Override `REVEAL` per-scene if a
  different tilt reads better, but keep a clear flat→tilted move.
- `reveal_3d(added_anims=…, run_time=…)` — the signature tilt; pass the "explode"
  animations so depth separation happens *as* the camera swings.
- `orbit(rate=…)` / `stop_orbit()` — gentle azimuth drift for the held 3D shot.

## Timing recipe (≈13–16 s short)
fade-in ~1s · 2D build ~5–7s · reveal tilt ~2.5–2.8s · component stream-in ~1.5s ·
orbit hold ~3s · settle ~0.4s. Keep it under ~20s for Reels/Shorts.
