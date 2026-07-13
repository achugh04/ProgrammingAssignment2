# Classifying an equation → a visualization

Read the transcribed LaTeX and match it to a *visual idea* + the standard camera
grammar (build flat — all components simultaneously — then tilt to 3D). Pick the
first row that fits. Rows 1 and 2 have dedicated installed skills
(`fourier-epicycles-short`, `taylor-series-short`) — route to those directly.

## Decision guide

**1. Fourier-type series** — `f(x) = Σ aₙ sin(nx) + bₙ cos(nx)` reconstructing a
periodic target (square/saw/triangle wave, or any `f` on `[-π,π]`).
- Visual: nested **rotating epicycles** (one circle per harmonic, radius = |coeff|,
  angular speed = n) whose tip **draws the wave**; the partial sum shows Gibbs ripples.
- 3D beat: the nested circles **explode along +z** into a stack of harmonic layers,
  each carrying its own sine component, dashed projections between them.
- Start from `fourier_square.py`. Change `KS` (which harmonics) and `AMPS` (coefficients).
  Saw wave: all n, `aₙ = 2/(nπ)·(-1)^{n+1}`. Triangle: odd n, `∝ 1/n²`.

**2. Taylor / Maclaurin / power series** — `f(x) = Σ cₙ (x-a)ⁿ`.
- Visual: draw `f` faintly, then **stack partial sums** of increasing degree that hug
  `f` over a widening interval.
- 3D beat: **explode each term** `cₙxⁿ` onto its own depth layer; they sum to `f` at z=0.
- Start from `taylor_sine.py`. Change `_term(n,x)` and the `FORMULA`. Examples:
  `eˣ: xⁿ/n!` · `cos x: (-1)ⁿx^{2n}/(2n)!` · `ln(1+x): (-1)^{n+1}xⁿ/n` · `1/(1-x): xⁿ`.

**3. Convergent numeric / geometric series** — `Σ arⁿ`, `Σ 1/n²`, etc. → a number.
- Visual: partial-sum **staircase or bar tower** climbing toward a faint limit line
  (a line, not a label — house style is zero text).
- 3D beat: lay successive partial sums into depth so the approach to the limit is a
  receding ramp. Adapt `taylor_sine.py` (treat each partial sum as a "layer").

**4. Parametric / complex path** — `z(t)=…`, `(x(t),y(t))`, polar `r(θ)`, roses, spirographs.
- Visual: a dot traces the **path**; optionally an epicycle chain generates it.
- 3D beat: tilt to reveal the traced path is a **3D curve**, or lift `t` into depth.
- Adapt the epicycle chain in `fourier_square.py`, or plot a `ParametricFunction` directly.

**5. Single function / calculus** — `f(x)`, `f'(x)`, `∫f`, a limit, a tangent line.
- Visual: graph `f`; animate the **tangent sweeping** (derivative) or **area filling**
  (integral, use `axes.get_area`), with the value read out.
- 3D beat: tilt for depth, or stack `f, f', f''` on layers. Adapt `taylor_sine.py`.

**6. Surface / field** — `z=f(x,y)`, vector field, `∇`, a saddle, a Gaussian.
- Visual: a manim `Surface`/`ThreeDAxes`; this one **starts** closer to 3D.
- Camera: open near `FRONT` looking down, then `reveal_3d` + slow `orbit()`.
- Write a fresh `ShortsScene` (it already subclasses `ThreeDScene`).

## When unsure
Ask yourself: *what does this equation MOVE?* A series → terms accumulating. A function
→ a curve/area/tangent. A parametric form → a path. Animate that motion, then tilt.
If two rows both fit, prefer the one with an existing template.
