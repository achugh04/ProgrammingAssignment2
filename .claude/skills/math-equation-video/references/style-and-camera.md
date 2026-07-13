# House style & the 2D→3D camera grammar

This is the channel's signature. Match it.

## Canvas
- **1080×1920 (9:16)**, near-black `#0d0d12` background (set in `manim.cfg` + `style.BG`).
- World frame is ~4.5 wide × 8 tall (x∈[-2.25,2.25], y∈[-4,4]). Compose a vertical
  column with generous margins — nothing may touch an edge in either camera phase.

## Pure visual — zero text
**No text overlays of any kind**: no title, no creator handle, no formula card, no
axis labels. The math itself is the whole frame. (`ShortsScene.build_overlay()` still
exists for title/handle/formula — leave it uncalled unless the user explicitly asks.)
Faint axes/guide lines are fine — they're geometry, not text.

## Simultaneity — everything animates at once
The defining motion rule: **all components of the equation build simultaneously.**
- Fourier: every joint of the epicycle chain traces its own partial sum at the same
  time (cyan 1-term, gold 2-term, orange 3-term, white full sum), each with a dashed
  connector from its joint to its trace front.
- Taylor: all partial-sum curves draw in the same beat, converging together.
Never animate components one-after-another in the 2D phase — sequential builds are
off-brand. Drive shared motion from one `ValueTracker` with per-component updaters.

## Palette (`mathviz.style`)
- `CYAN #35d0e0` primary / fundamental · `GOLD #f2c14e` second · `ORANGE #e8833a`
  third · `WHITE #f4f5f7` the combined/final curve · `GREEN`, `VIOLET` extras ·
  `MUTE #6b6b8a` axes and dashed guides.
- Hand successive components colors via `style.harmonic_color(i)`. Never hard-code hex.

## The camera move — the whole point
Two orientations live on `ShortsScene` (both with a huge `focal_distance`, i.e. an
**orthographic** projection — this is essential):
- `FRONT = phi=0°, theta=-90°` — looking straight down z at the xy-planes. Reads as flat 2D.
- `REVEAL = phi=64°, theta=-105°` — tilted up and rotated; the world gains depth.

**Author 3D-native.** Every component is BUILT at its true z from the first frame —
each harmonic circle on its own z-plane, each trace on its layer, chain links dropping
through depth. Because the projection is orthographic, depth has *exactly zero
footprint* in the front view: the flat phase collapses to the perfect classic 2D
picture. The reveal is then a **pure camera move** — no shifting, no fading, nothing
staged — the tilt just *sees the dimension that was always there*, and the animation
(the machine, the drawing) keeps running straight through the tilt and the orbit.

**Execution:**
```python
self.set_front_view()          # in construct(), before animating
# … build everything AT ITS TRUE z; animate all components simultaneously …
self.play(xt.animate(rate_func=linear).set_value(0.55 * DOMAIN), run_time=5.5)
self.move_camera(**self.REVEAL,                       # camera-only reveal;
    added_anims=[xt.animate(rate_func=linear)         # the machine keeps
                 .set_value(0.75 * DOMAIN)],          # drawing through it
    run_time=2.8)
self.orbit(rate=0.05)
self.play(xt.animate(rate_func=linear).set_value(DOMAIN), run_time=3.2)
self.stop_orbit()
```
Never `shift(OUT*z)` components during the reveal and never fade traces at the tilt —
that's the staged version this house style explicitly replaced.

## Do / don't
- **Do** keep one clean flat build before the tilt — the contrast sells the reveal.
- **Do** keep the depth stack compact (`DZ ≈ 0.55–0.65`) so the top layer stays
  in-frame through the whole orbit — check both phases with frames.
- **Do** let the animation keep running through the tilt — a frozen reveal is dead.
- **Don't** open with the tilted camera unless it's a surface. Flat-first, then reveal.
- **Don't** add any text. Don't animate sequentially. Don't let anything clip.
