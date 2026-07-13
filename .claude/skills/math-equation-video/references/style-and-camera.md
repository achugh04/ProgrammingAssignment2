# House style & the 2D→3D camera grammar

This is the channel's signature. Match it.

## Canvas
- **1080×1920 (9:16)**, near-black `#0d0d12` background (set in `manim.cfg` + `style.BG`).
- World frame is ~4.5 wide × 8 tall. Compose in a vertical column: title up top, the
  action in the upper-middle, formula card + handle pinned at the bottom.

## Palette (`mathviz.style`)
- `CYAN #35d0e0` primary / fundamental · `GOLD #f2c14e` secondary · `WHITE #f4f5f7`
  the combined/final curve · `ORANGE`, `GREEN`, `VIOLET` further terms · `MUTE #6b6b8a`
  axes, ticks, dashed guides.
- Hand successive components colors via `style.harmonic_color(i)`. Never hard-code hex.

## Type
- Title: quiet **serif**, uppercase, ~82% opacity, auto-fit to frame width (handled in
  `base.build_overlay`). Formula card: `MathTex` in a rounded rect with a cyan border
  and translucent dark fill (`style.formula_card`).

## The camera move — the whole point
Two orientations live on `ShortsScene`:
- `FRONT = phi=0°, theta=-90°` — looking straight at the xy-plane. Reads as flat 2D.
- `REVEAL = phi=64°, theta=-105°` — tilted up and rotated; the world gains depth.

**Depth convention:** author everything in the xy-plane (what FRONT shows). To make a
component "explode into 3D", offset it along **+z**. In FRONT a +z offset is invisible
(things overlap / nest); after the tilt it becomes a **receding vertical stack** — the
"…this turns 3D" reveal. So: nested epicycles → a stack of tilted ellipses; overlaid
partial sums → layered curves in depth.

**Execution:**
```python
self.set_front_view()          # in construct(), before animating
# … build & animate the whole thing FLAT …
self.reveal_3d(added_anims=[   # tilt WHILE components separate in +z
    group_i.animate.shift(OUT * z_i) for i, ...
], run_time=2.7)
self.play(*[Create(component_i) ...])   # per-layer detail streams in
self.orbit(rate=0.05); self.wait(3); self.stop_orbit()
```
Keep the title/handle/formula on `add_fixed_in_frame_mobjects` (done in `build_overlay`)
so they stay a flat overlay through the tilt.

## Do / don't
- **Do** keep one clean flat build before the tilt — the contrast is what sells it.
- **Do** keep the exploded stack compact (`DZ ≈ 0.55–0.65`) so the top layer doesn't
  collide with the title after tilting.
- **Don't** start already-3D unless it's a surface. Flat-first, then reveal.
- **Don't** let curves run under the formula card or off the sides — verify with frames.
