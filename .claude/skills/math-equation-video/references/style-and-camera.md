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
Two orientations live on `ShortsScene`:
- `FRONT = phi=0°, theta=-90°` — looking straight at the xy-plane. Reads as flat 2D.
- `REVEAL = phi=64°, theta=-105°` — tilted up and rotated; the world gains depth.

**Depth convention:** author everything in the xy-plane (what FRONT shows). To make a
component "explode into 3D", offset it along **+z**. In FRONT a +z offset is invisible
(things overlap / nest); after the tilt it becomes a **receding vertical stack** — the
"…this turns 3D" reveal. Nested epicycles → a stack of tilted ellipses; overlaid
partial sums → layered curves in depth.

**Execution:**
```python
self.set_front_view()          # in construct(), before animating
# … build & animate the whole thing FLAT, all components simultaneously …
self.reveal_3d(added_anims=[   # tilt WHILE components separate in +z
    group_i.animate.shift(OUT * z_i) for i, ...
], run_time=2.7)
self.play(*[Create(component_i) ...])   # per-layer detail streams in
self.orbit(rate=0.05); self.wait(3); self.stop_orbit()
```

## Do / don't
- **Do** keep one clean flat build before the tilt — the contrast sells the reveal.
- **Do** keep the exploded stack compact (`DZ ≈ 0.55–0.65`) so the top layer stays
  in-frame through the whole orbit.
- **Don't** start already-3D unless it's a surface. Flat-first, then reveal.
- **Don't** add any text. Don't animate sequentially. Don't let anything clip.
