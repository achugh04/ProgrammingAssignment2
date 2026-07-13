# manim gotchas (learned the hard way — read before debugging)

**Strokes rendering as filled blobs.** `ParametricFunction.set_opacity(v)` sets *both*
fill and stroke opacity, so your curve fills in as a solid shape. Always:
```python
curve = ParametricFunction(...).set_fill(opacity=0).set_stroke(opacity=0.9)
```

**Odd pixel dimensions crash x264.** libx264 + yuv420p needs even width/height. Preview
resolution is `540,960` (not `405,720`). Final is `1080,1920`. Keep both even.

**`-ql`/`-qh` override your resolution.** The quality flags force 854×480 / 1920×1080
*landscape* and ignore `manim.cfg`. Don't use them here — `render.sh` passes explicit
`-r 1080,1920 --fps N` to stay vertical.

**Import path.** Scenes do `from mathviz import ...`, which needs the repo root on
`PYTHONPATH`. `render.sh` sets it; if you invoke manim by hand, run from the repo root
with `PYTHONPATH=$PWD`.

**LaTeX must be installed** for `MathTex`/`Tex` (the formula card). If a render dies in a
`Tex`/`dvisvgm` step, run `./setup.sh` (installs the texlive subset). For plain labels
without LaTeX use `Text(...)` (Pango) instead.

**Fonts.** Use `font="serif"` (a Pango generic alias) rather than a specific family name
that may be absent on the render box.

**Animating `always_redraw` / updater mobjects.** You can't cleanly `.animate` a mobject
that an updater is rewriting every frame. Drive the build with a `ValueTracker` + updaters,
then `m.clear_updaters()` on each before the reveal so you can `shift`/`Transform` them.

**Camera + updaters.** During `move_camera`, updaters keep firing. If the driving
`ValueTracker` is frozen, updater'd mobjects stay put (good). `add_fixed_in_frame_mobjects`
keeps overlays flat through camera moves; add them *after* the first camera orientation is set.

**Title/label clipping.** The 9:16 column is narrow (~4.5 world units). Fit wide text with
`mob.set(width=config.frame_width - 0.5)` (the base does this for the title).

**Render is slow at 60fps/1080p.** Iterate with `--preview` (540×960, 15fps). Only render
final `--fps 30/60` once the preview frames look right.
