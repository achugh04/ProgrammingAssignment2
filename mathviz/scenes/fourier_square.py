"""
Pure-visual reproduction of the reference short — no text anywhere.

Nested rotating epicycles (the odd harmonics of a square-wave Fourier series)
spin in a flat front view while EVERY joint of the chain traces its own partial
sum simultaneously: cyan (1 term), gold (2 terms), orange (3 terms) and the
white full sum, each streaming downward with a dashed connector and a pen dot
at its moving front. Then the signature beat — reveal_3d() tilts the camera,
the colored partial traces fade away, and the circles explode into a receding
stack of harmonic layers, each carrying its own component sine, joined by
dashed projections and stack links; a slow orbit holds the 3D shot.

Render:
    ./render.sh fourier --fps 60
    manim -qh mathviz/scenes/fourier_square.py FourierSquareWave
"""
from __future__ import annotations

import numpy as np
from manim import (
    Circle,
    DashedLine,
    Dot,
    Line,
    ParametricFunction,
    ValueTracker,
    VGroup,
    FadeIn,
    FadeOut,
    Create,
    OUT,
    linear,
)

from mathviz import ShortsScene, style

# ---- Fourier data: square wave  f(x) = (4/π) Σ_{n odd} sin(nx)/n ------------ #
KS = [1, 3, 5, 7]                       # odd harmonics
AMPS = [4.0 / (np.pi * k) for k in KS]  # true Fourier amplitudes

# ---- layout / scale --------------------------------------------------------- #
SY = 0.55          # amplitude → world units (shared by circles AND wave)
ST = 0.50          # domain (time) → downward world units
# With no title above and no card below, the action owns the whole column:
# the assembly sits high, the traces stream well clear of the bottom edge.
CENTER = np.array([0.0, 1.9, 0.0])      # epicycle assembly anchor
DOMAIN = 3.0 * np.pi                     # 1.5 periods, fills the column
DZ = 0.60          # depth between harmonic layers in the 3D reveal

# ---- per-trace styling (k = number of summed harmonics) --------------------- #
TRACE_COLORS = [style.CYAN, style.GOLD, style.ORANGE, style.WHITE]
TRACE_WIDTHS = [2.0, 2.0, 2.0, 3.0]
TRACE_OPACS = [0.8, 0.8, 0.8, 1.0]
TRACE_STEP = 0.04  # coarse sampling keeps the per-frame rebuilds cheap


def _chain(x: float):
    """Nested epicycle vertices P0..PN in the xy-plane at parameter x."""
    pts = [CENTER.copy()]
    for k, a in zip(KS, AMPS):
        pts.append(pts[-1] + np.array([a * SY * np.sin(k * x),
                                       a * SY * np.cos(k * x), 0.0]))
    return pts


def _partial_value(n: int, x: float) -> float:
    """Partial-sum deflection using the first n harmonics."""
    return sum(a * SY * np.sin(k * x) for k, a in zip(KS[:n], AMPS[:n]))


def _partial_point(n: int, s: float, z: float = 0.0):
    """A point on partial-sum trace n: horizontal = value, vertical = time."""
    return np.array([CENTER[0] + _partial_value(n, s), CENTER[1] - s * ST, z])


def _harmonic_point(i: int, s: float, z: float = 0.0):
    """A point on the i-th harmonic's *own* sine component."""
    k, a = KS[i], AMPS[i]
    return np.array([CENTER[0] + a * SY * np.sin(k * s), CENTER[1] - s * ST, z])


class FourierSquareWave(ShortsScene):
    TITLE = ""
    HANDLE = ""
    FORMULA = None

    def construct(self):
        self.set_front_view()

        # ---- faint vertical "0" axis the wave is drawn against ------------- #
        axis = Line(
            np.array([0.0, CENTER[1] + 0.15, 0.0]),
            np.array([0.0, CENTER[1] - DOMAIN * ST - 0.2, 0.0]),
            color=style.MUTE, stroke_width=1.2,
        ).set_opacity(0.5)

        # ---- epicycle mobjects (persistent; driven by updaters) ------------ #
        xt = ValueTracker(0.0)

        circles, radii = [], []
        for i, (k, a) in enumerate(zip(KS, AMPS)):
            col = style.harmonic_color(i)
            circles.append(Circle(radius=a * SY, color=col,
                                   stroke_width=2.2, stroke_opacity=0.85))
            radii.append(Line(color=col, stroke_width=2.0))

        def bind():
            for i in range(len(KS)):
                def c_upd(m, i=i):
                    m.move_to(_chain(xt.get_value())[i])
                def r_upd(m, i=i):
                    p = _chain(xt.get_value())
                    m.put_start_and_end_on(p[i] + 1e-6, p[i + 1])
                circles[i].add_updater(c_upd)
                radii[i].add_updater(r_upd)

        bind()

        # ---- simultaneous partial-sum traces (the key move) ----------------- #
        # After the n-th circle, joint n carries a pen that traces the sum of
        # the first n harmonics. All four stream down at once: cyan, gold,
        # orange, then the white full sum. Each gets a dashed connector from
        # its chain joint to the moving trace front, plus a pen dot.
        traces, connectors, pens = [], [], []
        for n in range(1, len(KS) + 1):
            col = TRACE_COLORS[n - 1]
            w = TRACE_WIDTHS[n - 1]
            op = TRACE_OPACS[n - 1]

            tr = ParametricFunction(
                lambda s, n=n: _partial_point(n, s),
                t_range=[0, 1e-3, TRACE_STEP], color=col, stroke_width=w,
            ).set_fill(opacity=0).set_stroke(opacity=op)
            tr.add_updater(lambda m, n=n, col=col, w=w, op=op: m.become(
                ParametricFunction(
                    lambda s: _partial_point(n, s),
                    t_range=[0, max(xt.get_value(), 1e-3), TRACE_STEP],
                    color=col, stroke_width=w,
                ).set_fill(opacity=0).set_stroke(opacity=op)))
            traces.append(tr)

            conn = DashedLine(color=style.MUTE, stroke_width=1.2,
                              dash_length=0.06, stroke_opacity=0.5)
            conn.add_updater(lambda m, n=n: m.put_start_and_end_on(
                _chain(xt.get_value())[n],
                _partial_point(n, xt.get_value()) + 1e-6))
            connectors.append(conn)

            p = Dot(color=col, radius=0.035)
            p.add_updater(lambda m, n=n: m.move_to(
                _partial_point(n, xt.get_value())))
            pens.append(p)

        # ---- Phase 1: assemble + spin + draw (flat front view) ------------- #
        self.play(FadeIn(axis), Create(VGroup(*circles)), run_time=1.0)
        self.add(*radii, *traces, *connectors, *pens)
        self.play(xt.animate.set_value(DOMAIN), run_time=7.0, rate_func=linear)

        # freeze the machinery so we can explode it into depth
        for m in (*circles, *radii, *traces, *connectors, *pens):
            m.clear_updaters()

        # ---- Phase 2: the reveal — explode into a 3D harmonic stack -------- #
        # The colored partial traces (and every connector/pen) fade out; only
        # the white full sum stays behind at z=0 while the circles recede.
        explode = [FadeOut(VGroup(*traces[:-1], *connectors, *pens))]
        comp_waves, proj_lines, stack_links = [], [], []
        prev_center = None
        for i in range(len(KS)):
            z = (i + 1) * DZ
            grp = VGroup(circles[i], radii[i])
            explode.append(grp.animate.shift(OUT * z))

            comp = ParametricFunction(
                lambda s, i=i, z=z: _harmonic_point(i, s, z),
                t_range=[0, DOMAIN, 0.02],
                color=style.harmonic_color(i), stroke_width=2.2,
            ).set_fill(opacity=0).set_stroke(opacity=0.9)
            comp_waves.append(comp)

        # camera tilt happens WHILE the layers separate
        self.reveal_3d(added_anims=explode, run_time=2.8)

        # component waves + dashed projections stream in on their layers
        for i in range(len(KS)):
            z = (i + 1) * DZ
            top = _harmonic_point(i, 0.0, z)
            proj = DashedLine(circles[i].get_center(), top,
                              color=style.MUTE, stroke_width=1.1,
                              dash_length=0.05).set_opacity(0.6)
            proj_lines.append(proj)
            center = circles[i].get_center()
            if prev_center is not None:
                stack_links.append(DashedLine(prev_center, center,
                                              color=style.MUTE, stroke_width=1.0,
                                              dash_length=0.06).set_opacity(0.5))
            prev_center = center

        self.play(
            *[Create(w) for w in comp_waves],
            *[FadeIn(p) for p in proj_lines],
            *[Create(s) for s in stack_links],
            run_time=1.6,
        )

        # ---- Phase 3: cinematic orbit hold -------------------------------- #
        self.orbit(rate=0.05)
        self.wait(3.0)
        self.stop_orbit()
        self.wait(0.4)
