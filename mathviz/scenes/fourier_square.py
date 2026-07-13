"""
Flagship reproduction of the reference short: a Fourier square-wave built from
rotating epicycles that draws the wave downward, then "turns 3D" — the nested
circles explode into a receding stack of harmonic layers, each carrying its own
sine component, exactly the reference's camera beat.

Render:
    ./render.sh fourier --fps 30
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
    VGroup,
    FadeIn,
    FadeOut,
    Create,
    Write,
    OUT,
    linear,
    smooth,
    rate_functions,
)

from mathviz import ShortsScene, style

# ---- Fourier data: square wave  f(x) = (4/π) Σ_{n odd} sin(nx)/n ------------ #
KS = [1, 3, 5, 7]                       # odd harmonics
AMPS = [4.0 / (np.pi * k) for k in KS]  # true Fourier amplitudes

# ---- layout / scale --------------------------------------------------------- #
SY = 0.55          # amplitude → world units (shared by circles AND wave)
ST = 0.50          # domain (time) → downward world units
# Assembly sits low enough that its full vertical reach clears the title band,
# and the drawn wave ends above the formula card.
CENTER = np.array([0.0, 1.65, 0.0])     # epicycle assembly anchor
DOMAIN = 2.5 * np.pi                     # 1.25 periods, fills the column
DZ = 0.60          # depth between harmonic layers in the 3D reveal


def _chain(x: float):
    """Nested epicycle vertices P0..PN in the xy-plane at parameter x."""
    pts = [CENTER.copy()]
    for k, a in zip(KS, AMPS):
        pts.append(pts[-1] + np.array([a * SY * np.sin(k * x),
                                       a * SY * np.cos(k * x), 0.0]))
    return pts


def _value(x: float) -> float:
    """Square-wave value (horizontal deflection) at x."""
    return sum(a * SY * np.sin(k * x) for k, a in zip(KS, AMPS))


def _wave_point(s: float, z: float = 0.0):
    """A point on the drawn wave: horizontal = value, vertical = time (down)."""
    return np.array([CENTER[0] + _value(s), CENTER[1] - s * ST, z])


def _harmonic_point(i: int, s: float, z: float = 0.0):
    """A point on the i-th harmonic's *own* sine component."""
    k, a = KS[i], AMPS[i]
    return np.array([CENTER[0] + a * SY * np.sin(k * s), CENTER[1] - s * ST, z])


class FourierSquareWave(ShortsScene):
    TITLE = "Fourier Series"
    HANDLE = "@mathviz"
    FORMULA = (
        r"f(x)=\dfrac{4}{\pi}\sum_{n=1}^{\infty}"
        r"\dfrac{\sin\!\big((2n-1)x\big)}{2n-1}"
    )
    FORMULA_SCALE = 0.6

    def construct(self):
        self.set_front_view()
        self.build_overlay()

        # ---- faint vertical "0" axis the wave is drawn against ------------- #
        axis = Line(
            np.array([0.0, CENTER[1] + 0.15, 0.0]),
            np.array([0.0, CENTER[1] - DOMAIN * ST + 0.2, 0.0]),
            color=style.MUTE, stroke_width=1.2,
        ).set_opacity(0.5)

        # ---- epicycle mobjects (persistent; driven by updaters) ------------ #
        from manim import ValueTracker
        xt = ValueTracker(0.0)

        circles, radii = [], []
        for i, (k, a) in enumerate(zip(KS, AMPS)):
            col = style.harmonic_color(i)
            circles.append(Circle(radius=a * SY, color=col,
                                   stroke_width=2.2, stroke_opacity=0.85))
            radii.append(Line(color=col, stroke_width=2.0))
        pen = Dot(color=style.WHITE, radius=0.04)

        def bind():
            for i in range(len(KS)):
                def c_upd(m, i=i):
                    m.move_to(_chain(xt.get_value())[i])
                def r_upd(m, i=i):
                    p = _chain(xt.get_value())
                    m.put_start_and_end_on(p[i] + 1e-6, p[i + 1])
                circles[i].add_updater(c_upd)
                radii[i].add_updater(r_upd)
            pen.add_updater(lambda m: m.move_to(_chain(xt.get_value())[-1]))

        bind()

        # combined wave (white) + vertical connector, both live off xt
        wave = ParametricFunction(
            lambda s: _wave_point(s), t_range=[0, 1e-3], color=style.WHITE,
            stroke_width=3.0,
        )
        wave.add_updater(lambda m: m.become(ParametricFunction(
            _wave_point, t_range=[0, max(xt.get_value(), 1e-3)],
            color=style.WHITE, stroke_width=3.0)))
        connector = DashedLine(color=style.MUTE, stroke_width=1.4, dash_length=0.06)
        connector.add_updater(lambda m: m.put_start_and_end_on(
            _chain(xt.get_value())[-1], _wave_point(xt.get_value()) + 1e-6))

        # ---- Phase 1: assemble + spin + draw (flat front view) ------------- #
        self.play(FadeIn(axis), Create(VGroup(*circles)), run_time=1.0)
        self.add(*radii, pen, wave, connector)
        self.play(xt.animate.set_value(DOMAIN), run_time=6.0, rate_func=linear)

        # freeze the machinery so we can explode it into depth
        for m in (*circles, *radii, pen, wave, connector):
            m.clear_updaters()

        # ---- Phase 2: the reveal — explode into a 3D harmonic stack -------- #
        explode = []
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
