"""
3D-native reproduction of the reference short — no text, nothing staged.

The epicycle machine is BUILT in three dimensions from the first frame:
harmonic circle i lives on its own z-plane (the fundamental on top of the
stack), and each circle's tip drops one layer straight down in z to become
the center of the next circle — so viewed head-on (the camera's orthographic
front view) the chain collapses exactly to the classic nested 2D epicycles.
Every joint traces its partial sum on its own layer (cyan 1-term, gold
2-term, orange 3-term), and the final pen drops to the front layer (z=0)
where it traces the white full sum.

The camera move is therefore the whole reveal: the tilt just *sees the
dimension that was always there* — the vertical chain drops appear, the
braided traces separate into a receding stack — and the machine keeps
spinning and drawing straight through the tilt and the closing orbit.
Nothing fades, nothing is shifted, nothing is faked.

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
    Create,
    linear,
)

from mathviz import ShortsScene, style

# ---- Fourier data: square wave  f(x) = (4/π) Σ_{n odd} sin(nx)/n ------------ #
KS = [1, 3, 5, 7]                       # odd harmonics
AMPS = [4.0 / (np.pi * k) for k in KS]  # true Fourier amplitudes
N = len(KS)

# ---- layout / scale --------------------------------------------------------- #
SY = 0.55          # amplitude → world units (shared by circles AND traces)
ST = 0.50          # domain (time) → downward world units
CENTER = np.array([0.0, 1.9, 0.0])      # machine anchor (xy; the top layer in z)
DOMAIN = 3.0 * np.pi                     # 1.5 periods, fills the column
DZ = 0.60          # depth between layers

# Circle i's plane. The fundamental sits highest; each chain link drops one DZ
# so the final pen (and the white full-sum trace) land on the front layer z=0.
Z = [(N - i) * DZ for i in range(N)]

# ---- per-trace styling (n = number of summed harmonics) --------------------- #
TRACE_COLORS = [style.CYAN, style.GOLD, style.ORANGE, style.WHITE]
TRACE_WIDTHS = [2.0, 2.0, 2.0, 3.0]
TRACE_OPACS = [0.8, 0.8, 0.8, 1.0]
TRACE_STEP = 0.04  # coarse sampling keeps the per-frame rebuilds cheap


def _joints_xy(x: float):
    """xy of the chain joints: anchor, tip_0 (= center_1), …, tip_{N-1}."""
    pts = [CENTER[:2].copy()]
    for k, a in zip(KS, AMPS):
        pts.append(pts[-1] + np.array([a * SY * np.sin(k * x),
                                       a * SY * np.cos(k * x)]))
    return pts


def _center3(i: int, x: float):
    """3D center of circle i (joint i, lifted to the circle's plane)."""
    xy = _joints_xy(x)[i]
    return np.array([xy[0], xy[1], Z[i]])


def _tip3(i: int, x: float):
    """3D tip of circle i (joint i+1, in the circle's own plane)."""
    xy = _joints_xy(x)[i + 1]
    return np.array([xy[0], xy[1], Z[i]])


def _pen3(x: float):
    """The final pen: the last tip, dropped to the front layer z=0."""
    xy = _joints_xy(x)[N]
    return np.array([xy[0], xy[1], 0.0])


def _partial_value(n: int, x: float) -> float:
    """Deflection of the n-term partial sum."""
    return sum(a * SY * np.sin(k * x) for k, a in zip(KS[:n], AMPS[:n]))


def _trace_z(n: int) -> float:
    """Partial-sum trace n lives on its pen's layer (full sum on the front)."""
    return 0.0 if n == N else Z[n - 1]


def _trace_point(n: int, s: float):
    return np.array([CENTER[0] + _partial_value(n, s),
                     CENTER[1] - s * ST, _trace_z(n)])


class FourierSquareWave(ShortsScene):
    TITLE = ""
    HANDLE = ""
    FORMULA = None

    def construct(self):
        self.set_front_view()

        # ---- faint vertical "0" axis on the front layer --------------------- #
        axis = Line(
            np.array([0.0, CENTER[1] + 0.15, 0.0]),
            np.array([0.0, CENTER[1] - DOMAIN * ST - 0.2, 0.0]),
            color=style.MUTE, stroke_width=1.2,
        ).set_opacity(0.5)

        xt = ValueTracker(0.0)

        # ---- the machine: circles + radius arms, each on its own plane ------ #
        circles, radii = [], []
        for i, (k, a) in enumerate(zip(KS, AMPS)):
            col = style.harmonic_color(i)
            c = Circle(radius=a * SY, color=col,
                       stroke_width=2.2, stroke_opacity=0.85)
            c.add_updater(lambda m, i=i: m.move_to(_center3(i, xt.get_value())))
            circles.append(c)

            r = Line(color=col, stroke_width=2.0)
            r.add_updater(lambda m, i=i: m.put_start_and_end_on(
                _center3(i, xt.get_value()) + 1e-6, _tip3(i, xt.get_value())))
            radii.append(r)

        # ---- chain drops: tip_i falls one layer to the next center ---------- #
        # These have zero footprint in the front view (they are pure-z segments)
        # and become the vertical dashed links of the stack once the camera tilts.
        drops = []
        for i in range(N):
            d = DashedLine(color=style.MUTE, stroke_width=1.2,
                           dash_length=0.06, stroke_opacity=0.55)
            if i < N - 1:
                d.add_updater(lambda m, i=i: m.put_start_and_end_on(
                    _tip3(i, xt.get_value()), _center3(i + 1, xt.get_value())))
            else:
                d.add_updater(lambda m, i=i: m.put_start_and_end_on(
                    _tip3(i, xt.get_value()), _pen3(xt.get_value())))
            drops.append(d)

        # ---- simultaneous partial-sum traces, each on its own layer --------- #
        traces, connectors, pens = [], [], []
        for n in range(1, N + 1):
            col = TRACE_COLORS[n - 1]
            w = TRACE_WIDTHS[n - 1]
            op = TRACE_OPACS[n - 1]

            tr = ParametricFunction(
                lambda s, n=n: _trace_point(n, s),
                t_range=[0, 1e-3, TRACE_STEP], color=col, stroke_width=w,
            ).set_fill(opacity=0).set_stroke(opacity=op)
            tr.add_updater(lambda m, n=n, col=col, w=w, op=op: m.become(
                ParametricFunction(
                    lambda s: _trace_point(n, s),
                    t_range=[0, max(xt.get_value(), 1e-3), TRACE_STEP],
                    color=col, stroke_width=w,
                ).set_fill(opacity=0).set_stroke(opacity=op)))
            traces.append(tr)

            # dashed connector from the pen point to its trace front (in-plane)
            def pen_point(n=n):
                x = xt.get_value()
                return _pen3(x) if n == N else _tip3(n - 1, x)

            conn = DashedLine(color=style.MUTE, stroke_width=1.2,
                              dash_length=0.06, stroke_opacity=0.5)
            conn.add_updater(lambda m, n=n, pp=pen_point: m.put_start_and_end_on(
                pp(), _trace_point(n, xt.get_value()) + 1e-6))
            connectors.append(conn)

            p = Dot(color=col, radius=0.035)
            p.add_updater(lambda m, n=n: m.move_to(
                _trace_point(n, xt.get_value())))
            pens.append(p)

        # ---- Phase 1: flat front view — the machine draws ------------------- #
        self.play(FadeIn(axis), Create(VGroup(*circles)), run_time=1.0)
        self.add(*radii, *drops, *traces, *connectors, *pens)
        self.play(xt.animate(rate_func=linear).set_value(0.55 * DOMAIN),
                  run_time=5.5)

        # ---- Phase 2: the reveal — a pure camera move ----------------------- #
        # Nothing is shifted or faded; the tilt exposes the depth that was
        # there all along, and the machine keeps drawing through the move.
        self.move_camera(
            **self.REVEAL,
            added_anims=[xt.animate(rate_func=linear).set_value(0.75 * DOMAIN)],
            run_time=2.8,
        )

        # ---- Phase 3: orbit while the machine finishes the draw ------------- #
        self.orbit(rate=0.05)
        self.play(xt.animate(rate_func=linear).set_value(DOMAIN), run_time=3.2)
        self.wait(0.8)
        self.stop_orbit()
        self.wait(0.3)
