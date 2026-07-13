"""
Taylor / Maclaurin series of sin x — a second template proving the grammar
generalises: draw the convergence flat (front view), then "turn 3D" by
exploding each polynomial term onto its own depth layer that stack to the sum.

Render:
    ./render.sh taylor --fps 30
    manim -qh mathviz/scenes/taylor_sine.py TaylorSine
"""
from __future__ import annotations

import math
import numpy as np
from manim import (
    Line,
    Dot,
    ParametricFunction,
    VGroup,
    FadeIn,
    Create,
    OUT,
    linear,
)

from mathviz import ShortsScene, style

PI = np.pi
N_TERMS = 4                         # degrees 1, 3, 5, 7
SX, SY = 0.60, 0.62                # math -> world scale
ORG = np.array([0.0, 0.35, 0.0])   # world position of the math origin
DZ = 0.58                           # depth between term layers


def _term(n: int, x: float) -> float:
    """n-th Maclaurin term of sin: (-1)^n x^(2n+1) / (2n+1)!."""
    return (-1) ** n * x ** (2 * n + 1) / math.factorial(2 * n + 1)


def _partial(m: int, x: float) -> float:
    """Sum of terms 0..m."""
    return sum(_term(n, x) for n in range(m + 1))


def _map(x: float, val: float, z: float = 0.0):
    return ORG + np.array([SX * x, SY * val, z])


class TaylorSine(ShortsScene):
    TITLE = "Taylor Series"
    HANDLE = "@mathviz"
    FORMULA = r"\sin x=\sum_{n=0}^{\infty}\dfrac{(-1)^{n}\,x^{2n+1}}{(2n+1)!}"
    FORMULA_SCALE = 0.62

    def construct(self):
        self.set_front_view()
        self.build_overlay()

        # ---- axes ---------------------------------------------------------- #
        x_axis = Line(_map(-PI, 0), _map(PI, 0),
                      color=style.MUTE, stroke_width=1.4).set_opacity(0.6)
        y_axis = Line(_map(0, -2.1), _map(0, 2.1),
                      color=style.MUTE, stroke_width=1.4).set_opacity(0.4)

        # ---- true sin, faint reference ------------------------------------- #
        true_sin = ParametricFunction(
            lambda x: _map(x, math.sin(x)), t_range=[-PI, PI, 0.03],
            color=style.WHITE, stroke_width=2.0,
        ).set_fill(opacity=0).set_stroke(opacity=0.28)

        self.play(FadeIn(x_axis), FadeIn(y_axis), Create(true_sin), run_time=1.0)

        # ---- Phase 1: build partial sums, low -> high degree --------------- #
        partials = []
        for m in range(N_TERMS):
            col = style.harmonic_color(m)
            # low orders diverge near the edges; clip domain so it stays on-screen
            span = 2.6 + 0.15 * m
            p = ParametricFunction(
                lambda x, m=m: _map(x, _partial(m, x)),
                t_range=[-span, span, 0.02],
                color=col, stroke_width=3.0,
            ).set_fill(opacity=0).set_stroke(opacity=0.95)
            partials.append(p)
            self.play(Create(p), run_time=0.9)
            self.wait(0.15)

        self.wait(0.4)

        # ---- Phase 2: explode each term onto its own depth layer ----------- #
        term_curves, proj = [], []
        for n in range(N_TERMS):
            z = (n + 1) * DZ
            tc = ParametricFunction(
                lambda x, n=n, z=z: _map(x, _term(n, x), z),
                t_range=[-2.0, 2.0, 0.02],
                color=style.harmonic_color(n), stroke_width=2.4,
            ).set_fill(opacity=0).set_stroke(opacity=0.9)
            term_curves.append(tc)
            proj.append(Line(_map(0, 0, 0), _map(0, 0, z),
                             color=style.MUTE, stroke_width=1.0).set_opacity(0.4))

        # fade the intermediate partials, keep the best one (white) at z=0
        best = partials[-1].copy().set_color(style.WHITE)
        self.reveal_3d(
            added_anims=[p.animate.set_stroke(opacity=0.15) for p in partials[:-1]]
            + [partials[-1].animate.set_color(style.WHITE)],
            run_time=2.6,
        )
        self.play(
            *[Create(tc) for tc in term_curves],
            *[Create(pr) for pr in proj],
            run_time=1.6,
        )

        self.orbit(rate=0.05)
        self.wait(3.0)
        self.stop_orbit()
        self.wait(0.4)
