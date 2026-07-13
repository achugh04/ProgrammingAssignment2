"""
Taylor / Maclaurin series of sin x — 3D-native, no text, nothing staged.

Every partial sum is AUTHORED at its own depth from the first frame:
depth encodes approximation order — the crudest (degree-1 line) sits deepest,
each better approximation one layer closer, and the best one lives on the
front layer (z=0) together with the faint true sin it hugs. Head-on the
camera's orthographic front view collapses the stack, so the opening reads
as the classic flat convergence picture with every curve drawing
SIMULTANEOUSLY; the tilt is then a pure camera move that just sees the
dimension that was always there — the approximations separate into a
receding staircase of convergence, held with a slow orbit.

Render:
    ./render.sh taylor --fps 60
    manim -qh mathviz/scenes/taylor_sine.py TaylorSine
"""
from __future__ import annotations

import math
import numpy as np
from manim import (
    Line,
    ParametricFunction,
    FadeIn,
    Create,
)

from mathviz import ShortsScene, style

PI = np.pi
N_TERMS = 4                         # degrees 1, 3, 5, 7
SX, SY = 0.64, 0.72                # math -> world scale
ORG = np.array([0.0, 0.1, 0.0])    # world position of the math origin
DZ = 0.58                           # depth between approximation layers

# Partial sum m (m = 0 crudest) lives at depth: worse approximation, deeper.
Z = [(N_TERMS - 1 - m) * DZ for m in range(N_TERMS)]


def _term(n: int, x: float) -> float:
    """n-th Maclaurin term of sin: (-1)^n x^(2n+1) / (2n+1)!."""
    return (-1) ** n * x ** (2 * n + 1) / math.factorial(2 * n + 1)


def _partial(m: int, x: float) -> float:
    """Sum of terms 0..m."""
    return sum(_term(n, x) for n in range(m + 1))


def _map(x: float, val: float, z: float = 0.0):
    return ORG + np.array([SX * x, SY * val, z])


class TaylorSine(ShortsScene):
    TITLE = ""
    HANDLE = ""
    FORMULA = None

    def construct(self):
        self.set_front_view()

        # ---- axes (faint, unlabeled) on the front layer ---------------------- #
        x_axis = Line(_map(-PI, 0), _map(PI, 0),
                      color=style.MUTE, stroke_width=1.4).set_opacity(0.6)
        y_axis = Line(_map(0, -2.1), _map(0, 2.1),
                      color=style.MUTE, stroke_width=1.4).set_opacity(0.4)

        # ---- true sin, faint reference, on the front layer ------------------- #
        true_sin = ParametricFunction(
            lambda x: _map(x, math.sin(x)), t_range=[-PI, PI, 0.03],
            color=style.WHITE, stroke_width=2.0,
        ).set_fill(opacity=0).set_stroke(opacity=0.28)

        self.play(FadeIn(x_axis), FadeIn(y_axis), Create(true_sin), run_time=1.0)

        # ---- every partial sum, already at its own depth --------------------- #
        # Best approximation (front layer) is white, per the house rule that
        # white marks the final/most-complete curve.
        colors = [style.CYAN, style.GOLD, style.ORANGE, style.WHITE]
        partials = []
        for m in range(N_TERMS):
            col = colors[m % len(colors)]
            # low orders diverge near the edges; clip domain so it stays on-screen
            span = 2.6 + 0.15 * m
            p = ParametricFunction(
                lambda x, m=m: _map(x, _partial(m, x), Z[m]),
                t_range=[-span, span, 0.02],
                color=col, stroke_width=3.0,
            ).set_fill(opacity=0).set_stroke(opacity=0.95)
            partials.append(p)

        # one shared play => every approximation builds simultaneously; the
        # front view collapses the depth so it reads as the flat 2D picture
        self.play(*[Create(p) for p in partials], run_time=2.8)
        self.wait(0.4)

        # ---- the reveal: a pure camera move ---------------------------------- #
        # Nothing shifts, nothing fades — the tilt exposes the depth stacking
        # (crudest approximation deepest, best one hugging sin up front).
        self.reveal_3d(run_time=2.6)

        # ---- cinematic orbit hold -------------------------------------------- #
        self.orbit(rate=0.05)
        self.wait(3.4)
        self.stop_orbit()
        self.wait(0.4)
