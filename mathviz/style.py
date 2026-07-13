"""
mathviz.style — the shared visual language for every short.

One palette, one type system, one set of card/branding builders so that every
video in this repo reads as the same channel. Tuned to match the reference
"PhysAnim"-style look: near-black canvas, luminous cyan/gold/white strokes,
a quiet serif title and a bordered formula card.

Import these constants and helpers from scene files; never hard-code a colour.
"""
from __future__ import annotations

from manim import (
    BOLD,
    DOWN,
    UP,
    LEFT,
    RIGHT,
    ORIGIN,
    ManimColor,
    Text,
    MathTex,
    RoundedRectangle,
    VGroup,
)

# --------------------------------------------------------------------------- #
# Palette                                                                      #
# --------------------------------------------------------------------------- #
BG = ManimColor("#0d0d12")          # canvas — must match manim.cfg background_color

CYAN = ManimColor("#35d0e0")        # fundamental harmonic / primary curve
GOLD = ManimColor("#f2c14e")        # secondary harmonics
ORANGE = ManimColor("#e8833a")      # a warm partial-sum
WHITE = ManimColor("#f4f5f7")       # the combined / final curve
VIOLET = ManimColor("#8a8ad6")      # axes & structural lines
GREEN = ManimColor("#7ad19a")       # occasional extra harmonic
MUTE = ManimColor("#6b6b8a")        # dim guides, tick labels, projection lines

# Ordered wheel handed out to successive harmonics / series terms.
HARMONIC_COLORS = [CYAN, GOLD, WHITE, ORANGE, GREEN, VIOLET]


def harmonic_color(i: int) -> ManimColor:
    """Stable colour for the i-th harmonic/term (0-indexed)."""
    return HARMONIC_COLORS[i % len(HARMONIC_COLORS)]


# --------------------------------------------------------------------------- #
# Type                                                                         #
# --------------------------------------------------------------------------- #
# "serif" is a Pango generic alias that resolves to an installed serif face
# (DejaVu Serif on the render box), so this never fails for a missing font.
TITLE_FONT = "serif"

TITLE_SIZE = 40
HANDLE_SIZE = 20


def title_text(text: str) -> Text:
    """Quiet, wide serif title in the reference style (semi-transparent white)."""
    t = Text(text.upper(), font=TITLE_FONT, weight=BOLD, color=WHITE)
    t.set_opacity(0.82)
    # Gentle letter tracking for the airy look of the reference.
    t.scale(TITLE_SIZE / 48)
    return t


def handle_text(text: str) -> Text:
    return Text(text, font=TITLE_FONT, color=MUTE).scale(HANDLE_SIZE / 48)


# --------------------------------------------------------------------------- #
# Formula card                                                                 #
# --------------------------------------------------------------------------- #
def formula_card(tex: str, width: float = 3.9, tex_scale: float = 0.62) -> VGroup:
    """
    A bordered, translucent card holding a LaTeX formula — the caption block
    pinned to the bottom of the reference video.

    `tex` is raw LaTeX (no surrounding $). Returns a VGroup(card, formula).
    Caller is responsible for positioning + add_fixed_in_frame_mobjects().
    """
    formula = MathTex(tex, color=WHITE).scale(tex_scale)
    formula.set_max_width(width - 0.5)

    card = RoundedRectangle(
        corner_radius=0.18,
        width=width,
        height=formula.height + 0.55,
        stroke_color=CYAN,
        stroke_width=2.0,
        fill_color=BG,
        fill_opacity=0.55,
    )
    formula.move_to(card.get_center())
    return VGroup(card, formula)
