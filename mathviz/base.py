"""
mathviz.base — ShortsScene, the reusable spine every video is built on.

It owns three things the reference channel is recognisable for:

  1. Vertical 1080x1920 framing with a quiet serif title pinned top and a
     bordered LaTeX formula card pinned bottom (both fixed in frame, so they
     stay flat while the world rotates).
  2. The two canonical camera orientations — a flat FRONT view and the tilted
     REVEAL view — and the animated move between them (the "…this turns 3D"
     beat).
  3. A house palette (via mathviz.style).

Depth convention: the world is authored in the xy-plane (what you see in the
FRONT view). To "explode" components into 3D, offset them along +z. In the
front view a +z offset is invisible (things overlap / nest); after the reveal
tilt it reads as a receding vertical stack — exactly the reference's move.
"""
from __future__ import annotations

from manim import (
    DEGREES,
    DOWN,
    UP,
    ThreeDScene,
    VGroup,
    config,
)

from . import style


class ShortsScene(ThreeDScene):
    # ---- per-video copy (override in subclasses) -------------------------- #
    TITLE: str = ""
    HANDLE: str = "@mathviz"
    FORMULA: str | None = None          # raw LaTeX for the bottom card, or None
    FORMULA_SCALE: float = 0.62
    FORMULA_WIDTH: float = 3.9

    # ---- canonical camera orientations ------------------------------------ #
    # focal_distance is huge => effectively orthographic projection. Scenes are
    # authored 3D-native (components at their true z from frame one); with an
    # orthographic camera, depth has exactly zero footprint in the front view,
    # so the flat phase reads as perfect 2D and the tilt reveals the stack
    # purely through rotation.
    FRONT = dict(phi=0 * DEGREES, theta=-90 * DEGREES, focal_distance=1000)
    REVEAL = dict(phi=64 * DEGREES, theta=-105 * DEGREES, focal_distance=1000)

    # --------------------------------------------------------------------- #
    # Overlay: title + handle + formula card, all fixed in the camera frame #
    # --------------------------------------------------------------------- #
    def build_overlay(self) -> None:
        overlay = []

        if self.TITLE:
            self._title = style.title_text(self.TITLE)
            # Keep the title inside the 9:16 column (frame width ~4.5 world units).
            max_w = config.frame_width - 0.5
            if self._title.width > max_w:
                self._title.set(width=max_w)
            self._title.to_edge(UP, buff=0.5)
            overlay.append(self._title)

        if self.HANDLE:
            self._handle = style.handle_text(self.HANDLE)
            self._handle.to_edge(DOWN, buff=0.28)
            overlay.append(self._handle)

        if self.FORMULA:
            self._card = style.formula_card(
                self.FORMULA, width=self.FORMULA_WIDTH, tex_scale=self.FORMULA_SCALE
            )
            # Sit just above the handle.
            self._card.to_edge(DOWN, buff=0.62)
            overlay.append(self._card)

        if overlay:
            # Fixed in frame => stays a flat 2D overlay through every camera move.
            self.add_fixed_in_frame_mobjects(*overlay)
            self._overlay = VGroup(*overlay)

    # --------------------------------------------------------------------- #
    # Camera                                                                 #
    # --------------------------------------------------------------------- #
    def set_front_view(self) -> None:
        """Snap to the flat front view (call in setup, before animating)."""
        self.set_camera_orientation(**self.FRONT)

    def reveal_3d(self, added_anims=None, run_time: float = 2.6, **overrides):
        """
        The signature beat: tilt from the flat FRONT view into the REVEAL view.
        Pass component 'explode' animations via `added_anims` so the depth
        separation happens *as* the camera swings.
        """
        target = {**self.REVEAL, **overrides}
        self.move_camera(
            **target,
            added_anims=added_anims or [],
            run_time=run_time,
        )

    def orbit(self, rate: float = 0.045):
        """Gentle continuous azimuth drift for the held 3D shot."""
        self.begin_ambient_camera_rotation(rate=rate)

    def stop_orbit(self):
        self.stop_ambient_camera_rotation()
