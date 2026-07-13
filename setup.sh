#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# setup.sh — bootstrap the rendering toolchain for
#            "Interesting Math Series / Equations Visuals".
#
# Safe to re-run. Installs system deps (ffmpeg, cairo/pango, a LaTeX subset)
# then creates a local .venv with manim. Designed for a fresh Ubuntu 24.04
# container (Claude Code on the web) but works on any Debian/Ubuntu box.
# ---------------------------------------------------------------------------
set -euo pipefail
cd "$(dirname "$0")"

echo "==> [1/3] system packages (ffmpeg, cairo, pango, LaTeX subset)"
if command -v apt-get >/dev/null 2>&1; then
  export DEBIAN_FRONTEND=noninteractive
  # '|| true' — a fresh container may have a broken third-party PPA; core repos still work.
  sudo apt-get update -qq || apt-get update -qq || true
  PKGS="ffmpeg build-essential pkg-config python3-dev python3-venv \
        libcairo2-dev libpango1.0-dev dvisvgm \
        texlive-latex-base texlive-latex-extra texlive-latex-recommended \
        texlive-fonts-recommended texlive-science"
  # Try with sudo, fall back to bare apt-get (container often runs as root).
  sudo apt-get install -y --no-install-recommends $PKGS \
    || apt-get install -y --no-install-recommends $PKGS
else
  echo "    !! apt-get not found. Install ffmpeg, cairo, pango, dvisvgm and a"
  echo "       LaTeX distribution (with amsmath) manually, then re-run."
fi

echo "==> [2/3] python virtualenv (.venv)"
if [ ! -d .venv ]; then
  python3 -m venv .venv
fi
# shellcheck disable=SC1091
source .venv/bin/activate
pip install --upgrade pip setuptools wheel >/dev/null

echo "==> [3/3] python deps (manim, numpy, scipy)"
pip install -r requirements.txt

echo ""
echo "==> Verifying..."
python - <<'PY'
import manim, numpy, scipy
print(f"    manim {manim.__version__}  numpy {numpy.__version__}  scipy {scipy.__version__}")
PY
ffmpeg -version | head -1 | sed 's/^/    /'
echo "    latex: $(command -v latex || echo 'MISSING — MathTex formula cards will fail')"
echo ""
echo "Done. Activate with:  source .venv/bin/activate"
echo "Render a sample with: ./render.sh fourier --fps 30"
