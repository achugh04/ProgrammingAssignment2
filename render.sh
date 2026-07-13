#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# render.sh — render a scene to a phone-ready vertical short.
#
#   ./render.sh <scene> [--fps 24|30|60] [--preview] [--open]
#
#   <scene>   short name (see SCENES below) OR  path/to/file.py:ClassName
#   --fps     frame rate: 24, 30 or 60   (default 30)
#   --preview fast low-res portrait pass (405x720) for iteration
#   --open    print the final path for convenience
#
# Output: 1080x1920 H.264 (yuv420p, +faststart) MP4 in ./out/, named
#         <scene>_<fps>fps.mp4 — directly uploadable to Instagram Reels /
#         YouTube Shorts. Always 9:16, always yuv420p.
# ---------------------------------------------------------------------------
set -euo pipefail
cd "$(dirname "$0")"

# ---- scene registry: shortname -> file:Class ------------------------------ #
declare -A SCENES=(
  [fourier]="mathviz/scenes/fourier_square.py:FourierSquareWave"
  [taylor]="mathviz/scenes/taylor_sine.py:TaylorSine"
)

FPS=30
PREVIEW=0
OPENIT=0
SCENE_KEY=""

while [ $# -gt 0 ]; do
  case "$1" in
    --fps) FPS="$2"; shift 2;;
    --preview) PREVIEW=1; shift;;
    --open) OPENIT=1; shift;;
    -h|--help) grep '^#' "$0" | sed 's/^# \{0,1\}//'; exit 0;;
    *) SCENE_KEY="$1"; shift;;
  esac
done

if [ -z "$SCENE_KEY" ]; then
  echo "usage: ./render.sh <scene> [--fps 24|30|60] [--preview]"
  echo "scenes: ${!SCENES[*]}"
  exit 1
fi

if [ "$PREVIEW" -eq 1 ]; then
  FPS=15   # previews always run at a fast, throwaway frame rate
else
  case "$FPS" in 24|30|60) ;; *) echo "!! fps must be 24, 30 or 60"; exit 1;; esac
fi

# resolve scene -> file + class
TARGET="${SCENES[$SCENE_KEY]:-$SCENE_KEY}"
FILE="${TARGET%%:*}"
CLASS="${TARGET##*:}"
[ -f "$FILE" ] || { echo "!! no such scene file: $FILE"; exit 1; }

# ---- environment ---------------------------------------------------------- #
if [ -d .venv ]; then source .venv/bin/activate; fi
export PYTHONPATH="$PWD:${PYTHONPATH:-}"

# ---- resolution ----------------------------------------------------------- #
if [ "$PREVIEW" -eq 1 ]; then RES="540,960"; TAG="preview"; else RES="1080,1920"; TAG="${FPS}fps"; fi

STAMP="${SCENE_KEY//\//_}"
RAW_NAME="${CLASS}_raw"

echo "==> rendering $CLASS @ ${RES//,/x} ${FPS}fps"
manim render -r "$RES" --fps "$FPS" --format mp4 -o "$RAW_NAME" \
  --media_dir ./media "$FILE" "$CLASS"

# manim writes to media/videos/<file-stem>/<height>p<fps>/<name>.mp4 — compute
# that exact path so we never pick up a stale preview render of the same scene.
STEM="$(basename "$FILE" .py)"
HEIGHT="${RES##*,}"
RAW="media/videos/${STEM}/${HEIGHT}p${FPS}/${RAW_NAME}.mp4"
[ -f "$RAW" ] || RAW=$(find media/videos -path "*${HEIGHT}p${FPS}*/${RAW_NAME}.mp4" | head -1)
[ -f "$RAW" ] || { echo "!! could not locate rendered file at $RAW"; exit 1; }

mkdir -p out
FINAL="out/${STAMP}_${TAG}.mp4"

# ---- normalise for social upload: yuv420p, +faststart, even dims ---------- #
echo "==> normalising -> $FINAL"
ffmpeg -y -loglevel error -i "$RAW" \
  -c:v libx264 -profile:v high -pix_fmt yuv420p -preset slow -crf 18 \
  -movflags +faststart -an "$FINAL"

echo "==> done: $FINAL"
ffprobe -v error -select_streams v:0 \
  -show_entries stream=width,height,r_frame_rate,pix_fmt \
  -of default=noprint_wrappers=1 "$FINAL" 2>/dev/null | sed 's/^/    /' || true
[ "$OPENIT" -eq 1 ] && echo "$FINAL"
