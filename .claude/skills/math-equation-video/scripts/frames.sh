#!/usr/bin/env bash
# frames.sh — extract inspection JPEGs from a rendered short so you can eyeball it.
#
#   frames.sh <video.mp4> [count]
#
# Spreads `count` frames (default 8) evenly across the clip, writes them to a temp
# dir, and prints the paths. Open several (early 2D / the tilt / late 3D) and check
# for clipping, off-screen curves, filled-in strokes, and a legible 3D hold.
set -euo pipefail

VID="${1:?usage: frames.sh <video.mp4> [count]}"
COUNT="${2:-8}"
[ -f "$VID" ] || { echo "no such file: $VID" >&2; exit 1; }

# Prefer system ffmpeg/ffprobe; fall back to the pip-installed static ffmpeg.
FF="$(command -v ffmpeg || true)"
FP="$(command -v ffprobe || true)"
if [ -z "$FF" ]; then
  FF="$(python3 -c 'import imageio_ffmpeg; print(imageio_ffmpeg.get_ffmpeg_exe())' 2>/dev/null || true)"
fi
[ -n "$FF" ] || { echo "ffmpeg not found (run ./setup.sh)" >&2; exit 1; }

# Duration (seconds). ffprobe if available, else assume 15s.
DUR=15
if [ -n "$FP" ]; then
  DUR="$("$FP" -v error -show_entries format=duration -of csv=p=0 "$VID" 2>/dev/null || echo 15)"
fi

OUT="$(mktemp -d /tmp/mathviz_frames.XXXXXX)"
echo "frames -> $OUT  (duration ${DUR}s)"
n=0
python3 - "$COUNT" "$DUR" <<'PY' | while read -r t; do
import sys
count, dur = int(sys.argv[1]), float(sys.argv[2])
for i in range(count):
    # avoid the very first/last frame; spread across the middle
    print(round(dur * (i + 0.5) / count, 2))
PY
  n=$((n + 1))
  idx=$(printf '%02d' "$n")
  "$FF" -nostdin -ss "$t" -i "$VID" -frames:v 1 -q:v 3 "$OUT/f_${idx}_t${t}.jpg" -hide_banner -loglevel error
  echo "$OUT/f_${idx}_t${t}.jpg"
done
