#!/bin/sh
# tmux alert-bell hook から呼び出される bell サウンド再生スクリプト。
#
# 経緯: Ghostty 1.3.1 で bell-features = audio が silent NoOp になる現象
#       (BEL 受信は確認できるが AVAudio が呼ばれない) を回避するため、
#       tmux 側で alert-bell hook を捕捉して直接 OS の audio コマンドで
#       再生する経路を用意した。
#
# 設計:
#   - グレースフル劣化: 利用可能な audio コマンドを優先度順に試す
#   - クロスプラットフォーム: macOS / Linux / WSL / FreeBSD で動作
#   - 音源も OS 固有のシステムサウンドを優先採用
#   - 環境変数 TMUX_BELL_SOUND で任意のパス上書き可能
#   - 全て失敗した場合は silent (no-op) で終了

set -u

play_macos() {
  command -v afplay >/dev/null 2>&1 || return 1
  for f in \
    "${TMUX_BELL_SOUND:-}" \
    "/System/Library/Sounds/Funk.aiff" \
    "/System/Library/Sounds/Hero.aiff" \
    "/System/Library/Sounds/Glass.aiff"
  do
    [ -n "$f" ] && [ -r "$f" ] && exec afplay "$f"
  done
  return 1
}

play_linux() {
  if command -v paplay >/dev/null 2>&1; then
    for f in \
      "${TMUX_BELL_SOUND:-}" \
      "/usr/share/sounds/freedesktop/stereo/bell.oga" \
      "/usr/share/sounds/freedesktop/stereo/message.oga" \
      "/usr/share/sounds/alsa/Front_Center.wav"
    do
      [ -n "$f" ] && [ -r "$f" ] && exec paplay "$f"
    done
  fi
  if command -v aplay >/dev/null 2>&1; then
    f="${TMUX_BELL_SOUND:-/usr/share/sounds/alsa/Front_Center.wav}"
    [ -r "$f" ] && exec aplay -q "$f"
  fi
  if command -v canberra-gtk-play >/dev/null 2>&1; then
    exec canberra-gtk-play --id="bell"
  fi
  return 1
}

play_wsl() {
  command -v powershell.exe >/dev/null 2>&1 || return 1
  exec powershell.exe -NoProfile -Command \
    "[System.Media.SystemSounds]::Beep.Play()" >/dev/null 2>&1
}

case "$(uname)" in
  Darwin)
    play_macos
    ;;
  Linux)
    if [ -n "${WSL_DISTRO_NAME:-}" ] || grep -qi microsoft /proc/version 2>/dev/null; then
      play_wsl || play_linux
    else
      play_linux
    fi
    ;;
  FreeBSD)
    command -v aucat >/dev/null 2>&1 && [ -n "${TMUX_BELL_SOUND:-}" ] \
      && exec aucat -i "$TMUX_BELL_SOUND"
    ;;
esac

exit 0
