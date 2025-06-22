# Only run xrdb if DISPLAY is available and X server is running
if [ -n "$DISPLAY" ] && xset q >/dev/null 2>&1; then
    xrdb -merge ~/.Xresources 2>/dev/null
fi
