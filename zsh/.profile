export _JAVA_AWT_WM_NONREPARENTING=1
export FREEPLANE_JAVA_HOME=/usr/lib/jvm/jre-jetbrains/
export EDITOR=nvim
export QTWEBENGINE_FORCE_USE_GBM=0

if [ "$XDG_SESSION_DESKTOP" = "sway" ]; then
    export QT_QPA_PLATFORM=wayland
fi
if [ "$XDG_SESSION_DESKTOP" = "sway" ]; then
    export QT_AUTO_SCREEN_SCALE_FACTOR=1
fi
if [ "$XDG_SESSION_DESKTOP" = "sway" ]; then
    export QT_QPA_PLATFORMTHEME=kde
fi
