export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk/
export FREEPLANE_JAVA_HOME=/usr/lib/jvm/java-21-openjdk/
export EDITOR=nvim
export VDPAU_DRIVER=nvidia
export MOZ_DISABLE_RDD_SANDBOX=1
export LIBVA_DRIVER_NAME=nvidia
export CUDA_DISABLE_PERF_BOOST=1
# export QTWEBENGINE_FORCE_USE_GBM=0

if [ "$XDG_SESSION_DESKTOP" = "sway;wlroots" ]; then
    export QT_QPA_PLATFORM=wayland
    export QT_AUTO_SCREEN_SCALE_FACTOR=0
    export QT_QPA_PLATFORMTHEME=kde
    export QT_WAYLAND_RECONNECT=1
fi
