# Startup exports
export PATH=$HOME/bin:$PATH
export EDITOR=nvim
export BROWSER=firefox
export SHELL=/usr/bin/zsh
export PAGER=most
export MANPAGER='nvim +Man!'
export FZF_DEFAULT_COMMAND='fd'
export FZF_DEFAULT_OPTS='--height 60% --reverse'
export NO_AT_BRIDGE=1
export GTK_OVERLAY_SCROLLING=0
export QT_QPA_PLATFORMTHEME="kde"
# export MICRO_TRUECOLOR=1
export LYNX_LSS="$HOME/lynx.lss"
export _JAVA_AWT_WM_NONREPARENTING=1
export FREEPLANE_JAVA_HOME=/usr/lib/jvm/java-21-openjdk/
export GOPATH=$HOME/Projects/go

if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    export MOZ_ENABLE_WAYLAND=1
fi
