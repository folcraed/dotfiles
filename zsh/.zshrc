## Set typical completion stuff
zstyle ':completion:*' completer _complete _ignored _approximate
zstyle ':completion:*' list-colors "=(#b)"
zstyle ':completion:*' matcher-list '' '' '' ''
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/rob/.zshrc'

autoload -Uz compinit promptinit colors zmv
compinit -d
colors
setopt appendhistory autocd beep notify

## Use Emacs bindings and motions, not Vi
bindkey -e

## Set some sane key bindings (should be default in my opinion)
typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"
key[Control-Left]="${terminfo[kLFT5]}"
key[Control-Right]="${terminfo[kRIT5]}"

# setup key accordingly
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete
[[ -n "${key[Control-Left]}"  ]] && bindkey -- "${key[Control-Left]}"  backward-word
[[ -n "${key[Control-Right]}" ]] && bindkey -- "${key[Control-Right]}" forward-word

## Finally, make sure the terminal is in application mode, when zle is
## active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
	autoload -Uz add-zle-hook-widget
	function zle_application_mode_start { echoti smkx }
	function zle_application_mode_stop { echoti rmkx }
	add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
	add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

## Add some plugins for easier and prettier use
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh
source $HOME/.zsh/zsh-extract/extract.plugin.zsh

## -- Alias config --
alias al='cat ~/.zshrc | grep alias'
alias Z='source ~/.zshrc && clear'
alias vim='nvim'
alias bs='baloosearch $s'
alias bu='rsync -aiv --exclude=".comments/" ~/Dropbox/ /run/media/rob/Backups'
alias bu2='~/scripts/bu2m.sh'
alias cdb='cd ~/Dropbox'
alias cdt='cd ~/Temp'
alias cdD='cd ~/Downloads'
alias cdu='cd /usr/share'
alias cds='cd ~/Dropbox/Settings'
alias cdd='cd ~/.config/dotfiles'
alias cdg='cd ~/Dropbox/Genealogy'
alias cdm='cd ~/Dropbox/Mindmaps'
alias cdp='cd ~/projects'
alias cdc='cd ~/.config'
alias cdn='cd ~/Dropbox/Notes'
alias convtemp='node /home/rob/code/learning/convert-temp.js'
alias ddg='ddgr -x $s'
alias ll='exa -alFh'
alias fzy='fzf -e --bind "ctrl-y:execute-silent(echo {} | xclip)+abort"'
alias fzb='fzf -e --preview="bat --color=always {}"'
alias la='exa -alh'
alias lr='exa -alh --sort=newest -r'
alias lg='exa -alh --git'
alias lsg='exa -alh | grep $s'
alias lt='exa -alh --sort=modified'
alias grep='grep --color=auto'
alias so='xset s off && xset -dpms'
alias sx='xset s off && xset dpms 0 1800 0'
alias ytdl='yt-dlp --compat-options multistreams $s'
alias wV='cd /mnt/rob/Videos && ll'
alias eV='vim ~/.vim/vimrc'
alias eA='cd /home/rob/.config/awesome && vim rc.lua'
alias eI='cd /home/rob/.i3 && vim config'
alias eZ='nvim ~/.zshrc'
alias eN='nvim ~/.config/nvim/init.lua'
alias pacs='pacman -Ss $s'
alias pacq='pacman -Qi $s'
alias paci='sudo pacman -S $s'
alias pacr='sudo pacman -Rns $s'
alias paco='sudo pacman -Qdt'
alias pacu='sudo pacman -Syyu'
alias pacl='pacman -Ql $s'
alias pacc='sudo pacman -Sc'
alias pacm='sudo pacman-mirrors -c United_States,Canada -aP https'
alias pamu='pamac update'
alias pams='pamac search $s'
alias pami='pamac install $s'
alias jvac='sudo journalctl --vacuum-files=2'
alias eX='nvim ~/.Xresources'
alias xup='xrdb -merge ~/.Xresources'
alias london='TZ="Europe/London" date'
alias sydney='TZ="Australia/Sydney" date'
alias now='curl https://tgftp.nws.noaa.gov/data/observations/metar/decoded/KMSO.TXT'
alias later='lynx "https://forecast.weather.gov/MapClick.php?CityName=Missoula&state=MT&site=MSO&lat=46.9181&lon=-114.153&unit=0&lg=english&FcstType=text&TextType=1"'
alias dop='falkon "https://radar.weather.gov/lite/N0R/MSX_loop.gif"'
alias ports='sudo netstat -tulanp'
alias free='free -th'
alias dfh='df -h'
alias xo='xdg-open $f'
alias ts='tracker search -l 100 $s'
alias rgl='rg -a -l $s'
alias rgs='rg -C 2 $s'
alias slt='systemctl list-timers'
alias rect='recoll -t -a $s'

# Some alias' for git
alias ga='git add'
alias gst='git status'
alias gcam='git commit -a'
alias gd='git diff'
alias gp='git push'
alias gdh='git diff HEAD^ HEAD' #View last changes committed
alias gup='git pull'
alias gcl='git clone'

## Functions
cda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}


fo() {
  IFS=$'\n' files=($(fzf --query="$1" --multi --select-1 --exit-0))
  [[ -n "$files" ]] && xdg-open "${files[@]}"
}

# Simple calculator
function calc() {
        local result=""
        result="$(printf "scale=10;$*\n" | bc --mathlib | tr -d '\\\n')"
        #                       └─ default (when `--mathlib` is used) is 20
        #
        if [[ "$result" == *.* ]]; then
                # improve the output for decimal numbers
                printf "$result" |
                sed -e 's/^\./0./'        `# add "0" for cases like ".5"` \
                    -e 's/^-\./-0./'      `# add "0" for cases like "-.5"`\
                    -e 's/0*$//;s/\.$//'   # remove trailing zeros
        else
                printf "$result"
        fi
        printf "\n"
}
# Copied from https://github.com/addyosmani/dotfiles/.functions

## Initialize Starship prompt
eval "$(starship init zsh)"
