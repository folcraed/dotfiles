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
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt INC_APPEND_HISTORY

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
alias al='bat ~/.zshrc | grep alias'
alias ccache='sudo sh -c "echo 1 > /proc/sys/vm/drop_caches"'
alias cdb='cd ~/Dropbox'
alias cdt='cd ~/Temp'
alias cdD='cd ~/Downloads'
alias cdu='cd /usr/share'
alias cdd='cd ~/.config/dotfiles'
alias cdg='cd ~/Dropbox/Genealogy'
alias cdm='cd ~/Dropbox/Mindmaps'
alias cdp='cd ~/Projects'
alias cdc='cd ~/.config'
alias cdn='cd ~/Dropbox/Notes'
alias clrc='sudo sysctl vm.drop_caches=1'
alias dfh='df -h'
alias dui='dua i'
alias enw='emacs -nw' # Open Emacs in terminal
alias eN='nvim ~/.config/nvim/init.lua'
alias eZ='nvim ~/.zshrc'
alias free='free -th'
alias fzb='fzf -e --preview="bat --color=always {}"'
alias grep='grep --color=auto'
alias jvac='sudo journalctl --vacuum-files=2'
alias later='lynx "https://forecast.weather.gov/MapClick.php?CityName=Missoula&state=MT&site=MSO&lat=46.9181&lon=-114.153&unit=0&lg=english&FcstType=text&TextType=1"'
alias la='eza -alh $s'
alias lr='eza -alh --sort=newest -r' # Sorts with newest at top of list
alias lg='eza -alh --git'
alias lsg='eza -alh | grep $s'
alias lo='eza -alh --sort=newest' # Sorts with newest at bottom of list
alias london='TZ="Europe/London" date'
alias sydney='TZ="Australia/Sydney" date'
alias athens='TZ="Europe/Athens" date'
alias mconv='ffmpeg -f concat -safe 0 -i files.txt -c copy Time_Team.webm'
alias mpvd='mpv dvd://'
alias now='curl https://tgftp.nws.noaa.gov/data/observations/metar/decoded/KMSO.TXT'
alias pacs='pacman -Ss $s'
alias pacq='pacman -Qi $s'
alias pacf='pacman -Qo $s'
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
alias rf='recoll -t -a $s'
alias rgs='rg -C 2 $s'
alias slt='systemctl list-timers'
alias so='xset s off && xset -dpms'
alias sx='xset s off && xset dpms 0 1800 0'
alias vim='nvim'
alias xo='xdg-open $f'
alias ytdl='yt-dlp --compat-options multistreams -4 $s'
alias Z='source ~/.zshrc && clear'

# Some aliases for git
alias ga='git add'
alias gst='git status'
alias gcam='git commit -a'
alias gd='git diff'
alias gp='git push'
alias gdh='git diff HEAD^ HEAD' #View last changes committed
alias gup='git pull'
alias gcl='git clone'

## Functions
# Removes files in git cache when they cause problems
function gitclean() {
  for file in `cat .gitignore` ; do
    git rm -r --cached $file; done
}

# Simple calculator
# Copied from https://github.com/addyosmani/dotfiles/.functions
function calc() {
  local result=""
  result="$(printf "scale=10;$*\n" | bc --mathlib | tr -d '\\\n')"
  #                       └─ default (when `--mathlib` is used) is 20
  #
  if [[ "$result" == *.* ]]; then
    # improve the output for decimal numbers
    printf "$result" |
    sed -e 's/^\./0./'        # add "0" for cases like ".5"
        -e 's/^-\./-0./'      # add "0" for cases like "-.5"
        -e 's/0*$//;s/\.$//'  # remove trailing zeros
  else
    printf "$result"
  fi
  printf "\n"
}

# Shows what packages have been updated in the last week.
function npkg() {
  cd /var/cache/pacman/pkg
  fd --changed-within 1week -e zst
  cd ~/
}

# Backs up files with rsync to external drive
function bu2() {
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Archives/ /run/media/rob/backup/Dropbox/Archives
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Genealogy/ /run/media/rob/backup/Dropbox/Genealogy
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Mindmaps/ /run/media/rob/backup/Dropbox/Mindmaps
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Notes/ /run/media/rob/backup/Dropbox/Notes
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Docs/ /run/media/rob/backup/Dropbox/Docs
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Carroll/ /run/media/rob/backup/Dropbox/Carroll
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/.googleearth/myplaces.kml /run/media/rob/backup/.googleearth
  rsync -aiv --delete ~/.local/share/marble/bookmarks/bookmarks.kml /run/media/rob/backup/marble
  rsync -aiv --delete ~/.local/share/gramps/ /run/media/rob/backup/locals
  rsync -aiv --delete ~/.config/gramps/gramps60/ /run/media/rob/backup/configs/gramps60
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Videos/ /run/media/rob/backup/Videos
  rsync -aiv --exclude='.git/' --exclude='pkg/' ~/Projects/ /run/media/rob/backup/Projects
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/bin/ /run/media/rob/backup/bin
  rsync -aiv --delete --exclude='.dtrash' --exclude='.comments' --exclude='.git/' ~/scripts/ /run/media/rob/backup/scripts
}

## Initialize Starship prompt
eval "$(starship init zsh)"
