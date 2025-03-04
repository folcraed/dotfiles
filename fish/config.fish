set fish_greeting
starship init fish | source
set fzf_preview_dir_cmd eza --all --color=always

#================================================
# Functions
#================================================
function fish_title
    echo (status current-command) ' '
end

function bu2
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Archives/ /run/media/rob/backup/Dropbox/Archives
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Genealogy/ /run/media/rob/backup/Dropbox/Genealogy
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Mindmaps/ /run/media/rob/backup/Dropbox/Mindmaps
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Notes/ /run/media/rob/backup/Dropbox/Notes
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Docs/ /run/media/rob/backup/Dropbox/Docs
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Carroll/ /run/media/rob/backup/Dropbox/Carroll
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/.googleearth/myplaces.kml /run/media/rob/backup/.googleearth
    rsync -aiv --delete ~/.local/share/marble/bookmarks/bookmarks.kml /run/media/rob/backup/marble
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Videos/ /run/media/rob/backup/Videos
    rsync -aiv --exclude='.git/' --exclude='pkg/' ~/Projects/ /run/media/rob/backup/Projects
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/bin/ /run/media/rob/backup/bin
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments' --exclude='.git' ~/scripts/ /run/media/rob/backup/scripts
end

function ccache --description "Clears memory buffers and cache"
    sudo sh -c 'echo 1 > /proc/sys/vm/drop_caches'
end

function cdd
    cd ~/.config/dotfiles
end

function cdt
    cd ~/Temp
end

function cdn
    cd ~/Dropbox/Notes
end

function cdg
    cd ~/Dropbox/Genealogy
end

function cdm
    cd ~/Dropbox/Mindmaps
end

function cdc
    cd ~/.config
end

function cdp
    cd ~/Projects
end

function clrc
    sudo sysctl vm.drop_caches=1
end

function dfh
    df -h
end

function dui
    dua i
end

function ecn --description "Runs Emacsclient in terminal"
    emacsclient -nw
end

function eN
    nvim ~/.config/nvim/init.lua
end

function ela --description "Searches folder for file pattern"
    eza -la $argv
end

function extract --description "Expand or extract bundled & compressed files"
    set --local ext (echo $argv[1] | awk -F. '{print $NF}')
    switch $ext
        case tar # non-compressed, just bundled
            tar -xvf $argv[1]
        case gz
            if test (echo $argv[1] | awk -F. '{print $(NF-1)}') = tar # tar bundle compressed with gzip
                tar -zxvf $argv[1]
            else # single gzip
                gunzip $argv[1]
            end
        case tgz # same as tar.gz
            tar -zxvf $argv[1]
        case bz2 # tar compressed with bzip2
            tar -jxvf $argv[1]
        case rar
            unrar x $argv[1]
        case zip
            unzip $argv[1]
        case '*'
            echo "unknown extension"
    end
end

function gcam
    git commit -a
end

function gcl
    git clone $argv
end

function gp
    git push
end

function gst
    git status
end

function jvac
    sudo journalctl --vacuum-files=2
end

function la
    eza -alh $argv
end

function later
    lynx "https://forecast.weather.gov/MapClick.php?CityName=Missoula&state=MT&site=MSO&lat=46.9181&lon=-114.153&unit=0&lg=english&FcstType=text&TextType=1"
end

function lg
    eza -alh --git
end

function london
    TZ="Europe/London" date
end

function sydney
    TZ="Australia/Sydney" date
end

function athens
    TZ="Europe/Athens" date
end

function lt
    eza -alh --sort=modified
end

function mconv
    ffmpeg -f concat -safe 0 -i files.txt -c copy Time_Team.webm
end

function mpvd
    mpv dvd://
end

function npkg
    cd /var/cache/pacman/pkg
    fd --changed-within 1week -e zst
end

function now
    curl --retry-max-time 5 --retry 5 https://tgftp.nws.noaa.gov/data/observations/metar/decoded/KMSO.TXT
end

function rgs --description "Searches current folder and shows 2 lines with match"
    rg -C 2 --max-depth 1 $argv
end

function rsemacs --description "Restarts the emacs systemd daemon"
    systemctl --user restart emacs
end

function remacs --description "Restarts the emacs daemon"
    killall emacs
    sleep 2
    emacs --bg-daemon
end

function ytdl
    yt-dlp --compat-options multistreams -4 $argv
end

function pacs
    pacman -Ss $argv
end

function pacq
    pacman -Qi $argv
end

function paci
    sudo pacman -S $argv
end

function pacf
    pacman -Qo $argv
end

function pacr
    sudo pacman -Rns $argv
end

function paco
    sudo pacman -Qdt
end

function pacu
    sudo pacman -Syyu
end

function pacl
    pacman -Ql $argv
end

function pacc
    sudo pacman -Sc
end

function pacm
    sudo pacman-mirrors -c United_States,Canada
end

function pamu
    pamac update
end

function pams
    pamac search $argv
end

function pami
    pamac install $argv
end

function cleanl
    sudo cp -r /usr/share/locale/en_US /tmp/ &&
        sudo rm -r /usr/share/locale &&
        sudo mkdir /usr/share/locale &&
        sudo cp -r /tmp/en_US /usr/share/locale/
end

function icat
    kitty +kitten icat
end

function slt
    systemctl list-timers
end

function rf
    recoll -t -a $argv
end

function gitclean
    for file in (cat .gitignore)
        git rm -r --cached $file
    end
end
