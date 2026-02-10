set fish_greeting
starship init fish | source
set fzf_preview_dir_cmd eza --all --color=always

#================================================
# Functions
#================================================
function fish_title
    echo (status current-command) ' '
end

function bdbu
    echo 'Backing up Home...'
    rsync -aiv --exclude='.cache/' --exclude='.var/' ~/ /run/media/bigdrive
end

function bu2
    echo 'Backing up Archives...'
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Archives/ /run/media/rob/backup/Dropbox/Archives
    echo 'Backing up Genealogy...'
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Genealogy/ /run/media/rob/backup/Dropbox/Genealogy
    echo 'Backing up Mindmaps...'
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Mindmaps/ /run/media/rob/backup/Dropbox/Mindmaps
    echo 'Backing up Notes...'
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Notes/ /run/media/rob/backup/Dropbox/Notes
    echo 'Backing up Docs...'
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Docs/ /run/media/rob/backup/Dropbox/Docs
    echo 'Backing up Carroll...'
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Carroll/ /run/media/rob/backup/Dropbox/Carroll
    echo 'Backing up Marble...'
    rsync -aiv --delete ~/.local/share/marble/bookmarks/bookmarks.kml /run/media/rob/backup/marble
    echo 'Backing up Gramps Locals...'
    rsync -aiv --delete ~/.local/share/gramps/ /run/media/rob/backup/locals
    echo 'Backing up Gramps Configs...'
    rsync -aiv --delete ~/.config/gramps/gramps60/ /run/media/rob/backup/configs/gramps60
    echo 'Backing up Videos...'
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Videos/ /run/media/rob/backup/Videos
    echo 'Backing up Projects...'
    rsync -aiv --exclude='.git/' --exclude='pkg/' ~/Projects/ /run/media/rob/backup/Projects
    echo 'Backing up Bin...'
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/bin/ /run/media/rob/backup/bin
    echo 'Backing up Scripts...'
    rsync -aiv --delete --exclude='.dtrash' --exclude='.comments' --exclude='.git' ~/scripts/ /run/media/rob/backup/scripts
end

function ccache --description "Clears memory buffers and cache"
    sudo sh -c 'echo 1 > /proc/sys/vm/drop_caches'
end

function cdd --description "Changes to the dotfiles directory"
    cd ~/.config/dotfiles
end

function cdD --description "Changes to the Downloads directory"
    cd ~/Downloads
end

function cdt --description "Changes to the Temp directory"
    cd ~/Temp
end

function cdb --description "Changes to the Dropbox directory"
    cd ~/Dropbox
end

function cdn --description "Changes to the Notes directory"
    cd ~/Dropbox/Notes
end

function cdg --description "Changes to the Genealogy directory"
    cd ~/Dropbox/Genealogy
end

function cdm --description "Changes to the Mindmaps directory"
    cd ~/Dropbox/Mindmaps
end

function cdc --description "Changes to the .config directory"
    cd ~/.config
end

function cdp --description "Changes to the Projects directory"
    cd ~/Projects
end

function cdu --description "Changes to the /usr/share directory"
    cd /usr/share
end

function clrc --description "Clears the system buffers cache"
    sudo sysctl vm.drop_caches=1
end

function dfh --description "Shows disk usage in human readable form"
    df -h
end

function disks --description "Shows disk use and smart status"
    sudo inxi -a -D
end

function dui --description "Runs the disk usage utility"
    dua i
end

function eN --description "Edit the Neovim configuration"
    nvim ~/.config/nvim/init.lua
end

function enw --description "Run Emacs in terminal"
    emacs -nw
end

function lr --description "Sorts with newest at the top of list"
    eza -alh --sort=newest -r
end

function lo --description "Sorts with newest at the bottom of list"
    eza -alh --sort=newest
end

function la --description "List directory contents with optional regex search"
    eza -alh $argv
end

function lg --description "List recent git commits"
    eza -alh --git
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

function jvac --description "Cleans up (removes) old journal files"
    sudo journalctl --vacuum-files=2
end

function later --description "Shows weekly weather forecast"
    lynx "https://forecast.weather.gov/MapClick.php?CityName=Missoula&state=MT&site=MSO&lat=46.9181&lon=-114.153&unit=0&lg=english&FcstType=text&TextType=1"
end

function london --description "Shows current time in London, England"
    TZ="Europe/London" date
end

function sydney --description "Shows current time in Sydney, Australia"
    TZ="Australia/Sydney" date
end

function athens --description "Shows current time in Athens, Greece"
    TZ="Europe/Athens" date
end

function mconv --description "Compiles video files into one file"
    ffmpeg -f concat -safe 0 -i files.txt -c copy Time_Team.webm
end

function mpvd --description "Use mpv to watch DVD videos"
    mpv dvd://
end

function npkg --description "Shows list of recently installed/updated files"
    cd /var/cache/pacman/pkg
    fd --changed-within 1week -e zst
    cd ~/
end

function now --description "Shows current weather"
    curl --retry-max-time 5 --retry 5 https://tgftp.nws.noaa.gov/data/observations/metar/decoded/KMSO.TXT
end

function rgs --description "Searches current folder and shows 2 lines with match"
    rg -C 2 --max-depth 1 $argv
end

function ytdl --description "Downloads Youtube videos"
    yt-dlp --compat-options multistreams -4 $argv
end

function pacs --description "Search repos for package"
    pacman -Ss $argv
end

function pacq --description "Show package information"
    pacman -Qi $argv
end

function paci --description "Install package"
    sudo pacman -S $argv
end

function pacf --description "Search for package owning file"
    pacman -Qo $argv
end

function pacr --description "Remove package"
    sudo pacman -Rns $argv
end

function paco --description "Show list of orphan packages"
    sudo pacman -Qdt
end

function pacu --description "Pacman update from repos"
    sudo pacman -Syyu
end

function pacl --description "Show contents of installed package"
    pacman -Ql $argv
end

function pacc --description "Clear old packages from cache"
    sudo pacman -Sc
end

function pacm --description "Update pacman mirrors"
    sudo pacman-mirrors -c United_States
end

function pamu --description "Update all packages (repo and AUR)"
    pamac update
end

function pams --description "Search repos and AUR for package"
    pamac search $argv
end

function pami --description "Install package for repos or AUR"
    pamac install $argv
end

function icat
    kitty +kitten icat
end

function se --description "Annotate image with Spectacle"
    spectacle -E $argv
end

function slt
    systemctl list-timers
end

function rf --description "Search for text with Recoll"
    recollq -b $argv
end

function gitclean
    for file in (cat .gitignore)
        git rm -r --cached $file
    end
end
