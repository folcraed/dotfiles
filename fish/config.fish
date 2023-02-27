set fish_greeting
starship init fish | source
set fzf_preview_dir_cmd exa --all --color=always

#================================================
# Functions
#================================================
function fish_title
  echo (status current-command) ' '
end

function bu2
        rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Genealogy/ /run/media/rob/backup/Dropbox/Genealogy
        rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Mindmaps/ /run/media/rob/backup/Dropbox/Mindmaps
        rsync -aiv --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Notes/ /run/media/rob/backup/Dropbox/Notes
        rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Notebook/ /run/media/rob/backup/Dropbox/Notebook
        rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Docs/ /run/media/rob/backup/Dropbox/Docs
        rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Carroll/ /run/media/rob/backup/Dropbox/Carroll
        rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/.googleearth/myplaces.kml /run/media/rob/backup/.googleearth
        rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Videos/ /run/media/rob/backup/Videos
        rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/.local/bin/ /run/media/rob/backup/.local/bin
        rsync -aiv --delete --exclude='.dtrash' --exclude='.comments' --exclude='.git' ~/scripts/ /run/media/rob/backup/scripts
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

function cdl
    cd ~/code/learning
end

function dfh
	df -h
end

function eN
	nvim ~/.config/nvim/init.lua
end

function ela --description "Searches folder for file pattern"
	exa -la $argv
end

function extract --description "Expand or extract bundled & compressed files"
  set --local ext (echo $argv[1] | awk -F. '{print $NF}')
  switch $ext
    case tar  # non-compressed, just bundled
      tar -xvf $argv[1]
    case gz
      if test (echo $argv[1] | awk -F. '{print $(NF-1)}') = tar  # tar bundle compressed with gzip
        tar -zxvf $argv[1]
      else  # single gzip
        gunzip $argv[1]
      end
    case tgz  # same as tar.gz
      tar -zxvf $argv[1]
    case bz2  # tar compressed with bzip2
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
	exa -alh $argv
end

function later
        lynx "https://forecast.weather.gov/MapClick.php?CityName=Missoula&state=MT&site=MSO&lat=46.9181&lon=-114.153&unit=0&lg=english&FcstType=text&TextType=1"
end

function lg
	exa -alh --git
end

function lt
	exa -alh --sort=modified
end

function mconv
    ffmpeg -f concat -safe 0 -i files.txt -c copy Time_Team.webm
end

function now
    curl --retry-max-time 5 --retry 5 https://tgftp.nws.noaa.gov/data/observations/metar/decoded/KMSO.TXT
end

function rgs --description "Searches current folder and shows 2 lines with match"
    rg -C 2 --max-depth 1 $argv
end

function ytdl
	yt-dlp $argv
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

function wv
	cd /mnt/rob/Videos
    exa -l
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
