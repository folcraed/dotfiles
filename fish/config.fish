set fish_greeting
starship init fish | source

#================================================
# Functions
#================================================
function fish_title
  echo (status current-command) ' '
end

function bu
	rsync -aiv --exclude='.comments/' /home/rob/Dropbox/ /run/media/rob/Backups
end

function bu2
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Genealogy/ /mnt/rob/Dropbox/Genealogy &&
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Mindmaps/ /mnt/rob/Dropbox/Mindmaps &&
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Notes/ /mnt/rob/Dropbox/Notes &&
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Notebooks/ /mnt/rob/Dropbox/Notebooks &&
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Docs/ /mnt/rob/Dropbox/Docs &&
    rsync -aiv --delete --exclude='.comments/' ~/.googleearth/myplaces.kml /mnt/rob/.googleearth
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Genealogy/ /run/media/rob/Dropbox/Genealogy &&
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Mindmaps/ /run/media/rob/Dropbox/Mindmaps &&
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Notes/ /run/media/rob/Dropbox/Notes &&
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Notebooks/ /run/media/rob/Dropbox/Notebooks &&
    rsync -aiv --delete --exclude='.comments/' /home/rob/Dropbox/Docs/ /run/media/rob/Dropbox/Docs &&
    rsync -aiv --delete --exclude='.comments/' ~/.googleearth/myplaces.kml /run/media/rob/.googleearth
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

function cds
	cd ~/Dropbox/Settings
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

function dop
    qutebrowser 'https://radar.weather.gov/lite/N0R/MSX_loop.gif'
end

function eN
	nvim ~/.config/nvim/init.lua
end

function eZ
    nvim ~/Dropbox/Settings/.zshrc
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
    lynx 'https://forecast.weather.gov/MapClick.php?lat=46.8582&lon=-113.99&unit=0&lg=english&FcstType=text&TextType=1'
end

function lg
	exa -alh --git
end

function lt
	exa -alh --sort=modified
end

function now
    curl https://tgftp.nws.noaa.gov/data/observations/metar/decoded/KMSO.TXT
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

function rect
  recoll -t -a $argv
end
