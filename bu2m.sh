#!/usr/bin/bash
rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Genealogy/ /run/media/rob/backup/Dropbox/Genealogy
rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Mindmaps/ /run/media/rob/backup/Dropbox/Mindmaps
rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Notes/ /run/media/rob/backup/Dropbox/Notes
rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Notebooks/ /run/media/rob/backup/Dropbox/Notebooks
rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Docs/ /run/media/rob/backup/Dropbox/Docs
rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Dropbox/Carroll/ /run/media/rob/backup/Dropbox/Carroll
rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/.googleearth/myplaces.kml /run/media/rob/backup/.googleearth
rsync -aiv --delete --exclude='.dtrash' --exclude='.comments/' ~/Videos/ /run/media/rob/backup/Videos
