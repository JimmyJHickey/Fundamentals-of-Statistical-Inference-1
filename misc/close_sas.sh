#!/usr/bin/env bash

###
# Jimmy Hickey
# 2019-08-24
# Close SAS for Mac.
###

SAS_FOLDER="/Applications/SASUniversityEdition/myfolders/grad-scripts"
GIT_FOLDER="/Users/jimmy/git/grad-scripts"
DATE="$(date '+%Y-%m-%d %H:%M:%S')"
COMMIT_MESSAGE="SAS code update "${DATE}

# Power off SAS VirtualBox
VBoxManage controlvm SAS\ University\ Edition poweroff

cd ${SAS_FOLDER}
git pull

git add .
git commit -m "${COMMIT_MESSAGE}"
git push

cd ${GIT_FOLDER}
git pull
