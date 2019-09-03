#!/bin/bash

###
# Jimmy Hickey
# 2019-08-24
# Launch SAS for Mac.
###

SERVER_UP="200"
PAGE="http://127.0.0.1:10080/SASStudio/38/"
SAS_FOLDER="/Applications/SASUniversityEdition/myfolders/grad-scripts"

cd ${SAS_FOLDER}
git pull

# check if the server is already up
# curl will output just the HTTP code
# a 200 code means that the server is ready
function check_server()
{
	echo "$(curl -s -o /dev/null -w "%{http_code}" $PAGE)"
}

# get initial server status
server_status=$(check_server)

# If the server is down, launch it
if [[ "$server_status" != *"$SERVER_UP"* ]]; then
	VBoxManage startvm SAS\ University\ Edition --type headless

  # Wait for the web page to actually be up
	while [[ "$server_status" != *"$SERVER_UP"* ]];
	do
		server_status=$(check_server)
	done

fi

# Open SAS page in a new chrome window
open -na "Google Chrome" --args --new-window ${PAGE}
