#!/bin/bash

###
# Jimmy Hickey
# 2019-08-24
# Launch SAS for Mac.
###

SERVER_UP="OK"

# check if the server is already up
check_server=$(curl -Is http://127.0.0.1:10080/SASInformationCenter/index.html | head -1)

# If the server is down, launch it
if [[ "$check_server" != *"$SERVER_UP"* ]]; then
  VBoxManage startvm SAS\ University\ Edition --type headless

  # Wait for the web page to actually be up
  while [[ "$check_server" != *"$SERVER_UP"* ]];
  do
    check_server=$(curl -Is http://127.0.0.1:10080/SASInformationCenter/index.html | head -1)
  done

fi

# Open SAS page in a new chrome window
open -na "Google Chrome" --args --new-window http://127.0.0.1:10080
