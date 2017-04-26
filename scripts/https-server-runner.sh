#!/bin/bash -u

# `killall -HUP https-server-runner.sh` will kill this script.

server=dist/build/https-server/https-server

if test ! -f ${server} ; then
  echo "Missing '${server}. Need to build it first."
  exit 1
  fi

while /bin/true ; do
  timeout 1 ${server}
  # sleep 2
  done
