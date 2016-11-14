#!/bin/bash
set -e
npm i
npm i -g elm@0.17.1
elm-package install -y
chmod 766 -R elm-stuff
exec "$@"
