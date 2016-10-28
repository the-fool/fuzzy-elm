#!/bin/bash
set -e
npm i
npm i -g elm
elm-package install -y

exec "$@"
