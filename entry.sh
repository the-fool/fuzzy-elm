#!/bin/bash
set -e

npm i
elm-package install -y

exec "$@"
