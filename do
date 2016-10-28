#!/bin/bash

# Usage: ./do [build|dev]

if [ $# -lt 1 ]; then
	echo "Usage: ./do [build|dev]"
	exit 1
fi

function build {
  docker-compose build
}

if [ "$1" == "build" ]; then
  build
fi

if [ "$1" == "dev" ]; then
  docker inspect the-fool/elm-fuzz &> /dev/null
  if [ $? -ne 0 ]; then
    build
  fi
  docker-compose up
fi
