#!/bin/bash

# Usage: ./do [build|dev]

if [ $# -lt 1 ]; then
	echo "Usage: ./do [build|dev]"
	exit 1
fi

function build {
  docker build -t elm-fuzz .
}

if [ "$1" == "build" ]; then
  build
fi

if [ "$1" == "dev" ]; then
  docker inspect elm-fuzz &> /dev/null
  if [ $? -ne 0 ]; then
    build
  fi
  docker run -v $(pwd):/app elm-fuzz:latest npm start
fi
