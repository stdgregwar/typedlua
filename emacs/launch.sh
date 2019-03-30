#!/bin/bash
echo "Launching emacs with tl-mode/tl-checker"
PWD=$(pwd)
LUA_PATH="$PWD/?.lua"
PATH=$PWD:$PATH
PATH=$PATH LUA_PATH=$LUA_PATH emacs -l "$PWD/emacs/tl-mode.el" -l "$PWD/emacs/tl-checker.el"

