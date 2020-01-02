#!/bin/sh -e
~/.cask/bin/cask exec ert-runner -L . -L test "$@"
