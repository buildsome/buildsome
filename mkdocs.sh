#!/bin/bash

if [[ "$1" == "setup" ]] ; then
    sudo dnf install libyaml-devel python-virtualenv
    virtualenv docenv
    ./docenv/bin/pip install mkdocs==0.15.3
elif [[ "$1" == "upload" ]] ; then
    ./docenv/bin/mkdocs gh-deploy --clean
elif [[ "$1" == "serve" ]] ; then
    ./docenv/bin/mkdocs serve
elif [[ "$1" == "build" ]] ; then
    ./docenv/bin/mkdocs build
fi

