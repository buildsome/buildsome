#!/bin/bash

gcc -g -lrt -O2 -Wall sha1.c shared_test.c -o shared_test && "$@" ./shared_test && rm -f shared_test
