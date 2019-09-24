#!/bin/bash
set -eux

echo hello > /tmp/buildsome-test-result
mv -v /tmp/buildsome-test-result $(realpath result)
ls -lah ./result
