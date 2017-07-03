#!/bin/bash -u
# Prepares a centos/rhel/fedora system for building buildsome
cd $(dirname $0)
grep BuildReq buildsome.spec  | cut -d: -f2- | xargs sudo yum install -y
