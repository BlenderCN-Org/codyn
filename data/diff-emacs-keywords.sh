#!/bin/bash

# Compares keywords from stdin with those in codyn-mode.el

diff -U0 \
    <(
        for i in $(cat /dev/stdin | sed 's/[^a-zA-Z\_-]/ /g'); do echo $i; done |
        grep '[a-z]' |
        sort
    ) \
    <( \
        for i in $(cat codyn-mode.el | grep regexp-opt); do echo $i; done |
        grep -v 'concat\|regexp' |
        grep '[a-z]' |
        sed "s/[()\"']//g" |
        sort
    )
