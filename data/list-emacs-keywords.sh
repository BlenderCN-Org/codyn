#!/bin/bash

for i in $(cat codyn-mode.el | grep regexp-opt); do echo $i; done |
grep -v 'concat\|regexp' |
grep '[a-z]' |
sed "s/[()\"']//g" |
sort
