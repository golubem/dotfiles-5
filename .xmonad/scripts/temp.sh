#!/bin/bash

temp=$(sensors | grep 'Physical' | awk '{ print $4}')

echo "$temp"
