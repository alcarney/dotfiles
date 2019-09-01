#!/bin/bash
sudo pacman -S \
    imagemagick \
    rofi \
    sxiv

pikaur -S \
    i3-gaps-rounded-git \
    polybar

python -m pip install --upgrade --user pipx
pipx install pywal
