#!/bin/bash
if [ -d ~/.config/nvim ]; then
    rm -r ~/.config/nvim
fi

ln -s ~/.config/dotfiles/nvim ~/.config/nvim

url="https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

if [ ! -f ~/.config/nvim/autoload/plug.vim ]; then
    curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs $url
fi
if [ -d ~/.config/nvim/.py3 ]; then
    rm -r ~/.config/nvim/.py3
fi

python -m venv ~/.config/nvim/.py3
. ~/.config/nvim/.py3/bin/activate

pip install neovim
pip install isort
deactivate
sudo pacman -S neovim
