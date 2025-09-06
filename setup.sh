#!/usr/bin/env bash

# Cleanup old dotfiles
rm -rf ~/emacs.d ~/.gitconfig ~/.mg ~/.ripgreprc ~/.zshrc

# Create symlinks for dotfiles (dotfiles path is hardcoded since that
# path is just the usual for me).
ln -s ~/.dotfiles/emacs.d ~/.emacs.d
ln -s ~/.dotfiles/gitconfig ~/.gitconfig
ln -s ~/.dotfiles/mg ~/.mg
ln -s ~/.dotfiles/ripgreprc ~/.ripgreprc
ln -s ~/.dotfiles/zshrc ~/.zshrc
