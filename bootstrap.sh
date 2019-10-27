#!/bin/sh
set -e

# Install home-manager
nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --add https://nixos.org/channels/nixos-unstable unstable
nix-channel --update
exec $SHELL
nix-shell '<home-manager>' -A install

# Link home-config
mv ~/.config/nixpkgs/home.nix ~/home-config
rmdir ~/.config/nixpkgs
ln -s ~/home-config nixpkgs
