
[![Build Status](https://travis-ci.org/chisui/dakka.svg?branch=master)](https://travis-ci.org/chisui/dakka)

# Dakka

A dependently typed actor framework for haskell.

## Build

    nix-build

## Build thesis

    nix-build thesis

To include the current date and hashes for the current commit run:

    nix-build thesis \
      --arg date "$(date +\"%Y-%m-%d\")" \
      --argstr commit "$(curl https://api.github.com/repos/chisui/dakka/git/refs/heads/master)" \
      --argstr sha256 "$(nix-prefetch-url --unpack https://github.com/chisui/dakka/archive/master.tar.gz)"

Check out a build pdf version of the current master version [here](https://chisui.github.io/dakka/thesis.pdf).

