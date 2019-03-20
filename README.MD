
[![Build Status](https://travis-ci.org/chisui/dakka.svg?branch=master)](https://travis-ci.org/chisui/dakka)
[![Dakka: thesis](https://img.shields.io/badge/Dakka-thesis-blue.svg)](https://chisui.github.io/dakka/thesis.pdf)
[![Dakka: presentation](https://img.shields.io/badge/Dakka-presentation-blue.svg)](https://chisui.github.io/dakka/)

# Dakka

A dependently typed actor framework for haskell.

## Build

    nix build

## Build thesis

    nix build -f thesis

To include the current date and hashes for the current `HEAD` of `master` run:

    nix build -f thesis \
      --arg date "$(date +\"%Y-%m-%d\")" \
      --argstr commit "$(curl https://api.github.com/repos/chisui/dakka/git/refs/heads/master)" \
      --argstr sha256 "$(nix-prefetch-url --unpack https://github.com/chisui/dakka/archive/master.tar.gz)"

Check out a build pdf version of the current master version [here](https://chisui.github.io/dakka/thesis.pdf).

## Build presentation

    nix build -f presentation

Checkout a build version of the current master version [here](https://chisui.github.io/dakka/).

