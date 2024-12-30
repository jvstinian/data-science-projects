{ pkgs ? import <nixpkgs> {} }:
pkgs.callPackage ./pyapp.nix {}
