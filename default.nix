{
  pkgs ? import <nixpkgs> { },
}:
pkgs.callPackage ./nelson.nix { }
