{ pkgs, lib, config, inputs, ... }:

{
  packages = [ pkgs.cargo-watch ];
  languages.rust.enable = true;
}
