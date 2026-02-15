{ pkgs, lib, config, inputs, ... }:

{
	packages = [ pkgs.cargo-watch ];
	languages.rust = {
		enable = true;
		channel = "stable";
		mold.enable = true;
	};
}
