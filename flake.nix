{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, emacs-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ emacs-overlay.overlays.default ];
      };
    in
    {
      packages.${system}.emacs = pkgs.emacsWithPackagesFromUsePackage {
        config = ./config.el;
        package = pkgs.emacs30-pgtk;
        alwaysEnsure = true;
      };
    };
}
