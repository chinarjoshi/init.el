{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.nodejs_24
  ];

  shellHook = ''
    echo "Welcome to the jungle"
    . ~/.config/.aliases.sh
  '';
}

