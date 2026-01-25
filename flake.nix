{
  description = "Emacs configuration with all dependencies";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };

        isLinux = pkgs.stdenv.isLinux;
        isDarwin = pkgs.stdenv.isDarwin;

        # Language servers and tools
        languageServers = with pkgs; [
          # LSP servers
          gopls
          pyright
          nodePackages.bash-language-server
          nodePackages.typescript-language-server
          nodePackages.vscode-langservers-extracted # html, css, json, eslint
          nodePackages.yaml-language-server
          taplo # TOML LSP
          lua-language-server
          clang-tools # clangd for C/C++
          emacs-lsp-booster # eglot-booster dependency

          # Development tools
          nodejs_24
          git
        ];

        # System dependencies
        systemDeps = with pkgs; [
          # Fonts
          nerd-fonts.inconsolata
          inter

          # Build tools
          cmake
          gnumake
          gcc

          # Search tools
          ripgrep
          fd
        ]
        ++ pkgs.lib.optionals isLinux [
          pkgs.libvterm       # vterm module
          pkgs.wl-clipboard   # Wayland clipboard
        ];

        # Custom Emacs with packages
        emacsPackage = if isDarwin then pkgs.emacs30-macport else pkgs.emacs30-pgtk;

        myEmacs = pkgs.emacsWithPackagesFromUsePackage {
          config = ./config.el;
          package = emacsPackage;
          alwaysEnsure = true;
        };

      in
      {
        # Default package - Emacs with all dependencies
        packages.default = pkgs.symlinkJoin {
          name = "emacs-configured";
          paths = [ myEmacs ] ++ languageServers ++ systemDeps;
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/emacs \
              --prefix PATH : ${pkgs.lib.makeBinPath (languageServers ++ systemDeps)}
          '';
        };

        # Just Emacs with packages
        packages.emacs = myEmacs;

        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = [
            myEmacs
          ] ++ languageServers ++ systemDeps;

          shellHook = ''
            echo "Emacs development environment loaded"
            echo "Emacs: ${myEmacs}/bin/emacs"
            echo "LSP servers and tools available in PATH"
          '';
        };

        # App for running Emacs
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/emacs";
        };
      }
    );
}
