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

        # Emacs packages from ELPA/MELPA
        emacsPackages = with pkgs.emacsPackages; [
          # Completion and navigation
          vertico
          orderless
          marginalia
          consult
          avy

          # Evil mode ecosystem
          evil
          evil-collection
          evil-surround
          evil-commentary
          evil-snipe
          evil-matchit
          evil-visualstar
          evil-numbers
          goto-chg

          # Org mode extensions
          toc-org
          org-modern
          org-appear
          olivetti
          org-fragtog
          org-transclusion

          # UI enhancements
          which-key
          nerd-icons
          dirvish
          vi-tilde-fringe

          # Completion
          corfu
          cape

          # Language modes
          markdown-mode
          go-mode
          web-mode
          typescript-mode
          js2-mode
          lua-mode
          nix-mode

          # LSP and development
          eldoc-box
          magit
          diff-hl
          vterm
          treesit-auto

          # Utilities
          general
        ];

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
          # Clipboard (Wayland)
          wl-clipboard

          # Fonts
          (nerdfonts.override { fonts = [ "Inconsolata" ]; })
          inter

          # Terminal and build tools
          libvterm
          cmake
          gnumake
          gcc

          # Search tools
          ripgrep
          fd
        ];

        # Custom Emacs with packages
        myEmacs = pkgs.emacsWithPackagesFromUsePackage {
          config = ./config.el;
          package = pkgs.emacs30-pgtk;
          alwaysEnsure = true;
          extraEmacsPackages = epkgs: emacsPackages;
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
