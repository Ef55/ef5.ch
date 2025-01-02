{
  description = "A flake to build ef5.ch.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }@input: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        hpkgs = pkgs.haskell.packages.ghc966;

        iosevka-custom = pkgs.callPackage ./.nix/iosevka.nix {};
        
        executable = hpkgs.callPackage ./.nix/builder.nix {};

        runner = pkgs.writeShellApplication {
          name = "ef5-runner";

          runtimeInputs = [
            executable
            pkgs.nodePackages.katex
          ];

          text = ''
            # Symlink Iosevka
            fontdir=website/fonts
            mkdir -p "$fontdir"
            iosevkatarget="$fontdir"/Iosevka-web
            if [ -L "$iosevkatarget" ] ; then
              rm "$iosevkatarget"
            fi
            ln -s ${iosevka-custom}/share/Iosevka-web "$iosevkatarget"

            # Symlink katex
            katexdir=website/katex
            if [ -L "$katexdir" ] ; then
              rm "$katexdir"
            fi
            ln -s ${pkgs.nodePackages.katex}/lib/node_modules/katex/dist "$katexdir"

            ${executable}/bin/site "$@"
          '';
        };

        website = pkgs.stdenv.mkDerivation {
          name = "ef5.ch";
          buildInputs = [ ];
          src = pkgs.nix-gitignore.gitignoreSourcePure ./.gitignore ./.;

          # https://github.com/rpearce/hakyll-nix-template/blob/3d0e857197bed3ffd77f36233452e8eea279c64b/flake.nix#L64
          LANG = "en_US.UTF-8";
          # https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";

          buildPhase = ''
            ${runner}/bin/ef5-runner build --verbose
          '';

          installPhase = ''
            mkdir -p "$out/dist"
            cp -a _site/. "$out/dist"
          '';
        };
      in {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = (with hpkgs; [
              ghc
              stack
              hakyll
              cabal2nix

              haskell-language-server
              ormolu
              hlint
            ]);

            NIX_PATH = "nixpkgs=" + pkgs.path;
          };
        };

        apps = {
          default = flake-utils.lib.mkApp {
            drv = runner;
            exePath = "/bin/ef5-runner";
          };
        };

        packages = {
          runner = runner;
          website = website;
          default = website;
        };
      });
}
