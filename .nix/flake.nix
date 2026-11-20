{
  description = "A flake to build ef5.ch.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;

        hpkgs = pkgs.haskellPackages;

        root = ../.;
        haskellSrc = lib.fileset.toSource {
          inherit root;
          fileset = lib.fileset.unions [ ../ef5-ch.cabal ../src ];
        };
        websiteSrc = lib.fileset.toSource {
          inherit root;
          fileset = lib.fileset.difference ../website (lib.fileset.unions [
            (lib.fileset.maybeMissing ../website/fonts)
            (lib.fileset.maybeMissing ../website/icons)
            (lib.fileset.maybeMissing ../website/katex)
          ]);
        };

        # The website builder.
        ef5-ch = hpkgs.callCabal2nix "ef5-ch" haskellSrc { };

        # Assets used by the website...
        iosevka-web = pkgs.callPackage ./Iosevka.nix { };
        lucide-icons = pkgs.callPackage ./Lucide.nix {
          icons = [
            "mail"
            "file-text"
            "folder-git-2"
            "info"
            "quote"
          ];
        };
        simple-icons = pkgs.callPackage ./SimpleIcons.nix {
          icons = [
            "github"
            "git"
          ];
        };

        # ... (sym-)linked into the `website` directory.
        linkAssets = ''
          link() {
            if [ -e "$2" ] && [ ! -L "$2" ]; then
              # Do not overwrite if it already exists, and is not a symlink
              echo "warning: $2 is not a symlink, leaving it untouched" >&2
            else
              ln -sfnT "$1" "$2"
            fi
          }
          mkdir -p website/fonts
          link ${iosevka-web}/share/Iosevka-web website/fonts/Iosevka-web
          mkdir -p website/icons
          link ${lucide-icons}/share/lucide website/icons/lucide
          link ${simple-icons}/share/simple-icons website/icons/simple-icons
          link ${pkgs.katex}/lib/node_modules/katex/dist website/katex
        '';

        website = pkgs.stdenvNoCC.mkDerivation {
          name = "ef5.ch";
          src = websiteSrc;
          nativeBuildInputs = [ ef5-ch pkgs.katex ];
          # https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
          LANG = "C.UTF-8";
          buildPhase = ''
            ${linkAssets}
            site build --verbose
          '';
          installPhase = ''
            mkdir -p "$out/dist"
            cp -a _site/. "$out/dist"
          '';
        };

        # The website as a zip archive (a single file), containing _site/.
        # Sorted entries, fixed timestamps and no extra attributes, so the
        # archive is reproducible.
        website-zip = pkgs.runCommand "site.zip" { nativeBuildInputs = [ pkgs.zip ]; } ''
          cp -r --no-preserve=mode ${website}/dist _site
          find _site -exec touch -d "1980-01-01T00:00:00Z" {} +
          find _site -print | LC_ALL=C sort | zip -X -q "$out" -@
        '';

        # Uploads the website with rclone.
        # Nextcloud rejects some timestamps, so we set one we know it will accept.
        # --checksum: compare by content, since mtimes carry no information.
        deploy = pkgs.writeShellApplication {
          name = "deploy";
          runtimeInputs = [ pkgs.coreutils pkgs.findutils pkgs.rclone ];
          text = ''
            if [ $# -lt 1 ]; then
              echo "usage: deploy <remote:path> [rclone flags...]" >&2
              exit 1
            fi
            dest="$1"; shift
            site="$(mktemp -d)"
            trap 'rm -rf "$site"' EXIT
            cp -rL --no-preserve=mode ${website}/dist/. "$site"
            find "$site" -exec touch -d "2000-01-01T00:00:00Z" {} +
            rclone sync "$site"/ "$dest" --checksum --transfers 16 --progress "$@"
          '';
        };
      in {
        apps.deploy = {
          type = "app";
          program = "${deploy}/bin/deploy";
        };

        devShells.default = hpkgs.shellFor {
          packages = _: [ ef5-ch ];

          nativeBuildInputs = [
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.ormolu
            hpkgs.hlint

            pkgs.katex

            pkgs.rclone
            pkgs.act
          ];

          shellHook = linkAssets;
        };

        packages = {
          inherit ef5-ch website website-zip;
          default = website;
        };
      });
}
