# ef5.ch

## Build

Using the nix flake:
```
nix build .nix              # Website in result/dist/
nix build .nix#website-zip  # Website as a zip archive (containing _site/), at result
```

## Develop

Using the provided nix setup:
```
nix develop .nix # Or use nix-direnv
cabal run site -- build
cabal run site -- watch # Preview at http://127.0.0.1:8000
```

**Note** The nix shell does more than install dependencies: it also (sym-)links
some assets (fonts, katex, ...) into the sources. Existing directories (i.e. not
symlinks) at those paths are left untouched.