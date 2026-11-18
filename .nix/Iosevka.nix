# Custom build of Iosevka for the web (WOFF2 + CSS).
#
# Adapted from:
# https://github.com/NixOS/nixpkgs/blob/1f738b3f6d965d650009a7d211d0f521587188e9/pkgs/data/fonts/iosevka/default.nix

# Copyright (c) 2003-2024 Eelco Dolstra and the Nixpkgs/NixOS contributors
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
{ stdenv, lib, buildNpmPackage, fetchFromGitHub, remarshal, ttfautohint-nox, cctools }:

buildNpmPackage rec {
  pname = "Iosevka-web";
  version = "30.0.1";

  src = fetchFromGitHub {
    owner = "be5invis";
    repo = "iosevka";
    rev = "v${version}";
    hash = "sha256-THs6kN5VZpTvzTK7w/sGQbxoEyyPwzl93JDOvwucgeo=";
  };
  npmDepsHash = "sha256-maDIkbe4BKY7XYOQNGdOalyTGdBXgIU5t0QjVJW6lvQ=";

  nativeBuildInputs = [
    remarshal
    ttfautohint-nox
  ] ++ lib.optionals stdenv.hostPlatform.isDarwin [
    cctools
  ];

  # Sans, normal spacing, no character variants / stylistic sets (smaller
  # webfonts, see https://github.com/be5invis/Iosevka/issues/1414), weights
  # Light (300) to ExtraBold (800).
  buildPlans = ''
    [buildPlans.Iosevka-web]
    family = "Iosevka"
    spacing = "normal"
    serifs = "sans"
    noCvSs = true
    export-glyph-names = false

    [buildPlans.Iosevka-web.weights.Light]
    shape = 300
    menu = 300
    css = 300

    [buildPlans.Iosevka-web.weights.Regular]
    shape = 400
    menu = 400
    css = 400

    [buildPlans.Iosevka-web.weights.Medium]
    shape = 500
    menu = 500
    css = 500

    [buildPlans.Iosevka-web.weights.SemiBold]
    shape = 600
    menu = 600
    css = 600

    [buildPlans.Iosevka-web.weights.Bold]
    shape = 700
    menu = 700
    css = 700

    [buildPlans.Iosevka-web.weights.ExtraBold]
    shape = 800
    menu = 800
    css = 800
  '';
  passAsFile = [ "buildPlans" ];

  configurePhase = ''
    runHook preConfigure
    cp "$buildPlansPath" private-build-plans.toml
    runHook postConfigure
  '';

  # Use all cores the builder grants. Pipe through cat to disable verda's
  # progress bar, as in nixpkgs.
  buildPhase = ''
    runHook preBuild
    export HOME=$TMPDIR
    npm run build --no-update-notifier -- \
      --jCmd=$NIX_BUILD_CORES --verbosity=9 \
      ttf::$pname webfont::$pname | cat
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    target="$out/share/Iosevka-web"
    mkdir -p "$target"
    # WOFF2 only: all current browsers support it, and the TTFs would add
    # ~60 MB to the website. Drop the TTF fallbacks from the CSS accordingly.
    for css in dist/$pname/*.css; do
      sed "s|, url('TTF/[^']*') format('truetype')||" "$css" > "$target/$(basename "$css")"
    done
    cp -r dist/$pname/WOFF2 "$target"
    runHook postInstall
  '';
}
