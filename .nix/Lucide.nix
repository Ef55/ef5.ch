# Only the icons listed in `icons` are installed. Names are those shown on
# https://lucide.dev/icons.
{ lib, stdenvNoCC, fetchurl, icons }:

stdenvNoCC.mkDerivation rec {
  pname = "lucide-icons";
  version = "1.75.0";

  # The npm package has the plain SVGs, and is much smaller than the repository.
  src = fetchurl {
    url = "https://registry.npmjs.org/lucide-static/-/lucide-static-${version}.tgz";
    hash = "sha512-vtAnJzxoH8jp0++Ydf22CPAAL3WfQQjmDG+OuRlG+CXouLCrlfTz7zWuDvHkTtj61czkhJAISChHDXkxLLqKRw==";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    target="$out/share/lucide"
    mkdir -p "$target"
    touch "$target/lucide.css"
    for icon in ${lib.escapeShellArgs icons}; do
      if [ ! -f "icons/$icon.svg" ]; then
        echo "error: unknown Lucide icon '$icon' (see https://lucide.dev/icons)" >&2
        exit 1
      fi
      cp "icons/$icon.svg" "$target/"
      # One class per icon, used with `.icon` (see website/css/default.css).
      # The URL is relative to this CSS file, so it works wherever the site is.
      echo ".icon-$icon { mask-image: url('$icon.svg'); }" >> "$target/lucide.css"
    done
    cp LICENSE "$target/"
    runHook postInstall
  '';

  meta = {
    description = "Selected Lucide icons";
    homepage = "https://lucide.dev";
    license = lib.licenses.isc;
  };
}
