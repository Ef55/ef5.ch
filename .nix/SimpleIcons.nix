# Only the icons listed in `icons` are installed, as for Lucide. Names are the
# slugs used by simpleicons.org.
{ lib, stdenvNoCC, fetchurl, icons }:

stdenvNoCC.mkDerivation rec {
  pname = "simple-icons";
  version = "16.38.0";

  src = fetchurl {
    url = "https://registry.npmjs.org/simple-icons/-/simple-icons-${version}.tgz";
    hash = "sha512-B0Wws90eL0Fm65aH35CA8kM+7/Zzgsaytt0DndBx7VFm3+o9HurVIVRQZ1hvqqYYIyNJq7ycn3kQm2v2FhiWSQ==";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    target="$out/share/simple-icons"
    mkdir -p "$target"
    touch "$target/simple-icons.css"
    for icon in ${lib.escapeShellArgs icons}; do
      if [ ! -f "icons/$icon.svg" ]; then
        echo "error: unknown Simple Icons slug '$icon' (see https://simpleicons.org)" >&2
        exit 1
      fi
      cp "icons/$icon.svg" "$target/"
      # One class per icon, as for Lucide; "si-" avoids clashes with its names.
      echo ".si-$icon { mask-image: url('$icon.svg'); }" >> "$target/simple-icons.css"
    done
    cp LICENSE.md "$target/"
    runHook postInstall
  '';

  meta = {
    description = "Selected Simple Icons";
    homepage = "https://simpleicons.org";
    license = lib.licenses.cc0;
  };
}
