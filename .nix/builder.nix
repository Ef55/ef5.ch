{ mkDerivation, base, hakyll, lib, pandoc, pandoc-types, process, text, time, lucid2, clay }:

mkDerivation {
  pname = "ef5-ch";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base hakyll pandoc pandoc-types process text time lucid2 clay
  ];
  license = "unknown";
  mainProgram = "site";
}