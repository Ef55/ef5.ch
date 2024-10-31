{ mkDerivation, base, hakyll, lib, pandoc, pandoc-types, process, text, time }:

mkDerivation {
  pname = "ef5-ch";
  version = "0.1.0.0";
  src = ./../src;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base hakyll pandoc pandoc-types process text time
  ];
  license = "unknown";
  mainProgram = "site";
}