{ mkDerivation, base, pure-core, pure-default, pure-elm, pure-time, stdenv }:
mkDerivation {
  pname = "pure-hooks";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-default pure-elm pure-time ];
  homepage = "github.com/grumply/pure-hooks";
  description = "Effect Hooks";
  license = stdenv.lib.licenses.bsd3;
}
