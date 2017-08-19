let
  unstable = import /home/andrewmcveigh/code/nixpkgs {};
  stdenv = unstable.stdenv;
in {
  lifp = stdenv.mkDerivation {
    name = "chasm";
    src = ./.;
    buildInputs = with unstable; [
      cabal-install
      ghc
      zlib
      haskellPackages.idris
      gmp
    ];
  };
}
