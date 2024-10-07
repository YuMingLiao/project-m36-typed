let
  snack-build-project-m36 = true; 
project-m36 =
    import /home/nixos/snack-for-project-m36/lib.nix;
in {
  src = ./src;
  name = "project-m36-typed";
  packages = if snack-build-project-m36 then [project-m36] else [];
  dependencies = [
    "rio"
    "base"
    "generics-sop"
    "basic-sop"
    "QuickCheck"
    "binary"
    "binary-orphans"
    "type-level-sets"
    "random"
    "tf-random"
    "mtl"
    "uuid"
    "word8"
    "quickcheck-instances"
    "splitmix"
    "binary-instances"
  ] ++ (if snack-build-project-m36 then [] else ["project-m36"]);
  ghcOpts = [
    "-XHaskell2010"
    "-XBangPatterns"
    "-XBinaryLiterals"
    "-XConstraintKinds"
    "-XDataKinds"
    "-XDefaultSignatures"
    "-XDeriveDataTypeable"
    "-XDeriveFoldable"
    "-XDeriveFunctor"
    "-XDeriveGeneric"
    "-XDeriveTraversable"
    "-XDerivingStrategies"
    "-XDoAndIfThenElse"
    "-XEmptyDataDecls"
    "-XExistentialQuantification"
    "-XFlexibleContexts"
    "-XFlexibleInstances"
    "-XFunctionalDependencies"
    "-XGADTs"
    "-XGeneralizedNewtypeDeriving"
    "-XInstanceSigs"
    "-XKindSignatures"
    "-XLambdaCase"
    "-XMultiParamTypeClasses"
    "-XMultiWayIf"
    "-XNamedFieldPuns"
    "-XNoImplicitPrelude"
    "-XOverloadedStrings"
    "-XPartialTypeSignatures"
    "-XPatternGuards"
    "-XPolyKinds"
    "-XRankNTypes"
    "-XRecordWildCards"
    "-XScopedTypeVariables"
    "-XStandaloneDeriving"
    "-XTupleSections"
    "-XTypeApplications"
    "-XTypeFamilies"
    "-XTypeOperators"
    "-XTypeSynonymInstances"
    "-XViewPatterns"
  ];
  extensions = [ ];
}
