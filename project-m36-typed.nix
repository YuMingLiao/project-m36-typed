{ mkDerivation, base, basic-sop, binary, binary-instances
, binary-orphans, criterion, generics-sop, mtl, project-m36
, QuickCheck, quickcheck-instances, random, rio, splitmix, lib 
, tasty, tasty-hunit, tasty-quickcheck, tf-random, type-level-sets
, uuid, word8
}:
mkDerivation {
  pname = "project-m36-typed";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base basic-sop binary binary-instances binary-orphans generics-sop
    mtl project-m36 QuickCheck quickcheck-instances random rio splitmix
    tf-random type-level-sets uuid word8
  ];
  testHaskellDepends = [
    base basic-sop generics-sop QuickCheck rio tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base basic-sop criterion generics-sop QuickCheck rio tasty
    tasty-hunit tasty-quickcheck
  ];
  homepage = "http://github.com/matchwood/project-m36-typed#readme";
  description = "Typed interface to Project M36";
  license = lib.licenses.bsd3;
}
