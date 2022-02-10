{ mkDerivation, base, basic-sop, binary, binary-orphans, bytestring
, containers, criterion, generics-sop, mtl, ordered-containers
, project-m36, QuickCheck, quickcheck-instances, random, rio
, lib, tasty, tasty-hunit, tasty-quickcheck, text, tf-random
, time, type-level-sets, type-spec, uuid, vector, word8, splitmix, winery, convertible
}:
mkDerivation {
  pname = "project-m36-typed";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base basic-sop binary binary-orphans bytestring containers
    generics-sop mtl ordered-containers project-m36 QuickCheck
    quickcheck-instances random rio text tf-random time type-level-sets
    uuid vector word8 splitmix winery convertible
  ];
  testHaskellDepends = [
    base basic-sop binary binary-orphans generics-sop QuickCheck rio
    tasty tasty-hunit tasty-quickcheck text type-spec uuid
  ];
  benchmarkHaskellDepends = [
    base basic-sop criterion generics-sop QuickCheck rio tasty
    tasty-hunit tasty-quickcheck
  ];
  homepage = "http://github.com/matchwood/project-m36-typed#readme";
  description = "Typed interface to Project M36";
  license = lib.licenses.bsd3;
}
