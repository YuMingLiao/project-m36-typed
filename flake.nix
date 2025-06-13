{
  inputs = rec {
    common.url = "github:YuMingLiao/common";
    nixpkgs.follows = "common/nixpkgs";
    project-m36.url = "github:yumingliao/project-m36";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      common,
      project-m36,
      ...
    }:
    common.lib.mkFlake { inherit inputs; } {
      perSystem =
        {
          self',
          pkgs,
          config,
          system,
          ...
        }:
        {
          haskellProjects.default = {
            basePackages = config.haskellProjects.ghc965.outputs.finalPackages;

            packages = {
              project-m36.source = project-m36;
            };
          };

          packages.default = self'.packages.project-m36-typed;
          devShells.final = pkgs.mkShell {
            name = "A shell that has the final project-m36-typed";
            inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
            nativeBuildInputs = [
              (config.haskellProjects.default.outputs.finalPackages.ghcWithPackages (
                p: with p; [ project-m36-typed tasty-quickcheck tasty-hunit ]
              ))
            ];
          };

        };
    };
}
