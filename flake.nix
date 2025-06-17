{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    packageset.url = "github:mattpolzin/nix-idris2-packages";
    # don't set any follows for packageset if you want to benefit
    # from the Cachix cache. otherwise, go ahead.
    idris2f = {
      url = "github:/idris-lang/idris2/562ef55a31635a2a7196b80ad7a0e034b8de6d43";
      inputs.nixpkgs.follows = "nixpkgs";};   
  };

  outputs =
    {
      self,
      nixpkgs,
      packageset,
      idris2f,
      ...
    }:
    let
      lib = nixpkgs.lib;
      forEachSystem =
        f:
        lib.genAttrs lib.systems.flakeExposed (
          system:
          f {
            inherit system;
            inherit (packageset.packages.${system}) idris2 idris2Lsp buildIdris buildIdris' idris2Packages;
            pkgs = nixpkgs.legacyPackages.${system};
            inherit idris2f;
          }
        );
    in
    {
      packages = forEachSystem (
        { buildIdris, idris2Packages,idris2,system,... }:
        let
          # if you have 'allow-import-from-derivation' set true then you could
          # also use buildIdris' here and not specify `idrisLibraries`
          # explicitly.
          mydb=builtins.fromJSON (builtins.readFile ./pkgs.json);
          myPkgPy = buildIdris {
            ipkgName = "py";
            src = ./.;
            idrisLibraries = with idris2Packages; [
              packdb.ncurses-idris packdb.rhone-js packdb.json packdb.tailrec packdb.sop  packdb.idris2 #idris2f.packages.${system}.idris2.
            ];
          };
          myPkgPyDoc = buildIdris {
            ipkgName = "py_doc";
            src = ./.;
            idrisLibraries = with idris2Packages; [
              packdb.ncurses-idris packdb.rhone-js packdb.json packdb.tailrec packdb.sop  packdb.idris2 #idris2f.packages.${system}.idris2.
            ];
          };

          
        in
          rec {
            idris2-python-pure = myPkgPy.executable;
            py_doc = myPkgPyDoc.library';
            default = idris2-python-pure; #myPkg.executable; # or myPkg.library'
          } 
      );

      devShells = forEachSystem (
        {
          system,
          pkgs,
          idris2,
          idris2Lsp,
          ...
        }:
        {
          default = pkgs.mkShell {
            #packages = [
            #  idris2
            #  idris2Lsp
            #];
            inputsFrom = [ self.packagesIdris2.${system}.default.withSource
                           self.packagesIdris2.${system}.py_doc.withSource idris2 idris2Lsp];
          };
        }
      );
    };

  nixConfig = {
    extra-substituters = [
      "https://gh-nix-idris2-packages.cachix.org"
    ];
  };
}
