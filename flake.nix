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
            myidris2f = idris2f.packages.${system}.default;
          }
        );
    in
    {
      packages = forEachSystem (
        { buildIdris, idris2Packages,idris2,myidris2f,pkgs,system,... }:
        let
          # if you have 'allow-import-from-derivation' set true then you could
          # also use buildIdris' here and not specify `idrisLibraries`
          # explicitly.
          mydb=builtins.fromJSON (builtins.readFile ./pkgs.json);
          
          #myidris2f.propagatedIdrisLibraries=[];
          myPkgPyDoc = buildIdris {
            ipkgName = "py_doc";
            version = "0.1.0";            
            src = ./.;
            idrisLibraries = with idris2Packages; [
              packdb.ncurses-idris packdb.rhone-js packdb.json packdb.tailrec packdb.sop  packdb.idris2  #idris2f.packages.${system}.idris2.
            ];
          };


          pysupport =
            import ./py_support.nix {stdenv=pkgs.stdenv;};

          name = "${idris2.pname}-${idris2.version}";
          globalLibraries = [
            "\\$HOME/.nix-profile/lib/${name}"
            "/run/current-system/sw/lib/${name}"
            "${idris2}/${name}"
          ];
          globalLibrariesPath = builtins.concatStringsSep ":" globalLibraries;
          supportLibrariesPath = lib.makeLibraryPath [ pysupport ];
          supportSharePath = lib.makeSearchPath "share" [ pysupport ];
          
          myPkgPy = buildIdris {
            ipkgName = "idris2-python-pure";
            version = "0.1.0";
            src = ./.;
             
            idrisLibraries = with idris2Packages; [
              packdb.ncurses-idris packdb.rhone-js packdb.json packdb.tailrec packdb.sop packdb.idris2  myPkgPyDoc
            ];
            postFixup = ''
                wrapProgram $out/bin/idris2-python-pure \
                  --run 'export IDRIS2_PREFIX=''${IDRIS2_PREFIX-"$HOME/.idris2"}' \
                  --suffix IDRIS2_LIBS ':' "${supportLibrariesPath}" \
                  --suffix IDRIS2_DATA ':' "${supportSharePath}" \
                  --suffix IDRIS2_PACKAGE_PATH ':' "${globalLibrariesPath}" \
            '';  
            
          };

          myPkgIdris2 = buildIdris {
            ipkgName = "idris2";
            version = "0.7.0";
            src = ./.;
             
            idrisLibraries = with idris2Packages; [
               packdb.ncurses-idris packdb.rhone-js packdb.json packdb.tailrec packdb.sop packdb.idris2  myPkgPyDoc
            ];
          };
          

          
        in
          {
            idris2-python-pure = myPkgPy.executable;
            py_doc = myPkgPyDoc.library';
            idris2 = myPkgIdris2.executable;
            default = myPkgPy.executable; #myPkg.executable; # or myPkg.library'
          } 
      );

      devShells = forEachSystem (
        {
          system,
          pkgs,
          idris2,
          idris2Lsp,
          myidris2f,
          ...
        }:
        rec {
          #                   
          #pysupport =
          #  import ./py_support.nix {stdenv=pkgs.stdenv;};
          
          default = pkgs.mkShell {
            # shellHook=''
            #        export IDRIS2_PREFIX=.idris2
            #        export IDRIS2_LIBS=${pysupport.out}  
            #        IDR2_PTH=${myidris2f.out}/idris2-0.7.0
            #        export IDRIS2_PACKAGE_PATH=$IDRIS2_PACKAGE_PATH:$IDR2_PTH
            # '';
            packages = [
              idris2
              self.packages.${system}.default
              idris2Lsp
              pkgs.gnumake
            ]; #executables available in the dev shell
            inputsFrom = [ self.packages.${system}.default.withSource
                           self.packages.${system}.py_doc.withSource];
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
