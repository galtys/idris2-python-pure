{stdenv}:stdenv.mkDerivation {pname="py_support";
                              version="0.0.0";
                              phases = [ "unpackPhase" "installPhase" ];
                              installPhase = ''
                                mkdir -p $out/share/py
                                cp $src/* $out/share/py

                              '';                              
                              src=./PySupport;}
