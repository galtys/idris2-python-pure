{stdenv}:stdenv.mkDerivation {pname="php_support";
                              version="0.0.0";
                              phases = [ "unpackPhase" "installPhase" ];
                              installPhase = ''
                                mkdir -p $out
                                cp $src/* $out
                              '';
                              src=./PHPSupport;}
