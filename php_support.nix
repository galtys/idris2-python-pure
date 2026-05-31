{stdenv}:stdenv.mkDerivation {pname="php_support";
                              version="0.0.0";
                              phases = [ "unpackPhase" "installPhase" ];
                              installPhase = ''
                                mkdir -p $out/share/php
                                cp $src/* $out/share/php
                              '';
                              src=./PHPSupport;}
