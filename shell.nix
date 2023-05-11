{ pkgs ? import <nixpkgs> {} }:
with pkgs;

let
   haskellDeps = ps: with ps; [
      base
      lens
      mtl
      data-fix
      groups
      postgresql-simple
      unordered-containers
      prettyprinter
      comonad
      parsec
      SVGFonts
      diagrams
      diagrams-contrib
      pptable
      tabl
      groupBy
    ];

   myghc = pkgs.haskellPackages.ghcWithPackages haskellDeps;

   p="mufum";
   mypython = pkgs.python3.buildEnv.override {
     extraLibs = with pkgs.python37Packages; [Babel chardet decorator docutils feedparser gevent greenlet html2text jinja2 lxml Mako markupsafe mock num2words ofxparse passlib pillow psutil psycopg2 pydot  pyparsing pypdf2 pyserial python-dateutil pytz pyusb qrcode reportlab requests suds-jurko vobject XlsxWriter xlwt xlrd polib  python-stdnum  watchdog unicodecsv  pysftp networkx protobuf websockify sphinx_rtd_theme  python-dateutil urllib3 certifi]; };
   
in 
#idris_sdl2 idris_sdl

stdenv.mkDerivation {
  name = "idris-env";
  LD_LIBRARY_PATH="${pkgs.postgresql_10.lib}/lib";
  buildInputs = [
    #postgresql_10
    gmp
    yarn
    yarn2nix
    sass
    myghc
    gdb
    inkscape
    qpdf
    #graphviz-nox
    graphviz
    chez
    pkgs.haskellPackages.cabal-install
    poppler_utils
    #mypython
    #pkgs.python3Packages.ipython
    
    nodejs
     (with nodePackages; [
       browserify
       js-beautify
       #foundation-sites
       bower
       gulp
       
     ])
    #SDL2.dev
    zulip.out
    SDL2.dev
    SDL2_image.out
    #postgresql_10.lib
    #postgresql_10
#    postgresql_10.out    
    pkg-config.out
    libpqxx
    ];
}
