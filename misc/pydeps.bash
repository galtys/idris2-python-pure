#!/usr/bin/env bash

PACK_DIR="${PACK_DIR:-$HOME/.pack}"
CLONES_DIR="/home/jan/github.com"

#pushd "$CLONES/Idris2/libs/prelude""
PYBACKEND="$CLONES_DIR/idris2-python-pure/build/exec/pybackend"
IDRIS_COMMIT="6b768f28b1aaaaca836e83014881ba72d613e568"

pushd "$CLONES_DIR/Idris2/libs/prelude"
rm -rf build
"$PYBACKEND" --install prelude.ipkg
popd

pushd "$CLONES_DIR/Idris2/libs/base"
rm -rf build
"$PYBACKEND" --install base.ipkg
popd

pushd "$CLONES_DIR/Idris2/libs/contrib"
rm -rf build
"$PYBACKEND" --install contrib.ipkg
popd

pushd "$CLONES_DIR/idris2-elab-util"
rm -rf build
git checkout 966a2f87c92b043d54b3710d3e86ad3667a8d615
"$PYBACKEND" --install elab-util.ipkg
popd

pushd "$CLONES_DIR/idris2-sop"
rm -rf build
git checkout 0726a1154b55ba0cefb885962a726c847b3d8d43
"$PYBACKEND" --install sop.ipkg
popd

pushd "$CLONES_DIR/idris2-tailrec"
rm -rf build
git checkout d3747284c7e80a711464ec4b1a7976bcf39dd6f2
"$PYBACKEND" --install tailrec.ipkg
popd

###############

pushd "$CLONES_DIR/idris2-parser"
rm -rf build
git checkout eb26b51140668f266f41588122ffbb0ac9c3bb54
"$PYBACKEND" --install parser.ipkg
popd

pushd "$CLONES_DIR/idris2-parser"
rm -rf build
git checkout eb26b51140668f266f41588122ffbb0ac9c3bb54
"$PYBACKEND" --install json/parser-json.ipkg
popd

###############

pushd "$CLONES_DIR/idris2-json"
rm -rf build
git checkout fa0ba6c6b9e940d917159a3ab1a1d9a42171935e
"$PYBACKEND" --install json.ipkg
popd

pushd "$CLONES_DIR/idris2-python-pure"

#git checkout fa0ba6c6b9e940d917159a3ab1a1d9a42171935e
"$PYBACKEND" --install py_doc.ipkg
popd



pushd "$CLONES_DIR/idris2-quantifiers-extra"
rm -rf build
git checkout 0bd29837979d8dba034766d197f0ac8c351b0c68
"$PYBACKEND" --install quantifiers-extra.ipkg
popd

pushd "$CLONES_DIR/idris2-rhone"
rm -rf build
git checkout 28ca640a75064fb2bb19869b1f647a958e3451df
"$PYBACKEND" --install rhone.ipkg
popd

pushd "$CLONES_DIR/idris2-algebra"
rm -rf build
git checkout 1172ed5b4848c5e18f7bf5f9bb467bc3e1f6b7e9
"$PYBACKEND" --install algebra.ipkg
popd


pushd "$CLONES_DIR/idris2-refined"
rm -rf build
git checkout 4c7a684498f64f3da0ab7b0b92a1f3825cd60e86
"$PYBACKEND" --install refined.ipkg
popd

pushd "$CLONES_DIR/idris2-dom"
rm -rf build
git checkout 9b68ee497679cd1a7f085f22901506a62f33f13f
"$PYBACKEND" --install js/js.ipkg
popd


pushd "$CLONES_DIR/idris2-dom"
rm -rf build
git checkout 9b68ee497679cd1a7f085f22901506a62f33f13f
"$PYBACKEND" --install dom.ipkg
popd

pushd "$CLONES_DIR/idris2-rhone-js"
rm -rf build
git checkout 2f4f80aa32d60142349672019e9186eea91bcf1e
"$PYBACKEND" --install rhone-js.ipkg
popd

