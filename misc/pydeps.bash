#!/usr/bin/env bash

PACK_DIR="${PACK_DIR:-$HOME/.pack}"
CLONES_DIR="/home/jan/github.com"

#pushd "$CLONES/Idris2/libs/prelude""
PYBACKEND="$CLONES_DIR/idris2-python-pure2/build/exec/pybackend"
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



