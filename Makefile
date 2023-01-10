NAME = pybackend
TARGETDIR = ${CURDIR}/build/exec
TARGET = ${TARGETDIR}/${NAME}
PREFIX ?= $(HOME)/.idris2-pygen

all: pygen support install-pybackend

pygen:  py.ipkg
	idris2 --build py.ipkg
support: pygen
	mkdir -p `./build/exec/pybackend --libdir`/support/py/
	install src/Py/py_support.py `build/exec/pybackend --libdir`/support/py/
	install src/Py/run_main.py `build/exec/pybackend --libdir`/support/py/
install-pybackend:
	mkdir -p ${PREFIX}/bin
	install ${TARGET} ${PREFIX}/bin
test:
	./build/exec/pybackend --directive "test_import.py" --build hello.ipkg
#prelude:
#	${MAKE} -C libs/prelude IDRIS2=${TARGET} IDRIS2_INC_CGS=${IDRIS2_CG} IDRIS2_PATH=${IDRIS2_BOOT_PATH}

#base: prelude
#	${MAKE} -C libs/base IDRIS2=${TARGET} IDRIS2_INC_CGS=${IDRIS2_CG} IDRIS2_PATH=${IDRIS2_BOOT_PATH}
