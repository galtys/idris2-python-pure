NAME = pybackend
all: support odoo

support: 
	mkdir -p `./build/exec/pybackend --libdir`/support/py/
	install src/Py/py_support.py `build/exec/pybackend --libdir`/support/py/
	install src/Py/run_main.py `build/exec/pybackend --libdir`/support/py/
