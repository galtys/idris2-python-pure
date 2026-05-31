NAME = pybackend
all: support php-support

support:
	mkdir -p `./build/exec/pybackend --libdir`/support/py/
	install src/Py/py_support.py `build/exec/pybackend --libdir`/support/py/
	install src/Py/run_main.py `build/exec/pybackend --libdir`/support/py/

php-support:
	mkdir -p `./build/exec/idris2-php8 --libdir`/support/php/
	install src/PHP/php_support.php `./build/exec/idris2-php8 --libdir`/support/php/
	install src/PHP/run_main.php `./build/exec/idris2-php8 --libdir`/support/php/
