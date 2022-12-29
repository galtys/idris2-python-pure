# Pure python backend for idris2

This is still an experimental, pure python backend for the idris2 compiler based on the javascript backend. Just like the javascript backend, it generates a single python file. 

# How to build it

In order to build it, you must have the selfhosted version of idris2 and idris2api build and installed. 

The proven way is to setup an idris2 environment via a nix flake. Just go to the idris2 development directory and run `nix develop`. Then make sure that idris2 executable is in your search path.

After compiling the backend, you will need to use it to compile and install `prelude_pygen` and `base_pygen` packages found in the `libs` directory. They have been modified to support `pygen` ffi.


# Background

The purpose of this project wasn't to build the python bakcend but to learn how idris2 works under the hood. But eventually, I was able to adapt the javascript backend and with some tweaks, it actually generates python code. Currently, the generated code is inefficient (look at how it represents utf8 characters). 

I know there is another python backend based on refc, however I have realised that having a pure python version is quite neat as there is just one garbage collector and for my purposes,  pure python ffi has its own benefits too.

# TODO:
- tests
- utf8
- lazy support
- generate better code for case expressions
- get rid of comments in the generated code
- ffi? 

Feel free to raise issues or ask questions. 
