# Pure python backend for idris2

This is still an experimental, pure python backend for the idris2 compiler based on the javascript backend. Just like the javascript backend, it generates a single python file. 

# How to build it

In order to build it, you must have the selfhosted version of idris2 and idris2api build and installed. 

The proven way is to build it via pack:

`pack build py.ipkg`

In the misc folder, there is a bash script that you can adapt to install additional packages (including prelude,base ..). At the moment, the prefix of he backend points to .idris2-pygen

# Background

The purpose of this project wasn't to build the python bakcend but to learn how idris2 works under the hood. But eventually, I was able to adapt the javascript backend and with some tweaks, it actually generates python code. 

I know there is another python backend based on refc, however , I believe the used case for the pure python version is that there just one garbage collector and for my purposes,  pure python ffi has its own benefits too.

# Next step

In the next iteration, the --directive will take a name of a support_file without the py extension. This can be usefull if you need to bundle the result with this extra support file. 

# TODO:

- tests
- lazy support
- generate better code for case expressions
- get rid of comments in the generated code
- ffi? 

Feel free to raise issues or ask questions. 
