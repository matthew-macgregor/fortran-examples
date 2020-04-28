#!/bin/bash

# apt install gfortran-mingw-w64-x86-64
# compile with gcc in order to statically link quadmath and libgcc
x86_64-w64-mingw32-gcc $1 -o $1.exe -static-libgcc -Wl,-Bstatic -lgfortran -lquadmath -Wl,-Bdynamic -lm