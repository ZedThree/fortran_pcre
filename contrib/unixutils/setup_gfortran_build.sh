#!/bin/sh
# Set the location of cmake
X_CMAKE=/usr/local/bin/cmake

# Set the compiler (cmake often guesses wrong...)
#           -DCMAKE_Fortran_COMPILER=gfortran \
#           -DCMAKE_Fortran_COMPILER=ifort \
#           -DCMAKE_Fortran_COMPILER=f95 \
# Set the release type (VS does both automatically)
#           -DCMAKE_BUILD_TYPE=Release \
#           -DCMAKE_BUILD_TYPE=Debug \
# Show verbose makefile diagnostics
#           -DCMAKE_VERBOSE_MAKEFILE:BOOL=OFF \
#           -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON \
${X_CMAKE} -DCMAKE_Fortran_COMPILER=/usr/bin/gfortran \
           -DCMAKE_BUILD_TYPE=Debug \
           -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON \
           ..
