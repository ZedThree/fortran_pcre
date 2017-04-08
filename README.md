fortran_pcre
============

A very basic wrapper around the PCRE regex C library. Use at your own risk!

This contains the absolute bare minimum to be the least bit useful:

- `pcre_compile`: to compile a regex
- `pcre_exec`: to match a regex
- `pcre_fullinfo`: to get info on a match
- a bunch of constants to pass to `pcre_fullinfo`

`test.f90` is basically a minimal port of `pcredemo.c`

`test3.f90` runs a whole bunch of regex tests

- Compile test like: `gfortran-7 -g -std=f2008 pcre_constants.f90 pcre_module.f90 test3.f90 -o test3 -lpcre`


Requirements
------------

- libpcre1
- Fortran compiler with decent F2008 support
    - Tested with gfortran 5.4.1, 6.2.1, 7.0.1

Bugs/gotchas
------------

- some character classes like `\s` and `\d` don't work. No idea why.
- super minimal functionality at the mo


TODO
----

Pull requests more than welcome!

- wrap the rest of the API
- add module for passing strings between C/Fortran
- add the rest of the constants/hex flags
- move constants into separate module
- add tests!
- make object-oriented?


