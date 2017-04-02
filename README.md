fortran_pcre
============

A very basic wrapper around the PCRE regex C library. Use at your own risk!

This contains the absolute bare minimum to be the least bit useful:

- `pcre_compile`: to compile a regex
- `pcre_exec`: to match a regex
- `pcre_fullinfo`: to get info on a match
- a bunch of constants to pass to `pcre_fullinfo`

`test.f90` is basically a minimal port of `pcredemo.c`

Bugs/gotchas
------------

- some character classes like `\s` and `\d` don't work. No idea why.
- super minimal functionality at the mo


TODO
----

Pull requests more than welcome

- wrap the rest of the API
- add module for passing strings between C/Fortran
- add the rest of the constants/hex flags
- move constants into separate module
- add tests!
- make object-oriented?


