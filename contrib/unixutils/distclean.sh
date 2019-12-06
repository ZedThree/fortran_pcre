#!/usr/bin/zsh

X_MAKE=/usr/bin/make
X_RM=/bin/rm

# Build artifacts
if [[ -f Makefile ]]; then
    $X_MAKE clean
    $X_RM Makefile
fi

# Expected files
for fn (
CMakeCache.txt
CMakeDoxyfile.in
CMakeDoxygenDefaults.cmake
cmake_install.cmake
compile_commands.json
CPackConfig.cmake
CPackSourceConfig.cmake
CTestTestfile.cmake
DartConfiguration.tcl
Doxyfile
Doxygen_Extensions.cfg
doxygen_footer.tex
doxygen_header.tex
doxygen_warnings.txt
DoxygenLayout.xml
doxygen.sty
gmon.out
install_manifest.txt
) {
    if [[ -f $fn ]]; then
        $X_RM $fn
    fi
}

# Expected directories
for dn (
_CPack_Packages
CMakeFiles
doc
finclude
pcre-artifacts
pcre-prefix
pcre-source
pcre2-artifacts
pcre2-prefix
pcre2-source
Testing
) {
    if [[ -d $dn ]]; then
        $X_RM -rf $dn
    fi
}

# Might fail since glob is unguarded and not guaranteed to find anything
for fn (fsl_pcre*-Linux.*) {
    if [[ -f $fn ]]; then
        $X_RM $fn
    fi
}
