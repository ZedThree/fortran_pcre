cmake_minimum_required(VERSION 3.13)

enable_language(Fortran)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON) # Show your work...

# Set maximum processors to use as N_CORES = max(1, <number of CPUs> - 1)
include(ProcessorCount)
ProcessorCount(NCPU)
if(NCPU GREATER 1)
  math(EXPR N_CORES "${NCPU} - 1")
else()
  set(N_CORES, 1)
endif()

# Set project name and language
project(fsl_pcre LANGUAGES Fortran)

# Allow automated retrieval and building of external dependencies
include(ExternalProject)
include(FetchContent)

# CTest setup
include(CTest) # Needed for valgrind
enable_testing()

# Set package metadata
set(CPACK_PACKAGE_NAME "fsl_pcre")
set(CPACK_PACKAGE_VERSION_MAJOR "0")
set(CPACK_PACKAGE_VERSION_MINOR "1")
set(CPACK_PACKAGE_VERSION_PATCH "0")
set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Fortran wrapper around PCRE regular expression library")
set(CPACK_PACKAGE_VENDOR "Acorvid Technical Services Corp.")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
# set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/Copyright.txt")
# RAA: What is this? Base installation directory for NSIS installer?
set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}_${CPACK_PACKAGE_VERSION}")
# set(CPACK_SET_DESTDIR true)
# set(CPACK_INSTALL_PREFIX /opt/${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION})

### Build tool discovery
if(MSVC)
  find_program(NMAKE_EXECUTABLE NAMES nmake)
else(MSVC)
  find_program(MAKE_EXECUTABLE NAMES make gmake)
endif(MSVC)

# Get the name of the actual Fortran compiler (gfortran, g90, ifort, etc.)
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

# Set up Doxygen for documenting library
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/SetupDoxygen.cmake")

#OK ### Valgrind memory usage analysis tool
#OK find_program(MEMORYCHECK_COMMAND valgrind)
#OK if(MEMORYCHECK_COMMAND)
#OK   message(STATUS "Found memory checker: ${MEMORYCHECK_COMMAND}")
#OK   set(MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full")
#OK 
#OK   include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/AddMemcheckTest.cmake")
#OK # TODO: Verify this doesn't choke on Windows. Does VS have a memory analyzer? Does it work with CMake?
#OK   add_custom_target(test_memcheck
#OK                     COMMAND ${CMAKE_CTEST_COMMAND} -C Release
#OK                             --force-new-ctest-process --test-action memcheck
#OK                     COMMAND cat "${CMAKE_BINARY_DIR}/Testing/Temporary/MemoryChecker.*.log")
#OK else()
#OK   message(STATUS "Cannot find memory checker - skipping memcheck static analysis")
#OK endif()

### PCRE regular expression library
message(STATUS "Enable PCRE")
option(PCRE_IS_PREBUILT "Use prebuilt PCRE library (vs compiling from source)" OFF)
if(PCRE_IS_PREBUILT)
  # TODO: Needs implementation
  message(STATUS "Prebuilt PCRE detection not implemented")
#  include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/FindPCRE.cmake")
else()
  include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/BuildPCRE.cmake")
endif()

# Add compiler flags
#! if(MSVC) # MSVC-specific options
#!   message(STATUS "MSVC detected, adding compile flags")
#!   add_compile_options("/W3"
#!                       "/permissive-")
#! #  add_compile_options("/Wall")
#! 
#!   if(CMAKE_BUILD_TYPE MATCHES "Debug")
#!       message("** Windows build configured for Debug")
#!   else()
#!       message("** Windows build configured for Release")
#!   endif()
#! else() # Not MSVC

  if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
    # gfortran
    set(CMAKE_Fortran_FLAGS         "-ffpe-trap=invalid,zero,overflow -fbacktrace -fno-automatic -finit-local-zero -Wall -pedantic")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-fbounds-check -Og -pg -g")
  elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
    # ifort
    set (CMAKE_Fortran_FLAGS         "-warn all -zero -fpe0")
    set (CMAKE_Fortran_FLAGS_RELEASE "-O3")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -pg -check bounds -g")
  else ()
    # unknown compiler
    message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
    message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
    message ("No optimized Fortran compiler flags are known, we just try -O2...")
    set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-Wall -O0 -pg")
  endif ()

  if(CMAKE_BUILD_TYPE MATCHES "Debug")
    message("** Linux build configured for Debug")
# Configure code test coverage analysis
#OKCXX    include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/CodeCoverage.cmake")
#OKCXX    if(LCOV_PATH)
#OKCXX      APPEND_COVERAGE_COMPILER_FLAGS()
#OKCXX    else()
#OKCXX      message(STATUS "lcov not detected; not setting code coverage compilation flags")
#OKCXX    endif()
  else()
    message("** Linux build configured for Release")
  endif()
  #! endif()

# Set source and include prefix if needed
set(FSL_PCRE_SOURCE_DIR  "${CMAKE_SOURCE_DIR}/fsl_pcre/src")
#OK set(FSL_PCRE_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/fsl_pcre/include")

# Bring the headers into the project
#OK include_directories(${FSL_PCRE_INCLUDE_DIR})

# Define headers for fsl_pcre library
#OKCXX set(FSL_PCRE_INCLUDES
#OKCXX     "${FSL_PCRE_INCLUDE_DIR}/DoesNotExist.hpp"
#OKCXX     )

# Define sources for fsl_pcre library
set(FSL_PCRE_SOURCES
    "${FSL_PCRE_SOURCE_DIR}/pcre_constants.f90"
    "${FSL_PCRE_SOURCE_DIR}/pcre_module.f90"
    )

# Define Fortran module directory (output location)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/finclude)

# Define modules (built) for fsl_pcre library
set(FSL_PCRE_MODULES
    "${CMAKE_Fortran_MODULE_DIRECTORY}/pcre_constants.mod"
    "${CMAKE_Fortran_MODULE_DIRECTORY}/pcre_module.mod"
    )

# Adjust compiled code extensions to simplify test coverage analysis
#OK set(CMAKE_CXX_OUTPUT_EXTENSION_REPLACE ON)

# Define PCRE library artifact
# Q: Do we need to include the generated module files here?
add_library(fsl_pcre
#            SHARED
            STATIC
            ${FSL_PCRE_SOURCES}
#            ${FSL_PCRE_INCLUDES}
)
add_dependencies(fsl_pcre pcre)

# Set compilation options for library target
target_compile_options(fsl_pcre PUBLIC)

# For standard library
set(FSL_PCRE_LINK_LIBRARIES ${FSL_PCRE_LINK_LIBRARIES} ${PCRE_LIBRARIES})

list(REMOVE_DUPLICATES FSL_PCRE_LINK_LIBRARIES)
target_link_libraries(fsl_pcre PUBLIC ${FSL_PCRE_LINK_LIBRARIES})

###############################################################################
## Analysis ###################################################################
###############################################################################

#OKCXX option(RUN_CPPCHECK "Run cppcheck analysis" OFF)
#OKCXX 
#OKCXX if (RUN_CPPCHECK)
#OKCXX   # Run cppcheck if available
#OKCXX   find_program(CMAKE_CXX_CPPCHECK NAMES cppcheck)
#OKCXX   if (CMAKE_CXX_CPPCHECK)
#OKCXX     if (MSVC)
#OKCXX       set(CPPCHECK_PROJECT "${CMAKE_BINARY_DIR}/fsl_pcre.sln")
#OKCXX     else()
#OKCXX       set(CPPCHECK_PROJECT "${CMAKE_BINARY_DIR}/compile_commands.json")
#OKCXX     endif()
#OKCXX     list(
#OKCXX         APPEND CMAKE_CXX_CPPCHECK
#OKCXX             "--language=c++"
#OKCXX             "--enable=warning,style,performance,portability,information,missingInclude"
#OKCXX             "--inconclusive"
#OKCXX             "--force"
#OKCXX             "--inline-suppr"
#OKCXX             "--suppressions-list=${CMAKE_SOURCE_DIR}/CppCheckSuppressions.txt"
#OKCXX             "--output-file=${CMAKE_BINARY_DIR}/Testing/Temporary/cppcheck.log"
#OKCXX             "--project=${CPPCHECK_PROJECT}"
#OKCXX     )
#OKCXX   #          "-I/usr/include"
#OKCXX   #          "-I/usr/local/include"
#OKCXX   #ok
#OKCXX   #          "-I${CMAKE_SOURCE_DIR}/${FSL_PCRE_INCLUDE_DIR}"
#OKCXX   #          "${CMAKE_SOURCE_DIR}/${FSL_PCRE_SOURCE_DIR}"
#OKCXX     add_custom_target(cppcheck
#OKCXX                       COMMAND ${CMAKE_CXX_CPPCHECK}
#OKCXX                       COMMAND cat "${CMAKE_BINARY_DIR}/Testing/Temporary/cppcheck.log"
#OKCXX     )
#OKCXX   else()
#OKCXX     message(STATUS "Cannot find cppcheck - skipping cppcheck static analysis")
#OKCXX   endif()
#OKCXX endif (RUN_CPPCHECK)

###############################################################################
## Testing ####################################################################
###############################################################################

### Custom tests

# Set test source directory prefix
set(TSRC "${CMAKE_SOURCE_DIR}/test/src")

# Acceptance tests

set(FSL_PCRE_DEP_LINK_LIBRARIES ${FSL_PCRE_LINK_LIBRARIES})

# Benchmark verification test 1
set(ATEST_SOURCES ${FSL_PCRE_SOURCES} "${TSRC}/test.f90")
add_executable(atest_1 ${ATEST_SOURCES})
add_dependencies(atest_1 pcre)
target_link_libraries(atest_1 ${FSL_PCRE_DEP_LINK_LIBRARIES})
add_test(NAME atest_1
         COMMAND atest_1
         CONFIGURATIONS Debug Release
)
list(APPEND ATEST_EXECUTABLES atest_1)

# Benchmark verification test 3
set(ATEST_SOURCES ${FSL_PCRE_SOURCES} "${TSRC}/test3.f90")
add_executable(atest_3 ${ATEST_SOURCES})
add_dependencies(atest_3 pcre)
target_link_libraries(atest_3 ${FSL_PCRE_DEP_LINK_LIBRARIES})
add_test(NAME atest_3
         COMMAND atest_3
         CONFIGURATIONS Debug Release
)
list(APPEND ATEST_EXECUTABLES atest_3)

# Non-functional and regressions tests


#OKCXX if(LCOV_PATH)
#OKCXX   message(STATUS "lcov detected; setting up CTest coverage target")
#OKCXX   set(COVERAGE_LCOV_EXCLUDES "${CMAKE_CURRENT_SOURCE_DIR}/${FSL_PCRE_SOURCE_DIR}/main.cpp"
#OKCXX                              "${CMAKE_CURRENT_SOURCE_DIR}/ext_lib/*"
#OKCXX                              '/usr/include/*'
#OKCXX                              '/usr/local/include/*')
#OKCXX   if(CMAKE_BUILD_TYPE MATCHES "Debug")
#OKCXX     SETUP_TARGET_FOR_COVERAGE_LCOV(
#OKCXX         NAME coverage
#OKCXX #        EXECUTABLE ctest -C Release -j ${N_CORES} # Executable in PROJECT_BINARY_DIR
#OKCXX         EXECUTABLE ctest -C Debug -j ${N_CORES} # Executable in PROJECT_BINARY_DIR
#OKCXX ## General ctest use (no -C)
#OKCXX #        EXECUTABLE ctest -j ${N_CORES} # Executable in PROJECT_BINARY_DIR
#OKCXX #        LCOV_ARGS --no-external
#OKCXX         DEPENDENCIES
#OKCXX             atest_1
#OKCXX             atest_3
#OKCXX     )
#OKCXX   endif()
#OKCXX endif()

###############################################################################
## Packaging ##################################################################
###############################################################################

# All install commands get the same destination. this allows us to use paths
# relative to the executable.

# For documentation and license
install(FILES README.md LICENSE.md
#        DESTINATION doc
        DESTINATION share/fsl_pcre
        COMPONENT documentation)
set(CPACK_COMPONENT_DOCUMENTATION_DISPLAY_NAME "Documentation")
set(CPACK_COMPONENT_DOCUMENTATION_DESCRIPTION
    "Documentation and licensing")

#! # For shared library:
#! # install(TARGETS fsl_pcre DESTINATION /usr/lib)
#! # For shared object/"runtime" (should RUNTIME be LIBRARY instead?)
#! install(TARGETS fsl_pcre
#!         RUNTIME
#!         DESTINATION lib
#!         COMPONENT libraries)

# For static library/"archive"
install(TARGETS fsl_pcre
        ARCHIVE
        DESTINATION lib
        COMPONENT libraries)
set(CPACK_COMPONENT_LIBRARIES_DISPLAY_NAME "Libraries")
set(CPACK_COMPONENT_LIBRARIES_DESCRIPTION
    "Libraries used to build programs with fsl_pcre")

# For Fortran source files
# Note use of FILES and DESTINATION instead of pulling the whole include
# directory
install(FILES ${FSL_PCRE_SOURCES}
        DESTINATION src
        COMPONENT source)
set(CPACK_COMPONENT_SOURCE_DISPLAY_NAME "Fortran source")
set(CPACK_COMPONENT_SOURCE_DESCRIPTION
   "Fortran source files for use with fsl_pcre")

# For Fortran module files
# Note use of FILES and DESTINATION instead of pulling the whole include
# directory
install(FILES ${FSL_PCRE_MODULES}
        DESTINATION finclude
        COMPONENT modules)
set(CPACK_COMPONENT_MODULES_DISPLAY_NAME "Fortran modules")
set(CPACK_COMPONENT_MODULES_DESCRIPTION
    "Compiler-specific Fortran module files for use with fsl_pcre")

#OKCXX # For C++ header (include) files:
#OKCXX # Note use of FILES and DESTINATION instead of pulling the whole include
#OKCXX # directory; there were issues with the directory being installed as
#OKCXX # include/include and certain headers (Aria CSV) are used only for building
#OKCXX # test applications, not for the production library.
#OKCXX install(FILES ${FSL_PCRE_INCLUDES}
#OKCXX         DESTINATION include
#OKCXX         COMPONENT headers)
#OKCXX set(CPACK_COMPONENT_HEADERS_DISPLAY_NAME "C++ Headers")
#OKCXX set(CPACK_COMPONENT_HEADERS_DESCRIPTION
#OKCXX     "C++ header files for use with fsl_pcre")

# For main() executable and Boost unit test executables:
# Note that each executable name is individually added to the lists
# ATEST_EXECUTABLES and BTEST_EXECUTABLES
install(TARGETS ${ATEST_EXECUTABLES} #${BTEST_EXECUTABLES}
        RUNTIME
        DESTINATION share/fsl_pcre
        COMPONENT applications)
set(CPACK_COMPONENT_APPLICATIONS_DISPLAY_NAME "Test and benchmark applications")
set(CPACK_COMPONENT_APPLICATIONS_DESCRIPTION
    "fsl_pcre test and benchmark applications")

# For reference data used by test applications
#! install(DIRECTORY test/data
#!         DESTINATION share/fsl_pcre
#!         COMPONENT sampledata)
#! set(CPACK_COMPONENT_SAMPLEDATA_DISPLAY_NAME "Test and benchmark data")
#! set(CPACK_COMPONENT_SAMPLEDATA_DESCRIPTION
#!     "fsl_pcre test and benchmark data")

#OLDER We don't want to split our program up into several incomplete pieces.
# set(CPACK_MONOLITHIC_INSTALL 1)

# We want all components to ship in a single installer
set(CPACK_COMPONENTS_GROUPING "ALL_COMPONENTS_IN_ONE")

# Package the following set of components (see COMPONENT tag on each INSTALL
# directive)
#! set(CPACK_COMPONENTS_ALL libraries headers applications sampledata)
set(CPACK_COMPONENTS_ALL documentaton source libraries modules applications)

# Set installation dependencies
set(CPACK_COMPONENT_LIBRARIES_DEPENDS documentation)
set(CPACK_COMPONENT_SOURCE_DEPENDS documentation)
set(CPACK_COMPONENT_MODULES_DEPENDS documentation source libraries)
#! set(CPACK_COMPONENT_HEADERS_DEPENDS libraries)
#! set(CPACK_COMPONENT_APPLICATIONS_DEPENDS sampledata libraries)
set(CPACK_COMPONENT_APPLICATIONS_DEPENDS documentation libraries)

# Set installation groups
set(CPACK_COMPONENT_SOURCE_GROUP "Development")
set(CPACK_COMPONENT_LIBRARIES_GROUP "Development")
set(CPACK_COMPONENT_MODULES "Development")
# set(CPACK_COMPONENT_HEADERS_GROUP "Development")
set(CPACK_COMPONENT_APPLICATIONS_GROUP "Verfication")
#! set(CPACK_COMPONENT_SAMPLEDATA_GROUP "Verfication")

set(CPACK_COMPONENT_GROUP_DEVELOPMENT_DESCRIPTION
    "Libraries needed to develop applications with fsl_pcre")
#    "Libraries and headers needed to develop applications with fsl_pcre")
set(CPACK_COMPONENT_GROUP_VERIFICATION_DESCRIPTION
    "Test and benchmark applications")
#!    "Test and benchmark applications and data")

# NSIS-only installation "types"
# Q: Should this "install type" section be moved under Windows-specific
# installation options?
set(CPACK_ALL_INSTALL_TYPES Full Developer)
set(CPACK_COMPONENT_DOCUMENTATION_INSTALL_TYPES Developer Full)
set(CPACK_COMPONENT_SOURCE_INSTALL_TYPES Developer Full)
set(CPACK_COMPONENT_LIBRARIES_INSTALL_TYPES Developer Full)
set(CPACK_COMPONENT_MODULES_INSTALL_TYPES Developer Full)
#OKCXX set(CPACK_COMPONENT_HEADERS_INSTALL_TYPES Developer Full)
set(CPACK_COMPONENT_APPLICATIONS_INSTALL_TYPES Full)
#! set(CPACK_COMPONENT_SAMPLEDATA_INSTALL_TYPES Full)

if(WIN32 AND NOT UNIX)
  # Windows systems
  set(CPACK_GENERATOR "ZIP" "NSIS")

  # NSIS-specific settings
  set(CPACK_NSIS_DISPLAY_NAME "fsl_pcre")
  set(CPACK_NSIS_PACKAGE_NAME "fsl_pcre")
  set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)

  set(CPACK_NSIS_MUI_ICON "${PROJECT_SOURCE_DIR}/img/fsl_logo_32.ico")
  set(CPACK_NSIS_MUI_UNIICON "${PROJECT_SOURCE_DIR}/img/fsl_logo_32.ico")

  # NuGet-specific settings -- TBD

  # Enable once NuGet and WIX are installed/available
#  set(CPACK_GENERATOR "ZIP" "NSIS" "NuGet" "WIX")
else()
  # Debian-like systems
  set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Robert Apthorpe")
  set(CPACK_GENERATOR "ZIP" "TGZ" "TBZ2" "DEB")

  # Red Hat-like systems
  # set(CPACK_GENERATOR "ZIP" "TGZ" "TBZ2" "RPM")
endif()

# This must be last
include(CPack)
