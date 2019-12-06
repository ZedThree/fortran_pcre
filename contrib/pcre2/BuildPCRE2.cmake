#ok? set(PCRE2_STATIC TRUE CACHE BOOL "Build PCRE2 as local static library.")
option(PCRE2_STATIC "Build PCRE2 as local static library." ON)
# Unpack source archive to PCRE2_SOURCE_DIR
set(PCRE2_SOURCE_DIR "${CMAKE_CURRENT_BINARY_DIR}/pcre2-source")
# Locally install PCRE2 to PCRE2_LOCAL_INSTALL_DIR
set(PCRE2_LOCAL_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/pcre2-artifacts" CACHE PATH "PCRE2 local installation directory.")
# Adjust library and include directories as necessary
set(PCRE2_LOCAL_LIBRARY_DIR "${PCRE2_LOCAL_INSTALL_DIR}/lib" CACHE PATH "PCRE2 local library directory.")
set(PCRE2_LOCAL_INCLUDE_DIR "${PCRE2_LOCAL_INSTALL_DIR}/include" CACHE PATH "PCRE2 local include directory.")

set(PCRE2_CMAKE_ARGS "")
if(PCRE2_STATIC)
  message("   PCRE2 will be built as a position-independent static library")
  #  list(APPEND PCRE2_CMAKE_ARGS "-DSTATIC=on") 
  #  list(APPEND PCRE2_CMAKE_ARGS "-DCMAKE_POSITION_INDEPENDENT_CODE=ON") 
else()
  message("   PCRE2 will be built as a shared library")
  list(APPEND PCRE2_CMAKE_ARGS "-DBUILD_SHARED_LIBS=on") 
endif()
# Note: "<INSTALL_DIR>" is interpolated within ExternalProject_Add to PCRE2_LOCAL_INSTALL_DIR
list(APPEND PCRE2_CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>") 
# message("   PCRE2_STATIC = ${PCRE2_STATIC}")
# message("   PCRE2 CMake args include ${PCRE2_CMAKE_ARGS}")

# Adjust URL (local file or remote) and checksums accordingly
ExternalProject_Add(
  pcre2
#  URL         https://ftp.pcre.org/pub/pcre/pcre2-10.34.zip
  URL         ftp://ftp.pcre.org/pub/pcre/pcre2-10.34.zip
#  URL         "${CMAKE_CURRENT_SOURCE_DIR}/ext_lib/pcre2-1.3.0.zip"
  URL_HASH    SHA512=a9aa6caed23ef667952c703196c157de38b39bf5d482e3001a5bf56f58992d682d82461fce9e1ce11f80099998a4c8189169f05d0d63b9d0ef35d4a847e41c83
#  URL_HASH    SHA256=75a19013a10ab543fe923bbc9b7c2750559d2cca3b4c3611ab91392e5f62041c
#  URL_HASH    SHA1=b582e8667d8a57480d4848a56fdfd6b34f67dfad
#  URL_HASH    MD5=fdb10dba7f3be43730966bebdd3755ef
  SOURCE_DIR  "${PCRE2_SOURCE_DIR}"
  INSTALL_DIR "${PCRE2_LOCAL_INSTALL_DIR}"
  CMAKE_ARGS  ${PCRE2_CMAKE_ARGS}
)

message("++ PCRE2 will be built and installed to ${PCRE2_LOCAL_INSTALL_DIR}")
include_directories(${PCRE2_LOCAL_INCLUDE_DIR})

if(MSVC)
  if(PCRE2_STATIC)
    list(APPEND PCRE2_LIBRARIES "${PCRE2_LOCAL_LIBRARY_DIR}/pcre2.lib")
  else()
    list(APPEND PCRE2_LIBRARIES "${PCRE2_LOCAL_LIBRARY_DIR}/pcre2.dll")
  endif()
elseif(UNIX)
  if(PCRE2_STATIC)
    list(APPEND PCRE2_LIBRARIES "${PCRE2_LOCAL_LIBRARY_DIR}/libpcre2-8.a")
    list(APPEND PCRE2_LIBRARIES "${PCRE2_LOCAL_LIBRARY_DIR}/libpcre2-posix.a")
  else()
    list(APPEND PCRE2_LIBRARIES "${PCRE2_LOCAL_LIBRARY_DIR}/libpcre2.so")
  endif()
#  list(APPEND PCRE2_LIBRARIES m)
#  list(APPEND PCRE2_LIBRARIES pthread)
elseif(APPLE)
  list(APPEND PCRE2_LIBRARIES "${PCRE2_LOCAL_LIBRARY_DIR}/libpcre2.dylib")
endif()
