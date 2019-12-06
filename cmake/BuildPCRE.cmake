#ok? set(PCRE_STATIC TRUE CACHE BOOL "Build PCRE as local static library.")
option(PCRE_STATIC "Build PCRE as local static library." ON)
# Unpack source archive to PCRE_SOURCE_DIR
set(PCRE_SOURCE_DIR "${CMAKE_CURRENT_BINARY_DIR}/pcre-source")
# Locally install PCRE to PCRE_LOCAL_INSTALL_DIR
set(PCRE_LOCAL_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/pcre-artifacts" CACHE PATH "PCRE local installation directory.")
# Adjust library and include directories as necessary
set(PCRE_LOCAL_LIBRARY_DIR "${PCRE_LOCAL_INSTALL_DIR}/lib" CACHE PATH "PCRE local library directory.")
set(PCRE_LOCAL_INCLUDE_DIR "${PCRE_LOCAL_INSTALL_DIR}/include" CACHE PATH "PCRE local include directory.")

set(PCRE_CMAKE_ARGS "")
if(PCRE_STATIC)
  message("   PCRE will be built as a position-independent static library")
  #  list(APPEND PCRE_CMAKE_ARGS "-DSTATIC=on") 
  #  list(APPEND PCRE_CMAKE_ARGS "-DCMAKE_POSITION_INDEPENDENT_CODE=ON") 
else()
  message("   PCRE will be built as a shared library")
  list(APPEND PCRE_CMAKE_ARGS "-DBUILD_SHARED_LIBS=on") 
endif()
# Note: "<INSTALL_DIR>" is interpolated within ExternalProject_Add to PCRE_LOCAL_INSTALL_DIR
list(APPEND PCRE_CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>") 
# message("   PCRE_STATIC = ${PCRE_STATIC}")
# message("   PCRE CMake args include ${PCRE_CMAKE_ARGS}")

# Adjust URL (local file or remote) and checksums accordingly
ExternalProject_Add(
  pcre
#  URL         https://ftp.pcre.org/pub/pcre/pcre-8.43.zip
  URL         ftp://ftp.pcre.org/pub/pcre/pcre-8.43.zip
#  URL         "${CMAKE_CURRENT_SOURCE_DIR}/ext_lib/pcre-8.43.zip"
  URL_HASH    SHA512=e64d3b838551216bd7d593a3fed3180ae4098d745f47ee672f76c26f59834d23e415c7930e0d5660f625d4be3baba0702b5784be9ed24087dada1103aa9b8b6c
#  URL_HASH    SHA256=ae236dc25d7e0e738a94e103218e0085eb02ff9bd98f637b6e061a48decdb433
#  URL_HASH    SHA1=0d9c6446cec8adb064015a6b072b287325e79f1a
#  URL_HASH    MD5=42c0848153b8ddc7c7d19e4033f2a55b
  SOURCE_DIR  "${PCRE_SOURCE_DIR}"
  INSTALL_DIR "${PCRE_LOCAL_INSTALL_DIR}"
  CMAKE_ARGS  ${PCRE_CMAKE_ARGS}
)

message("++ PCRE will be built and installed to ${PCRE_LOCAL_INSTALL_DIR}")
include_directories(${PCRE_LOCAL_INCLUDE_DIR})

if(MSVC)
  if(PCRE_STATIC)
    list(APPEND PCRE_LIBRARIES "${PCRE_LOCAL_LIBRARY_DIR}/pcre.lib")
  else()
    list(APPEND PCRE_LIBRARIES "${PCRE_LOCAL_LIBRARY_DIR}/pcre.dll")
  endif()
elseif(UNIX)
  if(PCRE_STATIC)
    list(APPEND PCRE_LIBRARIES "${PCRE_LOCAL_LIBRARY_DIR}/libpcre.a")
    list(APPEND PCRE_LIBRARIES "${PCRE_LOCAL_LIBRARY_DIR}/libpcrecpp.a")
    list(APPEND PCRE_LIBRARIES "${PCRE_LOCAL_LIBRARY_DIR}/libpcreposix.a")
  else()
    list(APPEND PCRE_LIBRARIES "${PCRE_LOCAL_LIBRARY_DIR}/libpcre.so")
  endif()
#  list(APPEND PCRE_LIBRARIES m)
#  list(APPEND PCRE_LIBRARIES pthread)
elseif(APPLE)
  list(APPEND PCRE_LIBRARIES "${PCRE_LOCAL_LIBRARY_DIR}/libpcre.dylib")
endif()
