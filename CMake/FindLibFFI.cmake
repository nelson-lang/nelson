find_package(PkgConfig)
PKG_CHECK_MODULES(PC_LIBFFI QUIET libffi)

find_path(LIBFFI_INCLUDE_DIR
  ffi.h
  HINTS
  ${PC_LIBFFI_INCLUDEDIR}
)

if(LIBFFI_INCLUDE_DIR)
  find_library(LIBFFI_LIBRARY
    NAMES
    ffi
    HINTS
    ${PC_LIBFFI_LIBDIR}
    ${PC_LIBFFI_LIBRARY_DIRS}
  )
endif()

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(
  LibFFI
  DEFAULT_MSG
  LIBFFI_LIBRARY
  LIBFFI_INCLUDE_DIR
)

if(NOT LIBFFI_INCLUDE_DIR)
  if(EXISTS "/usr/include/ffi/ffi.h")
    set(LIBFFI_INCLUDE_DIR /usr/include/ffi)
  endif()
endif()
if(NOT LIBFFI_INCLUDE_DIR)
  if(EXISTS "$ENV{HOMEBREW_PREFIX}/opt/libffi/include/ffi.h")
    set(LIBFFI_INCLUDE_DIR "$ENV{HOMEBREW_PREFIX}/opt/libffi/include")
  endif()
endif()
if(NOT LIBFFI_INCLUDE_DIR)
  if(EXISTS "/usr/local/opt/libffi/include/ffi.h")
    set(LIBFFI_INCLUDE_DIR /usr/local/opt/libffi/include)
  endif()
endif()
if(NOT LIBFFI_INCLUDE_DIR)
  if(EXISTS "usr/lib/libffi-3.2.1/include/ffi.h")
    set(LIBFFI_INCLUDE_DIR "usr/lib/libffi-3.2.1/include")
  endif()
endif()
if(NOT LIBFFI_INCLUDE_DIR)
  if(EXISTS "/usr/include/x86_64-linux-gnu/ffi.h")
    set(LIBFFI_INCLUDE_DIR "/usr/include/x86_64-linux-gnu")
  endif()
endif()
mark_as_advanced(LIBFFI_INCLUDE_DIR LIBFFI_LIBRARY)
