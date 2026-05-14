# =====================================================================
# Prevent FindMKL from running ARM64
set(_is_arm64_vs_platform FALSE)
if(CMAKE_VS_PLATFORM_NAME STREQUAL "ARM64")
  return()
endif()

set(_is_arm64_processor FALSE)
if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(ARM64|arm64|AARCH64|aarch64)$")
  return()
endif()
# =====================================================================

# - Try to find mkl - the Intel Math Kernel Library
# Once done this will define
#  MKL_FOUND - System has mkl
#  MKL_INCLUDE_DIRS - The mkl include directories
#  MKL_LIBRARIES - The libraries needed to use mkl
#  MKL_DEFINITIONS - Compiler switches required for using mkl

if(WIN32 AND DEFINED ENV{MKL_DIR})
  set(_NELSON_WINDOWS_MKL_ROOT "$ENV{MKL_DIR}")
  set(
    MKL_INCLUDE_DIR
    "${_NELSON_WINDOWS_MKL_ROOT}/include"
    CACHE
      PATH
      "MKL include directory"
    FORCE
  )
  set(
    MKL_FFTW_INCLUDE_DIR
    "${_NELSON_WINDOWS_MKL_ROOT}/include"
    CACHE
      PATH
      "MKL FFTW include directory"
    FORCE
  )
  set(_NELSON_WINDOWS_MKL_BLAS
    "${_NELSON_WINDOWS_MKL_ROOT}/blas-lapack-wrapper/lib/libnlsblaslapack.lib"
  )
  set(_NELSON_WINDOWS_MKL_VML
    "${_NELSON_WINDOWS_MKL_ROOT}/vml/lib/libnlsvml_mkl.lib"
  )
  set(
    MKL_LIBRARIES
    "${_NELSON_WINDOWS_MKL_BLAS};${_NELSON_WINDOWS_MKL_VML}"
    CACHE
      STRING
      "Nelson MKL wrapper libraries"
    FORCE
  )
  set(
    MKL_CORE
    "${_NELSON_WINDOWS_MKL_BLAS}"
    CACHE
      FILEPATH
      "Nelson MKL core wrapper"
    FORCE
  )
  set(MKL_ILP "${MKL_LIBRARIES}" CACHE FILEPATH "Nelson MKL ILP wrapper" FORCE)
  set(
    MKL_SEQ
    "${MKL_LIBRARIES}"
    CACHE
      FILEPATH
      "Nelson MKL sequential wrapper"
    FORCE
  )
  set(MKL_STATIC_LIBRARIES "${MKL_LIBRARIES}")
  set(MKL_INCLUDE_DIRS ${MKL_INCLUDE_DIR} ${MKL_FFTW_INCLUDE_DIR})

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(
    MKL
    DEFAULT_MSG
    MKL_LIBRARIES
    MKL_INCLUDE_DIRS
  )

  if(MKL_FOUND)
    message(STATUS "Found MKL_INCLUDE_DIRS: ${MKL_INCLUDE_DIRS}")
    message(STATUS "Found MKL_LIBRARIES: ${MKL_LIBRARIES}")
  endif()

  mark_as_advanced(MKL_INCLUDE_DIR
    MKL_FFTW_INCLUDE_DIR
    MKL_LIBRARIES
    MKL_CORE
    MKL_ILP
    MKL_SEQ
  )
  return()
endif()

find_path(MKL_INCLUDE_DIR
  NAMES
  mkl.h
  HINTS
  $ENV{MKL_DIR}/include
  /opt/intel/oneapi/mkl/latest/include
  /opt/intel/mkl/include
  /usr/include/mkl
  PATHS
)

find_path(MKL_FFTW_INCLUDE_DIR
  NAMES
  fftw3.h
  HINTS
  $ENV{MKL_DIR}/include/fftw
  /opt/intel/oneapi/mkl/latest/include/fftw
  /opt/intel/mkl/include/fftw
  /usr/include/mkl/fftw
  PATHS
)

find_library(MKL_LIBRARIES
  NAMES
  mkl_rt
  HINTS
  $ENV{MKL_DIR}/lib/intel64
  /opt/intel/oneapi/mkl/latest/lib/intel64
  /opt/intel/mkl/lib/intel64
  PATHS
)

find_library(MKL_CORE
  NAMES
  libmkl_core.a
  HINTS
  $ENV{MKL_DIR}/lib/intel64
  /opt/intel/oneapi/mkl/latest/lib/intel64/
  /opt/intel/mkl/lib/intel64
  PATHS
)

find_library(MKL_ILP
  NAMES
  libmkl_intel_ilp64.a
  HINTS
  $ENV{MKL_DIR}/lib/intel64
  /opt/intel/oneapi/mkl/latest/lib/intel64/
  /opt/intel/mkl/lib/intel64
  PATHS
)

find_library(MKL_SEQ
  NAMES
  libmkl_sequential.a
  HINTS
  $ENV{MKL_DIR}/lib/intel64
  /opt/intel/oneapi/mkl/latest/lib/intel64/
  /opt/intel/mkl/lib/intel64
  PATHS
)

set(MKL_STATIC_LIBRARIES
  -Wl,--start-group
  ${MKL_CORE}
  ${MKL_ILP}
  ${MKL_SEQ}
  -Wl,--end-group
  -lpthread
  -lm
  -ldl
)
set(MKL_INCLUDE_DIRS ${MKL_INCLUDE_DIR} ${MKL_FFTW_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set MKL_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(
  MKL
  DEFAULT_MSG
  MKL_LIBRARIES
  MKL_CORE
  MKL_ILP
  MKL_SEQ
  MKL_INCLUDE_DIRS
)

if(MKL_FOUND)
  message(STATUS "Found MKL_INCLUDE_DIRS: ${MKL_INCLUDE_DIRS}")
  message(STATUS "Found MKL_LIBRARIES: ${MKL_LIBRARIES}")
  message(STATUS "Found MKL_STATIC_LIBRARIES: ${MKL_STATIC_LIBRARIES}")
endif()

mark_as_advanced(MKL_INCLUDE_DIR
  MKL_FFTW_INCLUDE_DIR
  MKL_LIBRARIES
  MKL_CORE
  MKL_ILP
  MKL_SEQ
)
