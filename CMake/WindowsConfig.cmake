# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# Windows CMake builds intentionally coexist with the hand-maintained Visual
# Studio solution.  They generate their own build tree, but write final artifacts
# into the historical Nelson bin/<platform> directories.
# ==============================================================================
include_guard(GLOBAL)
# ==============================================================================
if(NOT MSVC)
  message(FATAL_ERROR "Windows CMake builds require MSVC.")
endif()

if(NOT CMAKE_GENERATOR MATCHES "Visual Studio (17 2022|18 2026)")
  message(FATAL_ERROR
    "Windows CMake builds require the Visual Studio 17 2022 or Visual Studio 18 2026 generator."
  )
endif()
# ==============================================================================
set(_nelson_windows_platform "${CMAKE_GENERATOR_PLATFORM}")
if(NOT _nelson_windows_platform AND DEFINED CMAKE_VS_PLATFORM_NAME)
  set(_nelson_windows_platform "${CMAKE_VS_PLATFORM_NAME}")
endif()

if(_nelson_windows_platform STREQUAL "x64")
  set(NELSON_WINDOWS_PLATFORM_NAME "x64")
  set(NELSON_WINDOWS_THIRDPARTY_PLATFORM "x64")
elseif(_nelson_windows_platform STREQUAL "ARM64")
  set(NELSON_WINDOWS_PLATFORM_NAME "ARM64")
  set(NELSON_WINDOWS_THIRDPARTY_PLATFORM "arm64")
else()
  message(FATAL_ERROR
    "Unsupported Visual Studio platform: '${_nelson_windows_platform}'. Use x64, Win32, or ARM64."
  )
endif()

set(_nelson_windows_is_arm64 OFF)
if(NELSON_WINDOWS_PLATFORM_NAME STREQUAL "ARM64")
  set(_nelson_windows_is_arm64 ON)
endif()
# ==============================================================================
set(
  NELSON_WINDOWS_THIRDPARTY_ROOT
  "${CMAKE_SOURCE_DIR}/../nelson-thirdparty-${NELSON_WINDOWS_THIRDPARTY_PLATFORM}"
  CACHE
    PATH
    "Nelson Windows third-party dependency root"
)

if(NOT IS_DIRECTORY "${NELSON_WINDOWS_THIRDPARTY_ROOT}")
  message(FATAL_ERROR
    "Nelson Windows third-party root not found: ${NELSON_WINDOWS_THIRDPARTY_ROOT}"
  )
endif()
# ==============================================================================
message(STATUS "Nelson Windows CMake Configuration:")
message(STATUS "  Platform: ${NELSON_WINDOWS_PLATFORM_NAME}")
message(STATUS "  Third-party root: ${NELSON_WINDOWS_THIRDPARTY_ROOT}")
# ==============================================================================
if(NELSON_PORTABLE_BUILD)
  set(BIN_DIRECTORY
    "${PROJECT_BINARY_DIR}/bin/${NELSON_WINDOWS_PLATFORM_NAME}"
  )
else()
  set(BIN_DIRECTORY
    "${CMAKE_SOURCE_DIR}/bin/${NELSON_WINDOWS_PLATFORM_NAME}"
  )
endif()
file(TO_CMAKE_PATH "${BIN_DIRECTORY}" BIN_DIRECTORY)

if(NELSON_WINDOWS_PLATFORM_NAME STREQUAL "x64")
  set(_nelson_mpi_root "${NELSON_WINDOWS_THIRDPARTY_ROOT}/mpi")
  set(_nelson_mpi_include_dir "${_nelson_mpi_root}/Include")

  if(NELSON_WINDOWS_PLATFORM_NAME STREQUAL "x64")
    set(_nelson_mpi_library_dir "${_nelson_mpi_root}/Lib/x64")
    set(ENV{MSMPI_LIB64} "${_nelson_mpi_library_dir}")
  endif()

  set(ENV{MSMPI_INC} "${_nelson_mpi_include_dir}")

  list(PREPEND CMAKE_INCLUDE_PATH "${_nelson_mpi_include_dir}")
  list(PREPEND CMAKE_LIBRARY_PATH "${_nelson_mpi_library_dir}")
  list(PREPEND CMAKE_PREFIX_PATH "${_nelson_mpi_root}")
endif()

foreach(_config Debug Release RelWithDebInfo MinSizeRel)
  string(TOUPPER "${_config}" _config_upper)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_${_config_upper} "${BIN_DIRECTORY}")
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_${_config_upper} "${BIN_DIRECTORY}")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${_config_upper} "${BIN_DIRECTORY}")
endforeach()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${BIN_DIRECTORY}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${BIN_DIRECTORY}")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${BIN_DIRECTORY}")
# ==============================================================================
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")
add_compile_definitions(WIN32 _WINDOWS UNICODE _UNICODE)
add_compile_options(/W3 /MP /permissive-)

if(ENABLE_AVX2 AND NELSON_WINDOWS_PLATFORM_NAME STREQUAL "x64")
  add_compile_options(/arch:AVX2)
endif()
# ==============================================================================
list(PREPEND CMAKE_PREFIX_PATH
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/curl"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/hdf5"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/netcdf"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/icu"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libffi"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libgif"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libpng"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libwebp"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libgit2"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libiconv"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/liblzma"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libsndfile"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libxml2"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libxslt"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/matio"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/mkl"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/openblas"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/qhull"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/arrow"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/portaudio"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/sundials"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/taglib"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/tiff"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/zlib"
)

list(PREPEND CMAKE_INCLUDE_PATH
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/Eigen"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/icu/include"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libffi/include"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/sundials/include"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/arrow/include"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/netcdf/include"
)

if(NOT _nelson_windows_is_arm64)
  list(PREPEND CMAKE_INCLUDE_PATH
    "${NELSON_WINDOWS_THIRDPARTY_ROOT}/mkl/include"
  )
endif()

list(PREPEND CMAKE_LIBRARY_PATH
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/icu/lib"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libffi/lib"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/openblas/lib"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/qhull/lib"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/sundials/lib"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/arrow/lib"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/netcdf/lib"
)

if(NOT _nelson_windows_is_arm64)
  list(PREPEND CMAKE_LIBRARY_PATH
    "${NELSON_WINDOWS_THIRDPARTY_ROOT}/mkl/blas-lapack-wrapper/lib"
    "${NELSON_WINDOWS_THIRDPARTY_ROOT}/mkl/vml/lib"
  )
endif()

include_directories(BEFORE
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/Eigen"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/zlib/include"
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/arrow/include"
)

set(ARROW_INCLUDE_DIRS "${NELSON_WINDOWS_THIRDPARTY_ROOT}/arrow/include")
set(ARROW_LIBRARIES "${NELSON_WINDOWS_THIRDPARTY_ROOT}/arrow/lib/arrow.lib")
set(PARQUET_LIBRARIES "${NELSON_WINDOWS_THIRDPARTY_ROOT}/arrow/lib/parquet.lib")
set(ARROW_RUNTIME_DIR "${NELSON_WINDOWS_THIRDPARTY_ROOT}/arrow/bin")
set(NETCDF_INCLUDE_DIRS "${NELSON_WINDOWS_THIRDPARTY_ROOT}/netcdf/include")
set(NETCDF_LIBRARIES "${NELSON_WINDOWS_THIRDPARTY_ROOT}/netcdf/lib/netcdf.lib")
set(NETCDF_RUNTIME_DIR "${NELSON_WINDOWS_THIRDPARTY_ROOT}/netcdf/bin")

if(NOT _nelson_windows_is_arm64)
  include_directories(BEFORE "${NELSON_WINDOWS_THIRDPARTY_ROOT}/mkl/include")
endif()
# ==============================================================================
set(
  EIGEN3_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/Eigen"
  CACHE
    PATH
    "Eigen include directory"
)
if(NOT _nelson_windows_is_arm64)
  set(
    MKL_DIR
    "${NELSON_WINDOWS_THIRDPARTY_ROOT}/mkl"
    CACHE
      PATH
      "Intel MKL root"
  )
endif()
set(
  LIBFFI_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libffi/include"
  CACHE
    PATH
    "libffi include directory"
)
set(
  LIBFFI_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libffi/lib/ffi.lib"
  CACHE
    FILEPATH
    "libffi library"
)
set(ICU_ROOT "${NELSON_WINDOWS_THIRDPARTY_ROOT}/icu" CACHE PATH "ICU root")
set(
  ICU_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/icu/include"
  CACHE
    PATH
    "ICU include directory"
)
set(
  ICU_UC_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/icu/lib/icuuc.lib"
  CACHE
    FILEPATH
    "ICU uc library"
)
set(
  ICU_I18N_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/icu/lib/icuin.lib"
  CACHE
    FILEPATH
    "ICU i18n library"
)
set(
  GIF_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libgif/include"
  CACHE
    PATH
    "libgif include directory"
  FORCE
)
set(
  GIF_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libgif/lib/gif.lib"
  CACHE
    FILEPATH
    "libgif library"
  FORCE
)
set(GIF_LIBRARIES "${GIF_LIBRARY}" CACHE FILEPATH "libgif libraries" FORCE)
set(
  JPEG_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/tiff/include"
  CACHE
    PATH
    "libjpeg-turbo include directory supplied by the TIFF bundle"
  FORCE
)
set(
  JPEG_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/tiff/lib/jpeg.lib"
  CACHE
    FILEPATH
    "libjpeg-turbo library supplied by the TIFF bundle"
  FORCE
)
set(
  PNG_PNG_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libpng/include"
  CACHE
    PATH
    "libpng include directory"
  FORCE
)
set(
  PNG_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libpng/lib/libpng16.lib"
  CACHE
    FILEPATH
    "libpng library"
  FORCE
)
set(
  WEBP_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libwebp/include"
  CACHE
    PATH
    "libwebp include directory"
  FORCE
)
set(
  WEBP_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libwebp/lib/libwebp.lib"
  CACHE
    FILEPATH
    "libwebp library"
  FORCE
)
set(
  TIFF_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/tiff/include"
  CACHE
    PATH
    "TIFF include directory"
  FORCE
)
set(
  TIFF_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/tiff/lib/tiff.lib"
  CACHE
    FILEPATH
    "TIFF library"
  FORCE
)
set(TIFF_LIBRARIES "${TIFF_LIBRARY}" CACHE FILEPATH "TIFF libraries" FORCE)
set(
  ZLIB_ROOT
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/zlib"
  CACHE
    PATH
    "zlib root"
  FORCE
)
set(
  ZLIB_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/zlib/include"
  CACHE
    PATH
    "zlib include directory"
  FORCE
)
set(
  ZLIB_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/zlib/lib/zlib.lib"
  CACHE
    FILEPATH
    "zlib library"
  FORCE
)
set(
  LIBXML2_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libxml2/include"
  CACHE
    PATH
    "libxml2 include directory"
  FORCE
)
set(
  LIBXML2_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libxml2/lib/libxml2.lib"
  CACHE
    FILEPATH
    "libxml2 library"
  FORCE
)
set(
  LIBXSLT_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libxslt/include"
  CACHE
    PATH
    "libxslt include directory"
  FORCE
)
set(
  LIBXSLT_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libxslt/lib/libxslt.lib"
  CACHE
    FILEPATH
    "libxslt library"
  FORCE
)
set(
  LIBXSLT_LIBRARIES
  "${LIBXSLT_LIBRARY}"
  CACHE
    FILEPATH
    "libxslt libraries"
  FORCE
)
set(
  LIBEXSLT_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libxslt/lib/libexslt.lib"
  CACHE
    FILEPATH
    "libexslt library"
  FORCE
)
set(
  HDF5_ROOT
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/hdf5"
  CACHE
    PATH
    "HDF5 root"
  FORCE
)
set(
  CURL_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/curl/include"
  CACHE
    PATH
    "curl include directory"
  FORCE
)
set(
  CURL_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/curl/lib/libcurl.lib"
  CACHE
    FILEPATH
    "curl library"
  FORCE
)
set(
  LIBGIT2_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libgit2/include"
  CACHE
    PATH
    "libgit2 include directory"
  FORCE
)
set(
  LIBGIT2_LIBRARIES
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libgit2/lib/git2.lib"
  CACHE
    FILEPATH
    "libgit2 library"
  FORCE
)
set(
  MATIO_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/matio/include"
  CACHE
    PATH
    "matio include directory"
  FORCE
)
set(
  MATIO_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/matio/lib/libmatio.lib"
  CACHE
    FILEPATH
    "matio library"
  FORCE
)
set(
  MATIO_LIBRARIES
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/matio/lib/libmatio.lib"
  CACHE
    FILEPATH
    "matio library"
  FORCE
)
set(
  SNDFILE_INCLUDE_DIRS
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libsndfile/include"
  CACHE
    PATH
    "libsndfile include directory"
  FORCE
)
set(
  SNDFILE_LIBRARIES
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/libsndfile/lib/sndfile.lib"
  CACHE
    FILEPATH
    "libsndfile library"
  FORCE
)
set(
  TAGLIB_INCLUDES
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/taglib/include/taglib"
  CACHE
    PATH
    "taglib include directory"
  FORCE
)
set(
  TAGLIB_LIBRARIES_RELEASE
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/taglib/lib/tag.lib"
  CACHE
    FILEPATH
    "taglib release library"
  FORCE
)
set(
  TAGLIB_LIBRARIES_DEBUG
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/taglib/lib/tag.lib"
  CACHE
    FILEPATH
    "taglib debug library"
  FORCE
)
set(
  TAGLIB_LIBRARIES
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/taglib/lib/tag.lib"
  CACHE
    FILEPATH
    "taglib library"
  FORCE
)
set(
  Portaudio_INCLUDE_DIR
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/portaudio/include"
  CACHE
    PATH
    "PortAudio include directory"
  FORCE
)
set(
  Portaudio_LIBRARY
  "${NELSON_WINDOWS_THIRDPARTY_ROOT}/portaudio/lib/portaudio.lib"
  CACHE
    FILEPATH
    "PortAudio library"
  FORCE
)
set(
  PORTAUDIO_INCLUDE_DIRS
  "${Portaudio_INCLUDE_DIR}"
  CACHE
    PATH
    "PortAudio include directories"
  FORCE
)
set(
  PORTAUDIO_LIBRARIES
  "${Portaudio_LIBRARY}"
  CACHE
    FILEPATH
    "PortAudio libraries"
  FORCE
)
if(NOT _nelson_windows_is_arm64)
  set(ENV{MKL_DIR} "${NELSON_WINDOWS_THIRDPARTY_ROOT}/mkl")
endif()

message(STATUS "Nelson Windows CMake platform: ${NELSON_WINDOWS_PLATFORM_NAME}")
message(STATUS "Nelson Windows bin directory: ${BIN_DIRECTORY}")
message(STATUS
  "Nelson Windows third-party root: ${NELSON_WINDOWS_THIRDPARTY_ROOT}"
)
