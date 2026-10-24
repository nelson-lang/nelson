# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
cmake_minimum_required(VERSION 3.16)
# ==============================================================================
# Set CMake policies early for compatibility with newer CMake versions
if(POLICY CMP0144)
  cmake_policy(SET CMP0144 NEW)
endif()
if(POLICY CMP0167)
  cmake_policy(SET CMP0167 OLD)
endif()
# ==============================================================================
set(Nelson_VERSION_MAJOR_DEFAULT 2)
set(Nelson_VERSION_MINOR_DEFAULT 0)
set(Nelson_VERSION_MAINTENANCE_DEFAULT 0)
set(Nelson_VERSION_BUILD_DEFAULT 0)
# ==============================================================================
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake/")
include(VersionConfig)
# ==============================================================================
project(Nelson
  VERSION ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
  DESCRIPTION "Nelson programming language"
  LANGUAGES
    C
    CXX
)
# ==============================================================================
if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  include(WindowsConfig)
endif()
# ==============================================================================
set(DOMAIN_NAME "io.github.nelson_lang")
set(NELSON_APP_ID "${DOMAIN_NAME}.${PROJECT_NAME}")
set(Nelson_VERSION
  ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
)
# ==============================================================================
message(STATUS "CMake version: ${CMAKE_VERSION}")
message(STATUS "Nelson version: ${Nelson_VERSION}")
message(STATUS "CMAKE_INSTALL_PREFIX: ${CMAKE_INSTALL_PREFIX}")
# ==============================================================================
# Build type
# ==============================================================================
option(DEBUG_BUILD
  "Enable Debug build"
  OFF
)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  if(DEBUG_BUILD)
    set(
      CMAKE_BUILD_TYPE
      "Debug"
      CACHE
        STRING
        "Build type"
      FORCE
    )
  else()
    set(
      CMAKE_BUILD_TYPE
      "Release"
      CACHE
        STRING
        "Build type"
      FORCE
    )
  endif()
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
# ==============================================================================
option(ENABLE_SANITIZERS
  "Build with AddressSanitizer + UndefinedBehaviorSanitizer (LeakSanitizer where the platform supports it)"
  OFF
)
if(ENABLE_SANITIZERS)
  set(NELSON_SANITIZER_FLAGS
    "-fsanitize=address,undefined -fno-omit-frame-pointer"
  )
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${NELSON_SANITIZER_FLAGS}")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${NELSON_SANITIZER_FLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS
    "${CMAKE_EXE_LINKER_FLAGS} ${NELSON_SANITIZER_FLAGS}"
  )
  set(CMAKE_SHARED_LINKER_FLAGS
    "${CMAKE_SHARED_LINKER_FLAGS} ${NELSON_SANITIZER_FLAGS}"
  )
  set(CMAKE_MODULE_LINKER_FLAGS
    "${CMAKE_MODULE_LINKER_FLAGS} ${NELSON_SANITIZER_FLAGS}"
  )
  message(STATUS "Sanitizers ENABLED: ${NELSON_SANITIZER_FLAGS}")
endif()
# ==============================================================================
# Global options
# ==============================================================================
option(BUILD_SHARED_LIBS
  "Build shared libraries"
  ON
)
option(ENABLE_AVX2
  "Enable AVX2 instruction set (if available)"
  OFF
)
option(LGPL_ONLY
  "Build Nelson under LGPL v3.x only"
  OFF
)
option(BUILD_HELP
  "Build help files"
  OFF
)
option(MINIMAL_BUILD
  "Build minimal Nelson"
  OFF
)
include(PortableOptions)
option(WITHOUT_OPENMP
  "Disable OpenMP"
  OFF
)
option(WITHOUT_TBB
  "Disable TBB"
  OFF
)
option(WITHOUT_LIBGIT2
  "Disable libgit2"
  OFF
)
option(WITHOUT_LIBCURL
  "Disable libcurl"
  OFF
)
option(WITHOUT_FILEWATCHER
  "Disable File watcher"
  OFF
)
option(FORCE_LIBGFORTRAN_LINK
  "Force libgfortran link"
  OFF
)
option(WITHOUT_GIF
  "Disable GIF library"
  OFF
)
option(WITHOUT_TIFF
  "Disable TIFF library"
  OFF
)
option(ENABLE_CLANG_TIDY
  "Analyze code with clang-tidy"
  OFF
)
option(ENABLE_CLANG_TIDY_FIX
  "Analyze code with clang-tidy and fix errors"
  OFF
)
option(ENABLE_PACKAGING
  "Build Nelson for packaging"
  OFF
)
option(WITHOUT_ARPACK
  "Disable ARPACK support"
  OFF
)
# ==============================================================================
# Optional modules
# ==============================================================================
set(NELSON_OPTIONAL_MODULES
  MEX
  FFTW
  F2C
  MPI
  AUDIO
  WEBTOOLS
  WEBVIEW
  FILE_ARCHIVER
  IPC
  PARALLEL
  GRAPHICS
  RENDER_QT
  GRAPHICS_QT
  GRAPHICS_IO
  IMAGE_PROCESSING
  QML_ENGINE
  SIGNAL_PROCESSING
  VALIDATORS
  HDF5
  PREFERENCES
  NETCDF
  MATIO
  SLICOT
  CONTROL_SYSTEM
  HELP_TOOLS
  TEXT_EDITOR
  DATA_ANALYSIS
  DYNAMIC_LINK
  TESTS_MANAGER
  GUI
  JSON
  AI
  XML
  ASSERT_FUNCTIONS
  STATISTICS
  TRIGONOMETRIC_FUNCTIONS
  POLYNOMIAL_FUNCTIONS
  LOCALIZATION
  RANDOM
  OPTIMIZATION
  SPECIAL_FUNCTIONS
  TEXT_COMPLETION
  CHARACTERS_ENCODING
  GEOMETRY
  PYTHON_ENGINE
  JULIA_ENGINE
  PARQUET
  SPREADSHEET
  NFLOW_BLOCKS
  NFLOW_ENGINE
  NFLOW_FMI
  NFLOW_GUI
  CATEGORICAL
  ODE_SOLVERS
  COMPILER
  NMM_GUI
)

foreach(_mod IN LISTS NELSON_OPTIONAL_MODULES)
  option(WITHOUT_${_mod}_MODULE
    "Disable ${_mod} module"
    OFF
  )
endforeach()

# Nelson WebView desktop (not part of NELSON_OPTIONAL_MODULES). Built by default;
# disable with -DWITH_WEB_GUI_MODULE=OFF. It requires the WEBVIEW and
# TEXT_COMPLETION modules and is automatically disabled when either is off (e.g.
# in a MINIMAL_BUILD). See modules/web_gui/INTEGRATION.md.
option(WITH_WEB_GUI_MODULE
  "Build the WEB_GUI module and the nelson-webview executable"
  ON
)

# GPU compute module (WebGPU/Dawn), not part of NELSON_OPTIONAL_MODULES.  Its
# Dawn dependency ships with the Windows third-party set, so it is enabled by
# default on Windows and opt-in (OFF) elsewhere.
if(WIN32)
  set(_with_gpu_engine_default ON)
else()
  set(_with_gpu_engine_default OFF)
endif()
option(WITH_GPU_ENGINE_MODULE
  "Build the optional GPU (WebGPU/Dawn) compute module"
  ${_with_gpu_engine_default}
)

option(NELSON_ALLOW_MPI_ON_WINDOWS_ARM64
  "Allow MPI module on Windows ARM64 (disabled by default)"
  OFF
)

if(WIN32
  AND
    (CMAKE_VS_PLATFORM_NAME STREQUAL "ARM64" OR CMAKE_SYSTEM_PROCESSOR MATCHES "^(ARM64|arm64|AARCH64|aarch64)$")
)
  if(NOT NELSON_ALLOW_MPI_ON_WINDOWS_ARM64)
    set(
      WITHOUT_MPI_MODULE
      ON
      CACHE
        BOOL
        "Disable MPI module"
      FORCE
    )
    message(STATUS "Windows ARM64 detected: MPI module disabled by default.")
    message(STATUS
      "Use -DNELSON_ALLOW_MPI_ON_WINDOWS_ARM64=ON to allow enabling MPI manually."
    )
  endif()
endif()
# ==============================================================================
# Minimal build: disable everything optional
# ==============================================================================
if(MINIMAL_BUILD)
  message(STATUS "Minimal build enabled - disabling all optional modules.")
  set(WITHOUT_FILEWATCHER ON)
  set(WITHOUT_OPENMP ON)
  set(WITHOUT_TBB ON)
  foreach(_mod IN LISTS NELSON_OPTIONAL_MODULES)
    set(WITHOUT_${_mod}_MODULE ON)
  endforeach()
  # gpu_engine is opt-in through WITH_GPU_ENGINE_MODULE (not a WITHOUT_* module),
  # so disable it explicitly for a minimal build.
  set(WITH_GPU_ENGINE_MODULE OFF)

endif()
# ==============================================================================
include(PortableProfile)
# ==============================================================================
# Module dependency constraints
# ==============================================================================
if(WITHOUT_LOCALIZATION_MODULE)
  set(WITHOUT_I18N_MODULE ON)
endif()
if(WITHOUT_GUI_MODULE)
  set(WITHOUT_QML_ENGINE_MODULE ON)
  set(WITHOUT_TEXT_EDITOR_MODULE ON)
  set(WITHOUT_RENDER_QT_MODULE ON)
  set(WITHOUT_GRAPHICS_QT_MODULE ON)
endif()
if(WITHOUT_GRAPHICS_MODULE)
  set(WITHOUT_GRAPHICS_IO_MODULE ON)
  set(WITHOUT_GRAPHICS_QT_MODULE ON)
endif()
if(WITHOUT_RENDER_QT_MODULE)
  set(WITHOUT_GRAPHICS_QT_MODULE ON)
endif()
if(WITHOUT_DATA_ANALYSIS_MODULE)
  set(WITHOUT_VALIDATORS_MODULE ON)
  # categorical overloads data-analysis functions (e.g. histcounts).
  set(WITHOUT_CATEGORICAL_MODULE ON)
  if(NOT (NELSON_PORTABLE_BUILD AND NELSON_PORTABLE_ENABLE_TESTS))
    set(WITHOUT_TESTS_MANAGER_MODULE ON)
  endif()
endif()
if(LGPL_ONLY)
  set(WITHOUT_FFTW_MODULE ON)
endif()
if(WITHOUT_SLICOT_MODULE)
  set(WITHOUT_CONTROL_SYSTEM_MODULE ON)
endif()
# WebView drives the NFlow GUI, the package-manager window (nmm_gui) and the
# web desktop; without it the whole NFlow feature set and the module-manager
# GUI are disabled. The portable and WebAssembly profiles run the NFlow
# simulation core without WebView (the browser shell is the GUI host), so
# their explicit NFlow selection is kept.
if(WITHOUT_WEBVIEW_MODULE)
  if(NOT NELSON_PORTABLE_WITH_NFLOW)
    set(WITHOUT_NFLOW_BLOCKS_MODULE ON)
  endif()
  set(WITHOUT_NMM_GUI_MODULE ON)
endif()
# nflow_engine links the block library; nflow_gui (the GUI host) links the
# engine, the blocks and the webui host.
if(WITHOUT_NFLOW_BLOCKS_MODULE)
  set(WITHOUT_NFLOW_ENGINE_MODULE ON)
endif()
# nflow_engine links ode_solvers for the variable-step (CVODES) and
# differential-algebraic (IDAS) solver backends, so the NFlow feature set
# requires ode_solvers. Mirrors the installer's EnsureDependency(ODE_SOLVERS,
# NFLOW): without ode_solvers there is no NFlow.
if(WITHOUT_ODE_SOLVERS_MODULE)
  set(WITHOUT_NFLOW_ENGINE_MODULE ON)
endif()
# nflow_fmi (FMI 3.0 import) is part of the NFlow feature set.
if(WITHOUT_NFLOW_ENGINE_MODULE)
  set(WITHOUT_NFLOW_FMI_MODULE ON)
endif()
if(WITHOUT_NFLOW_ENGINE_MODULE
  OR WITHOUT_NFLOW_BLOCKS_MODULE
  OR WITHOUT_WEBVIEW_MODULE
)
  set(WITHOUT_NFLOW_GUI_MODULE ON)
endif()
if(WITHOUT_LIBGIT2
  AND WITHOUT_LIBCURL
  AND NOT (NELSON_WASM_BUILD AND NELSON_PORTABLE_WITH_WEBTOOLS)
)
  # the WebAssembly build serves webread/websave through the browser instead
  set(WITHOUT_WEBTOOLS_MODULE ON)
endif()
if(WITHOUT_XML_MODULE)
  set(WITHOUT_HELP_TOOLS_MODULE ON)
endif()
if(WITHOUT_JSON_MODULE)
  set(WITHOUT_AI_MODULE ON)
endif()
# the application compiler needs the file archiver, native addon loader and JSON.
if(WITHOUT_FILE_ARCHIVER_MODULE
  OR WITHOUT_DYNAMIC_LINK_MODULE
  OR WITHOUT_JSON_MODULE
)
  set(WITHOUT_COMPILER_MODULE ON)
endif()
if(WITH_WEB_GUI_MODULE
  AND (WITHOUT_WEBVIEW_MODULE OR WITHOUT_TEXT_COMPLETION_MODULE)
)
  message(STATUS
    "WEB_GUI module disabled (requires the WEBVIEW and TEXT_COMPLETION modules)."
  )
  set(WITH_WEB_GUI_MODULE OFF)
endif()
# ==============================================================================
# Status messages for disabled features
# ==============================================================================
if(WITHOUT_OPENMP)
  message(STATUS "OpenMP disabled.")
endif()
if(WITHOUT_TBB)
  message(STATUS "TBB disabled.")
endif()
if(LGPL_ONLY OR WITHOUT_FFTW_MODULE)
  message(STATUS "LGPL v3.x: enabled.")
endif()
foreach(_mod IN LISTS NELSON_OPTIONAL_MODULES)
  if(WITHOUT_${_mod}_MODULE)
    message(STATUS "${_mod} module disabled.")
  endif()
endforeach()
# ==============================================================================
# Compiler / language configuration
# ==============================================================================
include(C17Config)
include(clang-tidy)
# ==============================================================================
# Module helper functions (nelson_install_library, nelson_install_module_data)
# ==============================================================================
include(NelsonModuleHelpers)
# ==============================================================================
# Source / binary tree configuration
# ==============================================================================
include(TreeConfig)
# ==============================================================================
# Install RPATH
# ==============================================================================
include(GNUInstallDirs)
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(CMAKE_MACOSX_RPATH ON)
  # Use @loader_path-based rpaths so the bundle is relocatable.
  # Libraries in lib/Nelson/ find siblings via @loader_path,
  # executables in bin/ find them via @loader_path/../lib/Nelson.
  set(CMAKE_INSTALL_RPATH
    "@loader_path"
    "@loader_path/../lib/Nelson"
    "@loader_path/../${CMAKE_INSTALL_LIBDIR}/Nelson"
  )
else()
  set(CMAKE_INSTALL_RPATH
    "${CMAKE_INSTALL_PREFIX}/lib"
    "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}"
  )
endif()
# ==============================================================================
# Platform-specific setup
# ==============================================================================
if(CMAKE_SYSTEM_NAME STREQUAL "Emscripten")
  set(BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin/wasm)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    add_compile_definitions(_LARGEFILE_SOURCE _FILE_OFFSET_BITS=64)
  endif()
  set(BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin/linux)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  include(MacOsConfig)
endif()
# ==============================================================================
# Third-party dependencies
# ==============================================================================
include(ConfigureEigen)
# ==============================================================================
# -- Qt (GUI modules)
if(NOT WITHOUT_GUI_MODULE)
  include(QtConfig)
endif()
# ==============================================================================
# -- MPI
if(NOT WITHOUT_MPI_MODULE)
  include(MpiConfig)
endif()
# ==============================================================================
# -- ICU (character encoding)
if(NOT WITHOUT_CHARACTERS_ENCODING_MODULE)
  include(IcuConfig)
endif()
# ==============================================================================
# -- libffi (dynamic linking)
if(NOT WITHOUT_DYNAMIC_LINK_MODULE)
  include(FfiConfig)
endif()
# ==============================================================================
# -- GIF / TIFF (graphics)
if(NOT WITHOUT_GRAPHICS_MODULE)
  include(GifConfig)
  include(TiffConfig)
endif()
# ==============================================================================
# -- pkg-config
if(NELSON_WASM_BUILD)
  find_package(PkgConfig QUIET)
elseif(WIN32)
  find_package(PkgConfig QUIET)
else()
  find_package(PkgConfig REQUIRED)
endif()
# ==============================================================================
# -- XML
if(NOT WITHOUT_XML_MODULE)
  if(APPLE)
    # Keep libxml2 and libxslt from the same Homebrew keg family on macOS.
    set(_nelson_brew_prefix_candidates)
    if(DEFINED ENV{HOMEBREW_PREFIX})
      list(APPEND _nelson_brew_prefix_candidates "$ENV{HOMEBREW_PREFIX}")
    endif()
    list(APPEND _nelson_brew_prefix_candidates "/opt/homebrew" "/usr/local")
    list(REMOVE_DUPLICATES _nelson_brew_prefix_candidates)
    foreach(_nelson_brew_prefix IN LISTS _nelson_brew_prefix_candidates)
      set(_nelson_libxml2_prefix "${_nelson_brew_prefix}/opt/libxml2")
      set(_nelson_libxslt_prefix "${_nelson_brew_prefix}/opt/libxslt")
      if(NOT EXISTS "${_nelson_libxml2_prefix}/lib/libxml2.dylib"
        OR NOT EXISTS
          "${_nelson_libxml2_prefix}/include/libxml2/libxml/parser.h"
        OR NOT EXISTS "${_nelson_libxslt_prefix}/lib/libxslt.dylib"
        OR NOT EXISTS "${_nelson_libxslt_prefix}/include/libxslt/xslt.h"
      )
        continue()
      endif()
      set(
        LIBXML2_INCLUDE_DIR
        "${_nelson_libxml2_prefix}/include/libxml2"
        CACHE
          PATH
          "libxml2 include directory"
        FORCE
      )
      set(
        LIBXML2_LIBRARY
        "${_nelson_libxml2_prefix}/lib/libxml2.dylib"
        CACHE
          FILEPATH
          "libxml2 library"
        FORCE
      )
      set(
        LIBXML2_XMLLINT_EXECUTABLE
        "${_nelson_libxml2_prefix}/bin/xmllint"
        CACHE
          FILEPATH
          "xmllint executable"
        FORCE
      )
      set(
        LIBXSLT_INCLUDE_DIR
        "${_nelson_libxslt_prefix}/include"
        CACHE
          PATH
          "libxslt include directory"
        FORCE
      )
      set(
        LIBXSLT_EXSLT_INCLUDE_DIR
        "${_nelson_libxslt_prefix}/include"
        CACHE
          PATH
          "libexslt include directory"
        FORCE
      )
      set(
        LIBXSLT_LIBRARY
        "${_nelson_libxslt_prefix}/lib/libxslt.dylib"
        CACHE
          FILEPATH
          "libxslt library"
        FORCE
      )
      set(
        LIBXSLT_EXSLT_LIBRARY
        "${_nelson_libxslt_prefix}/lib/libexslt.dylib"
        CACHE
          FILEPATH
          "libexslt library"
        FORCE
      )
      set(
        LIBXSLT_XSLTPROC_EXECUTABLE
        "${_nelson_libxslt_prefix}/bin/xsltproc"
        CACHE
          FILEPATH
          "xsltproc executable"
        FORCE
      )
      break()
    endforeach()
    unset(_nelson_brew_prefix_candidates)
    unset(_nelson_brew_prefix)
    unset(_nelson_libxml2_prefix)
    unset(_nelson_libxslt_prefix)
  endif()
  find_package(LibXml2 REQUIRED)
  find_package(LibXslt REQUIRED)
  # ---------------------------------------------------------------------------
  # Guard: Homebrew's pkg-config for libxslt/libxml2 can hardcode the SDK
  # version that was active at brew-install time (e.g. MacOSX15.sdk).
  # If the system has since been upgraded (e.g. MacOSX26.sdk), adding that
  # stale path as -I or -isystem breaks <complex>, <cstdio>, etc. because
  # the compiler mixes headers from two different SDKs.
  # Since SDK include paths are already in the compiler's default search
  # path (via -isysroot / xcrun), we strip them from the imported targets.
  # ---------------------------------------------------------------------------
  if(APPLE)
    foreach(_xslt_tgt LibXslt::LibXslt LibXslt::LibExslt LibXml2::LibXml2)
      if(TARGET ${_xslt_tgt})
        get_target_property(_inc ${_xslt_tgt} INTERFACE_INCLUDE_DIRECTORIES)
        if(_inc)
          list(FILTER _inc
            EXCLUDE
            REGEX "/Library/Developer/.*/SDKs/.*/usr/include$"
          )
          set_target_properties(${_xslt_tgt}
            PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${_inc}"
          )
        endif()
      endif()
    endforeach()
  endif()
endif()
# ==============================================================================
include(ConfigureBlasLapack)
# ==============================================================================
# -- ARPACK
include(ArpackConfig)
# ==============================================================================
# -- HDF5
if(NOT WITHOUT_HDF5_MODULE)
  if(NELSON_WASM_BUILD
    AND NELSON_PORTABLE_WITH_HDF5
    AND HDF5_INCLUDE_DIRS
    AND HDF5_LIBRARIES
  )
    message(STATUS "Using configured WebAssembly HDF5 static libraries.")
  else()
    find_package(HDF5 REQUIRED)
  endif()
endif()
# ==============================================================================
# -- netCDF
if(NOT WITHOUT_NETCDF_MODULE)
  if(WIN32)
    if(NOT NETCDF_INCLUDE_DIRS OR NOT NETCDF_LIBRARIES)
      message(SEND_ERROR "netCDF library required for netCDF module.")
    endif()
  elseif(NELSON_WASM_BUILD
    AND NELSON_PORTABLE_WITH_NETCDF
    AND NETCDF_INCLUDE_DIRS
    AND NETCDF_LIBRARIES
  )
    message(STATUS "Using configured WebAssembly netCDF static libraries.")
  else()
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(NETCDF netcdf)
    if(NOT NETCDF_FOUND)
      find_path(NETCDF_INCLUDE_DIRS netcdf.h)
      find_library(NETCDF_LIBRARIES NAMES netcdf)
      if(NETCDF_INCLUDE_DIRS AND NETCDF_LIBRARIES)
        set(NETCDF_FOUND TRUE)
      endif()
    elseif(NETCDF_LIBRARY_DIRS)
      find_library(_NELSON_NETCDF_LIBRARY
        NAMES
        netcdf
        PATHS
        ${NETCDF_LIBRARY_DIRS}
        NO_DEFAULT_PATH
      )
      if(_NELSON_NETCDF_LIBRARY)
        set(NETCDF_LIBRARIES "${_NELSON_NETCDF_LIBRARY}")
      endif()
    endif()
    if(NOT NETCDF_FOUND)
      set(WITHOUT_NETCDF_MODULE ON CACHE BOOL "Disable NETCDF module" FORCE)
      message(STATUS "netCDF library not found: NETCDF module disabled.")
    endif()
  endif()
endif()
# ==============================================================================
# -- Arrow / Parquet
if(NOT WITHOUT_PARQUET_MODULE)
  if(NELSON_WASM_BUILD AND NELSON_PORTABLE_WITH_PARQUET)
    if(NOT ARROW_INCLUDE_DIRS OR NOT ARROW_LIBRARIES OR NOT PARQUET_LIBRARIES)
      message(FATAL_ERROR
        "The portable WebAssembly Parquet profile did not configure Arrow "
        "and Parquet archives."
      )
    endif()
  elseif(WIN32)
    if(NOT ARROW_INCLUDE_DIRS OR NOT ARROW_LIBRARIES OR NOT PARQUET_LIBRARIES)
      message(SEND_ERROR
        "Arrow and Parquet libraries required for Parquet module."
      )
    endif()
  else()
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(ARROW arrow)
    pkg_check_modules(PARQUET parquet)
    if(NOT ARROW_FOUND OR NOT PARQUET_FOUND)
      set(WITHOUT_PARQUET_MODULE ON CACHE BOOL "Disable PARQUET module" FORCE)
      message(STATUS
        "Arrow/Parquet libraries not found: PARQUET module disabled."
      )
    endif()
  endif()
endif()
# ==============================================================================
# -- OpenMP
if(NOT WITHOUT_OPENMP)
  find_package(OpenMP)
endif()
# ==============================================================================
# -- zlib (file archiver)
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  find_package(ZLIB REQUIRED)
endif()
# ==============================================================================
# -- curl / libgit2 (webtools)
if(NOT WITHOUT_WEBTOOLS_MODULE)
  if(NOT WITHOUT_LIBCURL)
    find_package(CURL REQUIRED)
  endif()
  if(NOT WITHOUT_LIBGIT2)
    find_package(libgit2 REQUIRED)
  endif()
endif()
# ==============================================================================
# -- Audio libraries
if(NELSON_PORTABLE_BUILD AND NELSON_PORTABLE_WITH_AUDIO)
  set(
    NELSON_AUDIO_FILE_ONLY_BACKEND
    ON
    CACHE
      BOOL
      "Build the audio module without native audio-device streams"
    FORCE
  )
endif()
if(NOT WITHOUT_AUDIO_MODULE)
  find_package(LibSndFile REQUIRED)
  if(NOT NELSON_AUDIO_FILE_ONLY_BACKEND)
    find_package(Taglib)
    if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
      find_package(ALSA REQUIRED)
      find_package(Jack REQUIRED)
    endif()
    find_package(Portaudio REQUIRED)
    include(PortAudioConfig)
  else()
    set(
      TAGLIB_FOUND
      FALSE
      CACHE
        BOOL
        "TagLib disabled for portable file-only audio"
      FORCE
    )
    set(
      PORTAUDIO_FOUND
      FALSE
      CACHE
        BOOL
        "PortAudio disabled for portable file-only audio"
      FORCE
    )
  endif()
endif()
# ==============================================================================
# -- MATIO
if(NOT WITHOUT_MATIO_MODULE)
  if(NELSON_WASM_BUILD
    AND NELSON_PORTABLE_WITH_MATIO
    AND MATIO_INCLUDE_DIRS
    AND MATIO_LIBRARIES
  )
    message(STATUS "Using configured WebAssembly MATIO static libraries.")
  else()
    find_package(MATIO REQUIRED)
  endif()
  if(MATIO_FOUND)
    message(STATUS "Found Matio lib: ${MATIO_LIBRARIES}")
  endif()
endif()
# ==============================================================================
# -- Dawn (WebGPU) for the optional gpu_engine module
if(WITH_GPU_ENGINE_MODULE)
  find_package(Dawn)
  if(DAWN_FOUND)
    message(STATUS "Found Dawn lib: ${DAWN_LIBRARIES}")
  else()
    set(WITH_GPU_ENGINE_MODULE OFF)
    message(STATUS "Dawn (WebGPU) not found: gpu_engine module disabled.")
  endif()
endif()
# ==============================================================================
# -- TBB
if(NOT WITHOUT_TBB)
  find_package(TBB)
endif()
# ==============================================================================
# -- Python
if(NOT WITHOUT_PYTHON_ENGINE_MODULE)
  find_package(Python3 COMPONENTS Interpreter Development)
  if(NOT Python3_Development_FOUND)
    set(WITHOUT_PYTHON_ENGINE_MODULE ON)
    message(STATUS "Python Development not found.")
  else()
    message(STATUS "Found Python include dirs: ${Python3_INCLUDE_DIRS}")
  endif()
endif()
# ==============================================================================
# -- Julia
if(NOT WITHOUT_JULIA_ENGINE_MODULE)
  find_package(Julia 1.7.0)
  if(NOT Julia_FOUND)
    set(WITHOUT_JULIA_ENGINE_MODULE ON)
    message(STATUS "Julia not found.")
  else()
    message(STATUS "Found Julia include dirs: ${JULIA_INCLUDE_DIR}")
    message(STATUS "Found Julia library dir: ${JULIA_LIBRARY}")
  endif()
endif()
# ==============================================================================
# Build configuration generation (nlsBuildConfig.h, modules.m)
# ==============================================================================
include(OptionsConfig)
# ==============================================================================
# Output directories (development build)
# ==============================================================================
set(INSTALL_DIRECTORY "${CMAKE_INSTALL_PREFIX}Nelson-${Nelson_VERSION}")
if(CMAKE_SYSTEM_NAME STREQUAL "Emscripten")
  set(LIBRARY_OUTPUT "Nelson-${Nelson_VERSION}/bin/wasm/")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(LIBRARY_OUTPUT "Nelson-${Nelson_VERSION}/bin/macOS/")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  set(LIBRARY_OUTPUT
    "Nelson-${Nelson_VERSION}/bin/${NELSON_WINDOWS_PLATFORM_NAME}/"
  )
else()
  set(LIBRARY_OUTPUT "Nelson-${Nelson_VERSION}/bin/linux/")
endif()
set(ROOT_OUTPUT "Nelson-${Nelson_VERSION}/")

if(NOT WIN32)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${BIN_DIRECTORY})
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${BIN_DIRECTORY})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${BIN_DIRECTORY})
endif()
# ==============================================================================
# Packaging
# ==============================================================================
include(PackageConfig)
# ==============================================================================
# Subdirectories
# ==============================================================================
add_subdirectory(modules)
add_subdirectory(etc)
add_subdirectory(locale)
add_subdirectory(resources)
add_subdirectory(desktop)
# ==============================================================================
# Install tools/ scripts used by CI and packaged installers
# ==============================================================================
set(_tools_dest "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/tools")

install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/tools/tests_all/
  DESTINATION ${_tools_dest}/tests_all
)

install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/tools/benchmark_all/
  DESTINATION ${_tools_dest}/benchmark_all
)

install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/tools/tests_result/
  DESTINATION ${_tools_dest}/tests_result
)

install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/tools/module_CI/
  DESTINATION ${_tools_dest}/module_CI
)

install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/tools/python_environment_CI/
  DESTINATION ${_tools_dest}/python_environment_CI
)

install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/tools/julia_environment_CI/
  DESTINATION ${_tools_dest}/julia_environment_CI
)

# ==============================================================================
# Install documentation files
# ==============================================================================
set(_doc_dest "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(
  FILES
    ${CMAKE_SOURCE_DIR}/LICENSE
    ${CMAKE_SOURCE_DIR}/gpl-3.0.md
    ${CMAKE_SOURCE_DIR}/lgpl-3.0.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG-0.1.x.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG-0.2.x.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG-0.3.x.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG-0.4.x.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG-0.5.x.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG-0.6.x.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG-0.7.x.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG-1.x.x.md
    ${CMAKE_SOURCE_DIR}/CONTRIBUTORS.md
    ${CMAKE_SOURCE_DIR}/README.md
    ${CMAKE_SOURCE_DIR}/THIRDPARTY.md
    ${CMAKE_SOURCE_DIR}/TRADEMARKS.md
    ${CMAKE_SOURCE_DIR}/CLA.md
    ${CMAKE_SOURCE_DIR}/nelson-lint.json
    ${CMAKE_SOURCE_DIR}/nelson-format.json
    ${CMAKE_SOURCE_DIR}/nelson-format-ignore.json
  DESTINATION "${_doc_dest}"
)
# ==============================================================================
# Writable scratch directory at nelsonroot()/temp
# ==============================================================================
# Runtime code and several compiler/deploytool tests call
# tempname(fullfile(nelsonroot(), 'temp')), which requires the folder to exist
# (tempname validates its directory argument).  Source-tree runs use the repo's
# own temp/; installed builds shipped none, so those tests failed with
# "Value must be a folder".  Create an empty temp/ in the installed layout.
install(CODE "file(MAKE_DIRECTORY \"\${CMAKE_INSTALL_PREFIX}/${_doc_dest}/temp\")"
)
# ==============================================================================
# CMake package config
# ==============================================================================
include(CMakePackageConfigHelpers)

set(
  NELSON_CMAKECONFIG_INSTALL_DIR
  "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  CACHE
    STRING
    "Install path for NelsonConfig.cmake"
)

set(
  NELSON_DATA_INSTALL_DIR
  "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules"
  CACHE
    STRING
    "Install path for Nelson data files"
)

configure_package_config_file(
  ${PROJECT_NAME}Config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${PROJECT_NAME}Config.cmake"
  INSTALL_DESTINATION
  ${NELSON_CMAKECONFIG_INSTALL_DIR}
)

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
  VERSION
  ${Nelson_VERSION}
  COMPATIBILITY
  AnyNewerVersion
)
# ==============================================================================
install(
  FILES
    ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${PROJECT_NAME}Config.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
  DESTINATION ${NELSON_CMAKECONFIG_INSTALL_DIR}
)
# ==============================================================================
install(EXPORT ${PROJECT_NAME}-targets
  FILE ${PROJECT_NAME}Targets.cmake
  DESTINATION ${NELSON_CMAKECONFIG_INSTALL_DIR}
)
