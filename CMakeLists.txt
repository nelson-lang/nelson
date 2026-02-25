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
if(CMAKE_SYSTEM_NAME
   STREQUAL
   "Windows")
  message(SEND_ERROR "On Windows, please use the Visual Studio solution.")
endif()
# ==============================================================================
set(Nelson_VERSION_MAJOR_DEFAULT 1)
set(Nelson_VERSION_MINOR_DEFAULT 16)
set(Nelson_VERSION_MAINTENANCE_DEFAULT 0)
set(Nelson_VERSION_BUILD_DEFAULT 0)
# ==============================================================================
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake/")
include(VersionConfig)
# ==============================================================================
project(
  Nelson
  VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
  DESCRIPTION "Nelson programming language"
  LANGUAGES C CXX)
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
option(
  DEBUG_BUILD
  "Enable Debug build"
  OFF)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  if(DEBUG_BUILD)
    set(CMAKE_BUILD_TYPE
        "Debug"
        CACHE STRING
              "Build type"
              FORCE)
  else()
    set(CMAKE_BUILD_TYPE
        "Release"
        CACHE STRING
              "Build type"
              FORCE)
  endif()
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
# ==============================================================================
# Global options
# ==============================================================================
option(
  BUILD_SHARED_LIBS
  "Build shared libraries"
  ON)
option(
  ENABLE_AVX2
  "Enable AVX2 instruction set (if available)"
  OFF)
option(
  LGPL_ONLY
  "Build Nelson under LGPL v3.x only"
  OFF)
option(
  BUILD_HELP
  "Build help files"
  OFF)
option(
  MINIMAL_BUILD
  "Build minimal Nelson"
  OFF)
option(
  WITHOUT_OPENMP
  "Disable OpenMP"
  OFF)
option(
  WITHOUT_TBB
  "Disable TBB"
  OFF)
option(
  WITHOUT_LIBGIT2
  "Disable libgit2"
  OFF)
option(
  WITHOUT_LIBCURL
  "Disable libcurl"
  OFF)
option(
  WITHOUT_FILEWATCHER
  "Disable File watcher"
  OFF)
option(
  FORCE_LIBGFORTRAN_LINK
  "Force libgfortran link"
  OFF)
option(
  WITHOUT_GIF
  "Disable GIF library"
  OFF)
option(
  WITHOUT_TIFF
  "Disable TIFF library"
  OFF)
option(
  FORCE_QT5
  "Force to use Qt5"
  OFF)
option(
  ENABLE_CLANG_TIDY
  "Analyze code with clang-tidy"
  OFF)
option(
  ENABLE_CLANG_TIDY_FIX
  "Analyze code with clang-tidy and fix errors"
  OFF)
option(
  ENABLE_PACKAGING
  "Build Nelson for packaging"
  OFF)
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
    FILE_ARCHIVER
    IPC
    SIO_CLIENT
    PARALLEL
    GRAPHICS
    GRAPHICS_IO
    IMAGE_PROCESSING
    QML_ENGINE
    SIGNAL_PROCESSING
    VALIDATORS
    HDF5
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
    XML
    NIG
    ASSERT_FUNCTIONS
    STATISTICS
    TRIGONOMETRIC_FUNCTIONS
    POLYNOMIAL_FUNCTIONS
    LOCALIZATION
    RANDOM
    SPECIAL_FUNCTIONS
    TEXT_COMPLETION
    CHARACTERS_ENCODING
    GEOMETRY
    PYTHON_ENGINE
    JULIA_ENGINE
    SPREADSHEET)

foreach(_mod IN LISTS NELSON_OPTIONAL_MODULES)
  option(
    WITHOUT_${_mod}_MODULE
    "Disable ${_mod} module"
    OFF)
endforeach()
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
endif()
# ==============================================================================
# Module dependency constraints
# ==============================================================================
if(WITHOUT_LOCALIZATION_MODULE)
  set(WITHOUT_I18N_MODULE ON)
endif()
if(WITHOUT_GUI_MODULE)
  set(WITHOUT_GRAPHICS_MODULE ON)
  set(WITHOUT_QML_ENGINE_MODULE ON)
  set(WITHOUT_TEXT_EDITOR_MODULE ON)
endif()
if(WITHOUT_DATA_ANALYSIS_MODULE)
  set(WITHOUT_VALIDATORS_MODULE ON)
  set(WITHOUT_TESTS_MANAGER_MODULE ON)
endif()
if(LGPL_ONLY)
  set(WITHOUT_FFTW_MODULE ON)
endif()
if(WITHOUT_SLICOT_MODULE)
  set(WITHOUT_CONTROL_SYSTEM_MODULE ON)
endif()
if(WITHOUT_LIBGIT2 AND WITHOUT_LIBCURL)
  set(WITHOUT_WEBTOOLS_MODULE ON)
endif()
if(WITHOUT_XML_MODULE)
  set(WITHOUT_HELP_TOOLS_MODULE ON)
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
if(CMAKE_SYSTEM_NAME
   STREQUAL
   "Darwin")
  set(CMAKE_MACOSX_RPATH ON)
  # Use @loader_path-based rpaths so the bundle is relocatable.
  # Libraries in lib/Nelson/ find siblings via @loader_path,
  # executables in bin/ find them via @loader_path/../lib/Nelson.
  set(CMAKE_INSTALL_RPATH
      "@loader_path"
      "@loader_path/../lib/Nelson"
      "@loader_path/../${CMAKE_INSTALL_LIBDIR}/Nelson")
else()
  set(CMAKE_INSTALL_RPATH
      "${CMAKE_INSTALL_PREFIX}/lib"
      "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
endif()
# ==============================================================================
# Platform-specific setup
# ==============================================================================
if(CMAKE_SYSTEM_NAME
   STREQUAL
   "Linux")
  if(CMAKE_SIZEOF_VOID_P
     EQUAL
     8)
    add_compile_definitions(_LARGEFILE_SOURCE _FILE_OFFSET_BITS=64)
  endif()
  set(BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin/linux)
elseif(
  CMAKE_SYSTEM_NAME
  STREQUAL
  "Darwin")
  include(MacOsConfig)
endif()
# ==============================================================================
# Third-party dependencies
# ==============================================================================
# -- Eigen3
find_package(
  Eigen3
  3.3
  QUIET
  NO_MODULE)
if(NOT Eigen3_FOUND)
  find_package(Eigen3 REQUIRED)
endif()
if(TARGET Eigen3::Eigen)
  message(STATUS "Found Eigen3 target: Eigen3::Eigen")
  get_target_property(
    EIGEN3_INCLUDE_DIR
    Eigen3::Eigen
    INTERFACE_INCLUDE_DIRECTORIES)
elseif(
  NOT
  DEFINED
  EIGEN3_INCLUDE_DIR
  AND DEFINED EIGEN3_INCLUDE_DIRS)
  set(EIGEN3_INCLUDE_DIR ${EIGEN3_INCLUDE_DIRS})
endif()
message(STATUS "Eigen3 include directory: ${EIGEN3_INCLUDE_DIR}")
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
find_package(PkgConfig REQUIRED)
# ==============================================================================
# -- Boost
find_package(
  Boost
  1.71.0
  REQUIRED
  CONFIG
  COMPONENTS serialization
             filesystem
             thread
             chrono
             regex
             date_time
             iostreams)
# ==============================================================================
# -- XML
if(NOT WITHOUT_XML_MODULE)
  find_package(LibXml2 REQUIRED)
  find_package(LibXslt REQUIRED)
endif()
# ==============================================================================
# -- BLAS / LAPACK
find_package(MKL)
find_package(OpenBLAS)
find_package(BLAS)
find_package(LAPACK)
find_package(LAPACKE)
if(MKL_FOUND)
  set(NELSON_BLAS_LAPACKX_LIBRARIES ${MKL_LIBRARIES})
elseif(OpenBLAS_FOUND)
  if(CMAKE_SYSTEM_NAME
     STREQUAL
     "Linux")
    if(NOT LAPACKE_FOUND)
      message(SEND_ERROR "LAPACKE Library required.")
    endif()
    set(NELSON_BLAS_LAPACKX_LIBRARIES ${OpenBLAS_LIB} ${LAPACKE_LIBRARIES})
  else()
    if(FORCE_LIBGFORTRAN_LINK)
      set(NELSON_BLAS_LAPACKX_LIBRARIES ${OpenBLAS_LIB} -lgfortran)
    else()
      set(NELSON_BLAS_LAPACKX_LIBRARIES ${OpenBLAS_LIB})
    endif()
  endif()
else()
  if(NOT BLAS_FOUND)
    message(SEND_ERROR "BLAS Library required.")
  endif()
  if(NOT LAPACK_FOUND)
    message(SEND_ERROR "LAPACK Library required.")
  endif()
  if(NOT LAPACKE_FOUND)
    message(SEND_ERROR "LAPACKE Library required.")
  endif()
  set(NELSON_BLAS_LAPACKX_LIBRARIES
      ${BLAS_LIBRARIES}
      ${LAPACK_LIBRARIES}
      ${LAPACKE_LIBRARIES})
endif()
message(
  STATUS "NELSON_BLAS_LAPACKX_LIBRARIES: ${NELSON_BLAS_LAPACKX_LIBRARIES}")
# ==============================================================================  
# -- HDF5
if(NOT WITHOUT_HDF5_MODULE)
  find_package(HDF5 REQUIRED)
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
if(NOT WITHOUT_AUDIO_MODULE)
  find_package(LibSndFile REQUIRED)
  find_package(Taglib)
  if(CMAKE_SYSTEM_NAME
     STREQUAL
     "Linux")
    find_package(ALSA REQUIRED)
    find_package(Jack REQUIRED)
  endif()
  find_package(Portaudio REQUIRED)
  include(PortAudioConfig)
endif()
# ==============================================================================
# -- MATIO
if(NOT WITHOUT_MATIO_MODULE)
  find_package(MATIO REQUIRED)
  if(MATIO_FOUND)
    message(STATUS "Found Matio lib: ${MATIO_LIBRARIES}")
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
# Boost compile definitions
# ==============================================================================
add_compile_definitions(BOOST_ALL_NO_LIB BOOST_ALL_DYN_LINK)
# ==============================================================================
# Output directories (development build)
# ==============================================================================
set(INSTALL_DIRECTORY "${CMAKE_INSTALL_PREFIX}Nelson-${Nelson_VERSION}")
if(CMAKE_SYSTEM_NAME
   STREQUAL
   "Darwin")
  set(LIBRARY_OUTPUT "Nelson-${Nelson_VERSION}/bin/macOS/")
else()
  set(LIBRARY_OUTPUT "Nelson-${Nelson_VERSION}/bin/linux/")
endif()
set(ROOT_OUTPUT "Nelson-${Nelson_VERSION}/")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${BIN_DIRECTORY})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${BIN_DIRECTORY})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${BIN_DIRECTORY})
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
# Module skeleton install
# ==============================================================================
include(ModuleSkeletonConfig)
# ==============================================================================
# Install documentation files
# ==============================================================================
set(_doc_dest "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(
  FILES ${CMAKE_SOURCE_DIR}/gpl-3.0.md
        ${CMAKE_SOURCE_DIR}/lgpl-3.0.md
        ${CMAKE_SOURCE_DIR}/CHANGELOG.md
        ${CMAKE_SOURCE_DIR}/CHANGELOG-0.1.x.md
        ${CMAKE_SOURCE_DIR}/CHANGELOG-0.2.x.md
        ${CMAKE_SOURCE_DIR}/CHANGELOG-0.3.x.md
        ${CMAKE_SOURCE_DIR}/CHANGELOG-0.4.x.md
        ${CMAKE_SOURCE_DIR}/CHANGELOG-0.5.x.md
        ${CMAKE_SOURCE_DIR}/CHANGELOG-0.6.x.md
        ${CMAKE_SOURCE_DIR}/CHANGELOG-0.7.x.md
        ${CMAKE_SOURCE_DIR}/CONTRIBUTORS.md
        ${CMAKE_SOURCE_DIR}/README.md
        ${CMAKE_SOURCE_DIR}/THIRDPARTY.md
        ${CMAKE_SOURCE_DIR}/CLA.md
  DESTINATION "${_doc_dest}")
# ==============================================================================
# CMake package config
# ==============================================================================
include(CMakePackageConfigHelpers)

set(NELSON_CMAKECONFIG_INSTALL_DIR
    "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
    CACHE STRING "Install path for NelsonConfig.cmake")

set(NELSON_DATA_INSTALL_DIR
    "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules"
    CACHE STRING "Install path for Nelson data files")

configure_package_config_file(
  ${PROJECT_NAME}Config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${PROJECT_NAME}Config.cmake"
  INSTALL_DESTINATION ${NELSON_CMAKECONFIG_INSTALL_DIR})

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
  VERSION ${Nelson_VERSION}
  COMPATIBILITY AnyNewerVersion)
# ==============================================================================
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${PROJECT_NAME}Config.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
        DESTINATION ${NELSON_CMAKECONFIG_INSTALL_DIR})
# ==============================================================================
install(
  EXPORT ${PROJECT_NAME}-targets
  FILE ${PROJECT_NAME}Targets.cmake
  DESTINATION ${NELSON_CMAKECONFIG_INSTALL_DIR})
# ==============================================================================
