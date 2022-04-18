# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  message(SEND_ERROR "On Windows, please uses Visual studio solution.")
endif()
# ==============================================================================
message(
  STATUS
    "CMake version ${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}.${CMAKE_PATCH_VERSION}"
)
# ==============================================================================
cmake_minimum_required(VERSION 3.9)
# ==============================================================================
project(Nelson)
# ==============================================================================
set(Nelson_VERSION_MAJOR_DEFAULT 0)
set(Nelson_VERSION_MINOR_DEFAULT 6)
set(Nelson_VERSION_MAINTENANCE_DEFAULT 4)
set(Nelson_VERSION_BUILD_DEFAULT 0)
# ==============================================================================
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  option(CMAKE_INSTALL_PREFIX "Install directory used by instal." "$ENV{HOME}")
endif()
message(STATUS "CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX}")
# ==============================================================================
option(ENABLE_CLANG_TIDY "Analyze code with clang-tidy." OFF)
option(ENABLE_CLANG_TIDY_FIX  "Analyze code with clang-tidy and fix errors." OFF)
# ==============================================================================
option(ENABLE_PACKAGING "Build Nelson for packaging" OFF)
# ==============================================================================
option(WITH_FFTW "Build Nelson without FFTW module" ON)
option(WITH_SLICOT "Build Nelson without SLICOT module" ON)
option(LGPL21_ONLY "Build Nelson under LGPL v2.1 only" OFF)
# ==============================================================================
if (LGPL21_ONLY)
  set(WITH_FFTW OFF)
  set(WITH_SLICOT OFF)
  message(STATUS "LGPL v2.1: ${LGPL21_ONLY}")
endif()
# ==============================================================================
message(STATUS "Build Nelson for packaging: ${ENABLE_PACKAGING}")
message(STATUS "FFTW module: ${WITH_FFTW}")
message(STATUS "SLICOT module: ${WITH_SLICOT}")
# ==============================================================================
if(POLICY CMP0068)
  cmake_policy(SET CMP0068 NEW)
endif()
# ==============================================================================
set(CMAKE_BUILD_TYPE "Release")
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake/")
set(BUILD_SHARED_LIBS "true")
# ==============================================================================
include (VersionConfig)
# ==============================================================================
include (OptionsConfig)
# ==============================================================================
include(C17Config)
# ==============================================================================
include(clang-tidy)
# ==============================================================================
include(TreeConfig)
# ==============================================================================
set(CMAKE_SKIP_BUILD_RPATH TRUE)
set(CMAKE_INSTALL_RPATH ".")
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(CMAKE_MACOSX_RPATH 1)
endif()
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  set(BITNESS 32)
  if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(BITNESS 64)
    add_definitions(-D_LARGEFILE_SOURCE)
    add_definitions(-D_FILE_OFFSET_BITS=64)
  endif()
endif()
# ==============================================================================
include(Eigen3Config)
# ==============================================================================
include(Qt5Config)
# ==============================================================================
include(MpiConfig)
# ==============================================================================
include(IcuConfig)
# ==============================================================================
include(FfiConfig)
# ==============================================================================
find_package(PkgConfig REQUIRED)
find_package(
  Boost 1.64.0
  COMPONENTS serialization
             system
             filesystem
             locale
             thread
             chrono
             regex
             date_time
             iostreams
  REQUIRED)
find_package(MKL)  
find_package(Qt5Widgets REQUIRED)
find_package(Qt5Gui REQUIRED)
find_package(Qt5Qml REQUIRED)
find_package(Qt5Help REQUIRED)
find_package(Qt5Quick REQUIRED)
find_package(Qt5PrintSupport REQUIRED)
find_package(LibXml2 REQUIRED)
find_package(LibSndFile REQUIRED)
find_package(Taglib REQUIRED)
find_package(LAPACK REQUIRED)
find_package(HDF5 REQUIRED)
find_package(ZLIB REQUIRED)
find_package(CURL REQUIRED)
find_package(Libgit2 REQUIRED)
find_package(OpenMP)
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  find_package(ALSA REQUIRED)
  find_package(Jack REQUIRED)
endif()
# ==============================================================================
find_package(Portaudio REQUIRED)
include (PortAudioConfig)
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  find_package(LAPACKE REQUIRED)
endif()
# ==============================================================================
include (OpenMpConfig)
# ==============================================================================
find_package(MATIO REQUIRED)
if(MATIO_FOUND)
  message(STATUS "Found Matio lib: ${MATIO_LIBRARIES}")
endif()
# ==============================================================================
add_definitions(-DBOOST_ALL_NO_LIB)
add_definitions(-DBOOST_ALL_DYN_LINK)
add_definitions(-DOMPI_SKIP_MPICXX)
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  set(BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin/linux)
endif()
# =============================================================================
include(MacOsConfig)
# ==============================================================================
set(INSTALL_DIRECTORY
    "${CMAKE_INSTALL_PREFIX}Nelson-${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}"
)
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(LIBRARY_OUTPUT
      "Nelson-${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}/bin/macOS/"
  )
else()
  set(LIBRARY_OUTPUT
      "Nelson-${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}/bin/linux/"
  )
endif()
set(ROOT_OUTPUT
    "Nelson-${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}/"
)
# ==============================================================================
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${BIN_DIRECTORY})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${BIN_DIRECTORY})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${BIN_DIRECTORY})
# ==============================================================================
include(PackageConfig)
# ==============================================================================
add_subdirectory(modules)
add_subdirectory(etc)
add_subdirectory(locale)
add_subdirectory(resources)
add_subdirectory(desktop)
# ==============================================================================
include(ModuleSkeletonConfig)
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/gpl-3.0.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/lgpl-3.0.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.1.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.2.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.3.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.4.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.5.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CONTRIBUTORS.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/README.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/THIRDPARTY.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CLA.md DESTINATION ${ROOT_OUTPUT})
# ==============================================================================
