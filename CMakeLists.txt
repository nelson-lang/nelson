# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software; you can redistribute it
# and/or modify it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the License,
# or (at your option) any later version.
#
# Alternatively, you can redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the Free Software Foundation;
# either version 2 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.
# LICENCE_BLOCK_END
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
set(CMAKE_INSTALL_PREFIX "$ENV{HOME}")
# ==============================================================================
set(Nelson_VERSION_MAJOR_DEFAULT 0)
set(Nelson_VERSION_MINOR_DEFAULT 5)
set(Nelson_VERSION_MAINTENANCE_DEFAULT 12)
set(Nelson_VERSION_BUILD_DEFAULT 0)
# ==============================================================================
option(WITHOUT_FFTW "Build Nelson without FFTW module" OFF)
option(WITHOUT_SLICOT "Build Nelson without SLICOT module" OFF)
option(LGPL21_ONLY "Build Nelson under LGPL v2.1 only" OFF)
# ==============================================================================
if(POLICY CMP0068)
  cmake_policy(SET CMP0068 NEW)
endif()
# ==============================================================================
set(CMAKE_BUILD_TYPE "Release")
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake/")
set(BUILD_SHARED_LIBS "true")
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  message(SEND_ERROR "On Windows, please uses Visual studio solution.")
endif()
# ==============================================================================
include (VersionConfig)
# ==============================================================================
include (OptionsConfig)
# ==============================================================================
include(C11Config)
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
find_package(Qt5Sql REQUIRED)
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
  set(BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin/linux${BITNESS})
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
      "Nelson-${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}/bin/linux${BITNESS}/"
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
# ==============================================================================
include(ModuleSkeletonConfig)
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/COPYING DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/COPYING.LGPLv2.1 DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.1.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.2.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.3.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.4.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CONTRIBUTORS.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/README.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/THIRDPARTY.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CLA.md DESTINATION ${ROOT_OUTPUT})
# ==============================================================================
