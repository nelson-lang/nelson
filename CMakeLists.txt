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
set(Nelson_VERSION_MAINTENANCE_DEFAULT 9)
set(Nelson_VERSION_BUILD_DEFAULT 0)
# ==============================================================================
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  option(CMAKE_INSTALL_PREFIX "Install directory used by instal." "$ENV{HOME}")
endif()
message(STATUS "CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX}")
# ==============================================================================
option(LGPL_ONLY "Build Nelson under LGPL v3.x only" OFF)
# ==============================================================================
option(MINIMAL_BUILD "Build mininal Nelson " OFF)
# ==============================================================================
option(WITHOUT_OPENMP "Disable OpenMP" OFF)
# ==============================================================================
list (APPEND
      CAN_BE_DISABLED_MODULES
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
      QML_ENGINE
      SIGNAL_PROCESSING
      VALIDATORS
      HDF5
      MATIO
      SLICOT
      HELP_TOOLS
      HELP_BROWSER
      TEXT_EDITOR
      DATA_ANALYSIS
      DYNAMIC_LINK
      TESTS_MANAGER
      GUI
      JSON
      NIG
      )
foreach(mod ${CAN_BE_DISABLED_MODULES})
  option(WITHOUT_${mod}_MODULE "Disable ${mod} module." OFF)
endforeach(mod)
# ==============================================================================
if (WITHOUT_GUI_MODULE)
  set(WITHOUT_GRAPHICS_MODULE ON)
  set(WITHOUT_QML_ENGINE_MODULE ON)  
  set(WITHOUT_HELP_BROWSER_MODULE ON)  
  set(WITHOUT_TEXT_EDITOR_MODULE ON)  
endif()
# ==============================================================================
if (WITHOUT_DATA_ANALYSIS_MODULE)
  set(WITHOUT_VALIDATORS_MODULE ON)  
  set(WITHOUT_TESTS_MANAGER_MODULE ON)
endif()
# ==============================================================================
if (MINIMAL_BUILD)
  set(WITHOUT_OPENMP ON)
  foreach(mod ${CAN_BE_DISABLED_MODULES})
    set(WITHOUT_${mod}_MODULE ON)
  endforeach(mod)
endif()
# ==============================================================================
if (LGPL_ONLY)
  set(WITHOUT_FFTW_MODULE ON)
  set(WITHOUT_SLICOT_MODULE ON)
endif()
# ==============================================================================
option(FORCE_QT5 "Force to use Qt5." OFF)
# ==============================================================================
option(ENABLE_CLANG_TIDY "Analyze code with clang-tidy." OFF)
option(ENABLE_CLANG_TIDY_FIX  "Analyze code with clang-tidy and fix errors." OFF)
# ==============================================================================
option(ENABLE_PACKAGING "Build Nelson for packaging" OFF)
# ==============================================================================
message(STATUS "Build Nelson for packaging: ${ENABLE_PACKAGING}")
# ==============================================================================
if (WITHOUT_OPENMP)
message(STATUS "OpenMP disabled.")
endif()
# ==============================================================================
if (MINIMAL_BUILD)
message(STATUS "Minimal build.")
endif()
# ==============================================================================
foreach(mod ${CAN_BE_DISABLED_MODULES})
  if (WITHOUT_${mod}_MODULE)
    message(STATUS "${mod} module disabled.")
  endif()
endforeach(mod)
# ==============================================================================
if (LGPL_ONLY OR (WITHOUT_FFTW_MODULE AND WITHOUT_SLICOT_MODULE))
  message(STATUS "LGPL v3.x: enabled.")
endif()
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
if (NOT WITHOUT_GUI_MODULE)
include(QtConfig)
endif()
# ==============================================================================
if (NOT WITHOUT_MPI_MODULE)
include(MpiConfig)
endif()
# ==============================================================================
include(IcuConfig)
# ==============================================================================
if (NOT WITHOUT_DYNAMIC_LINK_MODULE)
include(FfiConfig)
endif()
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
# ==============================================================================
if (NOT WITHOUT_HELP_TOOLS_MODULE)
find_package(LibXml2 REQUIRED)
endif()
# ==============================================================================
find_package(LAPACK REQUIRED)
# ==============================================================================
if (NOT WITHOUT_MPI_MODULE)
find_package(HDF5 REQUIRED)
endif()
# ==============================================================================
if (NOT WITHOUT_OPENMP)
find_package(OpenMP)
include (OpenMpConfig)
endif()
# ==============================================================================
if (NOT WITHOUT_FILE_ARCHIVER_MODULE)
find_package(ZLIB REQUIRED)
endif()
# ==============================================================================
if (NOT WITHOUT_WEBTOOLS_MODULE)
find_package(CURL REQUIRED)
find_package(Libgit2 REQUIRED)
endif()
# ==============================================================================
if (NOT WITHOUT_AUDIO_MODULE)
  find_package(LibSndFile REQUIRED)
  find_package(Taglib REQUIRED)
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    find_package(ALSA REQUIRED)
    find_package(Jack REQUIRED)
  endif()
  find_package(Portaudio REQUIRED)
  include (PortAudioConfig)
endif()
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  find_package(LAPACKE REQUIRED)
endif()
# ==============================================================================
if (NOT WITHOUT_MATIO_MODULE)
  find_package(MATIO REQUIRED)
  if(MATIO_FOUND)
    message(STATUS "Found Matio lib: ${MATIO_LIBRARIES}")
  endif()
endif()
# ==============================================================================
add_definitions(-DBOOST_ALL_NO_LIB)
add_definitions(-DBOOST_ALL_DYN_LINK)
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
