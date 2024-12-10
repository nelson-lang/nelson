# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
#
# SPDX-License-Identifier: LGPL-3.0-or-later
#
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
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake/")
# ==============================================================================
project(Nelson)
# ==============================================================================
option(DEBUG_BUILD "Disable Release build" OFF)
if(DEBUG_BUILD)
  set(default_build_type "Debug")
else()
  set(default_build_type "Release")
endif()
# ==============================================================================
set(Nelson_VERSION_MAJOR_DEFAULT 1)
set(Nelson_VERSION_MINOR_DEFAULT 10)
set(Nelson_VERSION_MAINTENANCE_DEFAULT 0)
set(Nelson_VERSION_BUILD_DEFAULT 0)
# ==============================================================================
include(VersionConfig)
# ==============================================================================
set(Nelson_VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
)
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
option(WITHOUT_TBB "Disable TBB" OFF)
# ==============================================================================
option(WITHOUT_FILEWATCHER "Disable File watcher" OFF)
# ==============================================================================
option(FORCE_LIBGFORTRAN_LINK "Force libgfortran link" OFF)
# ==============================================================================
list(
  APPEND
  optional_modules
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
  CONTROL_SYSTEM
  HELP_TOOLS
  HELP_BROWSER
  TEXT_EDITOR
  DATA_ANALYSIS
  DYNAMIC_LINK
  TESTS_MANAGER
  GUI
  JSON
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
  PYTHON_ENGINE)
foreach(mod ${optional_modules})
  option(WITHOUT_${mod}_MODULE "Disable ${mod} module." OFF)
endforeach(mod)
# ==============================================================================
if(MINIMAL_BUILD)
  set(WITHOUT_FILEWATCHER ON)
  set(WITHOUT_OPENMP ON)
  set(WITHOUT_TBB ON)
  foreach(mod ${optional_modules})
    set(WITHOUT_${mod}_MODULE ON)
  endforeach(mod)
endif()
# ==============================================================================
if(WITHOUT_LOCALIZATION_MODULE)
  set(WITHOUT_I18N_MODULE ON)
endif()
# ==============================================================================
if(WITHOUT_GUI_MODULE)
  set(WITHOUT_GRAPHICS_MODULE ON)
  set(WITHOUT_QML_ENGINE_MODULE ON)
  set(WITHOUT_HELP_BROWSER_MODULE ON)
  set(WITHOUT_TEXT_EDITOR_MODULE ON)
endif()
# ==============================================================================
if(WITHOUT_DATA_ANALYSIS_MODULE)
  set(WITHOUT_VALIDATORS_MODULE ON)
  set(WITHOUT_TESTS_MANAGER_MODULE ON)
endif()
# ==============================================================================
if(LGPL_ONLY)
  set(WITHOUT_FFTW_MODULE ON)
  set(WITHOUT_SLICOT_MODULE ON)
endif()
# ==============================================================================
if (WITHOUT_SLICOT_MODULE)
  set(WITHOUT_CONTROL_SYSTEM_MODULE ON)
endif()
# ==============================================================================
option(FORCE_QT5 "Force to use Qt5." OFF)
# ==============================================================================
option(ENABLE_CLANG_TIDY "Analyze code with clang-tidy." OFF)
option(ENABLE_CLANG_TIDY_FIX "Analyze code with clang-tidy and fix errors." OFF)
# ==============================================================================
option(ENABLE_PACKAGING "Build Nelson for packaging" OFF)
# ==============================================================================
if(WITHOUT_OPENMP)
  message(STATUS "OpenMP disabled.")
endif()
# ==============================================================================
if(WITHOUT_TBB)
  message(STATUS "TBB disabled.")
endif()
# ==============================================================================
if(MINIMAL_BUILD)
  message(STATUS "Minimal build.")
endif()
# ==============================================================================
foreach(mod ${optional_modules})
  if(WITHOUT_${mod}_MODULE)
    message(STATUS "${mod} module disabled.")
  endif()
endforeach(mod)
# ==============================================================================
if(LGPL_ONLY OR (WITHOUT_FFTW_MODULE AND WITHOUT_SLICOT_MODULE))
  message(STATUS "LGPL v3.x: enabled.")
endif()
# ==============================================================================
if(POLICY CMP0068)
  cmake_policy(SET CMP0068 NEW)
endif()
# ==============================================================================
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
# ==============================================================================
if(NOT WITHOUT_PYTHON_ENGINE_MODULE)
  find_package (Python3 COMPONENTS Interpreter Development)
  if (NOT Python3_Development_FOUND)
    set(WITHOUT_PYTHON_ENGINE_MODULE ON)
    message(STATUS "Python Developpment not found.")
  else()
    message(STATUS "Found Python include dirs: ${Python3_INCLUDE_DIRS}")
  endif()
endif()
# ==============================================================================
include(OptionsConfig)
# ==============================================================================
include(C17Config)
# ==============================================================================
include(clang-tidy)
# ==============================================================================
include(TreeConfig)
# ==============================================================================
include(GNUInstallDirs)
set(CMAKE_INSTALL_RPATH
    "${CMAKE_INSTALL_PREFIX}/lib:${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}"
)
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
find_package(Eigen3 3.3 REQUIRED)
# ==============================================================================
if(NOT WITHOUT_GUI_MODULE)
  include(QtConfig)
endif()
# ==============================================================================
if(NOT WITHOUT_MPI_MODULE)
  include(MpiConfig)
endif()
# ==============================================================================
if(NOT WITHOUT_CHARACTERS_ENCODING_MODULE)
  include(IcuConfig)
endif()
# ==============================================================================
if(NOT WITHOUT_DYNAMIC_LINK_MODULE)
  include(FfiConfig)
endif()
# ==============================================================================
find_package(PkgConfig REQUIRED)
# ==============================================================================
list(
  APPEND
  boost_components
  serialization
  system
  filesystem
  thread
  chrono
  regex
  date_time
  iostreams)
find_package(
  Boost 1.64.0
  COMPONENTS ${boost_components}
  REQUIRED)
# ==============================================================================
if(NOT WITHOUT_HELP_TOOLS_MODULE)
  find_package(LibXml2 REQUIRED)
endif()
# ==============================================================================
find_package(MKL)
find_package(OpenBLAS)
find_package(BLAS)
find_package(LAPACK)
find_package(LAPACKE)
# ==============================================================================
if(MKL_FOUND)
  set(NELSON_BLAS_LAPACKX_LIBRARIES ${MKL_LIBRARIES})
else()
  if(OpenBLAS_FOUND)
    if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
      if(NOT LAPACKE_FOUND)
        message(SEND_ERROR "LAPACKE Library required.")
      endif()
      set(NELSON_BLAS_LAPACKX_LIBRARIES ${OpenBLAS_LIB} ${LAPACKE_LIBRARIES})
    else()
      if (FORCE_LIBGFORTRAN_LINK)
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
    set(NELSON_BLAS_LAPACKX_LIBRARIES ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES}
                                      ${LAPACKE_LIBRARIES})
  endif()
endif()
message(
  STATUS "NELSON_BLAS_LAPACKX_LIBRARIES: ${NELSON_BLAS_LAPACKX_LIBRARIES}")
# ==============================================================================
if(NOT WITHOUT_HDF5_MODULE)
  find_package(HDF5 REQUIRED)
endif()
# ==============================================================================
if(NOT WITHOUT_OPENMP)
  find_package(OpenMP)
endif()
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  find_package(ZLIB REQUIRED)
endif()
# ==============================================================================
if(NOT WITHOUT_WEBTOOLS_MODULE)
  find_package(CURL REQUIRED)
  find_package(libgit2 REQUIRED)
endif()
# ==============================================================================
if(NOT WITHOUT_AUDIO_MODULE)
  find_package(LibSndFile REQUIRED)
  find_package(Taglib)
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    find_package(ALSA REQUIRED)
    find_package(Jack REQUIRED)
  endif()
  find_package(Portaudio REQUIRED)
  include(PortAudioConfig)
endif()
# ==============================================================================
if(NOT WITHOUT_MATIO_MODULE)
  find_package(MATIO REQUIRED)
  if(MATIO_FOUND)
    message(STATUS "Found Matio lib: ${MATIO_LIBRARIES}")
  endif()
endif()
# ==============================================================================
if(NOT WITHOUT_OPENMP)
  find_package(TBB)
endif()
# ==============================================================================
add_definitions(-DBOOST_ALL_NO_LIB)
add_definitions(-DBOOST_ALL_DYN_LINK)
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  set(BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin/linux)
endif()
# =============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  include(MacOsConfig)
endif()
# ==============================================================================
set(INSTALL_DIRECTORY "${CMAKE_INSTALL_PREFIX}Nelson-${Nelson_VERSION}")
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(LIBRARY_OUTPUT "Nelson-${Nelson_VERSION}/bin/macOS/")
else()
  set(LIBRARY_OUTPUT "Nelson-${Nelson_VERSION}/bin/linux/")
endif()
set(ROOT_OUTPUT "Nelson-${Nelson_VERSION}/")
# ==============================================================================
# dev build path is different of installed version
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
include(GNUInstallDirs)
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/gpl-3.0.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/lgpl-3.0.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.1.x.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.2.x.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.3.x.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.4.x.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.5.x.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.6.x.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.7.x.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/CONTRIBUTORS.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/README.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/THIRDPARTY.md
        DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
install(
  FILES ${CMAKE_SOURCE_DIR}/CLA.md
  DESTINATION DESTINATION
  "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
# ==============================================================================
include(CMakePackageConfigHelpers)
# ==============================================================================
set(NELSON_CMAKECONFIG_INSTALL_DIR
    "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
    CACHE STRING "install path for nelsonConfig.cmake")
# ==============================================================================
set(NELSON_DATA_INSTALL_DIR
    "${CMAKE_DATA_ROOTDIR}/${PROJECT_NAME}/modules"
    CACHE STRING "install path for Nelson data files")
# ==============================================================================
configure_package_config_file(
  ${PROJECT_NAME}Config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${PROJECT_NAME}Config.cmake"
  INSTALL_DESTINATION ${NELSON_CMAKECONFIG_INSTALL_DIR})
# ==============================================================================
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
