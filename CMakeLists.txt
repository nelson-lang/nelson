# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN This program is free software; you can redistribute it
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
set(Nelson_VERSION_MINOR_DEFAULT 4)
set(Nelson_VERSION_MAINTENANCE_DEFAULT 9)
set(Nelson_VERSION_BUILD_DEFAULT 0)
# ==============================================================================
if(DEFINED ENV{NELSON_VERSION_MAJOR})
  set(Nelson_VERSION_MAJOR $ENV{NELSON_VERSION_MAJOR})
else()
  set(Nelson_VERSION_MAJOR ${Nelson_VERSION_MAJOR_DEFAULT})
endif()
# ==============================================================================
if(DEFINED ENV{Nelson_VERSION_MINOR})
  set(Nelson_VERSION_MINOR $ENV{Nelson_VERSION_MINOR})
else()
  set(Nelson_VERSION_MINOR ${Nelson_VERSION_MINOR_DEFAULT})
endif()
# ==============================================================================
if(DEFINED ENV{NELSON_VERSION_MAINTENANCE})
  set(Nelson_VERSION_MAINTENANCE $ENV{NELSON_VERSION_MAINTENANCE})
else()
  set(Nelson_VERSION_MAINTENANCE ${Nelson_VERSION_MAINTENANCE_DEFAULT})
endif()
# ==============================================================================
if(DEFINED ENV{NELSON_VERSION_BUILD})
  set(Nelson_VERSION_BUILD $ENV{NELSON_VERSION_BUILD})
else()
  set(Nelson_VERSION_BUILD ${Nelson_VERSION_BUILD_DEFAULT})
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
option(WITHOUT_FFTW "Build Nelson without FFTW module" OFF)
option(WITHOUT_SLICOT "Build Nelson without SLICOT module" OFF)
option(LGPL21_ONLY "Build Nelson under LGPL v2.1 only" OFF)
# ==============================================================================
if(POLICY CMP0068)
  cmake_policy(SET CMP0068 NEW)
endif()
# ==============================================================================
if(EXISTS ${CMAKE_SOURCE_DIR}/modules/core/src/include/Nelson_VERSION.h)

else()
  configure_file(
    "${CMAKE_SOURCE_DIR}/modules/core/src/include/Nelson_VERSION.h.in"
    "${CMAKE_SOURCE_DIR}/modules/core/src/include/Nelson_VERSION.h")
endif()
# ==============================================================================
if(EXISTS ${CMAKE_SOURCE_DIR}/modules/modules.nls)

else()
  if(WITHOUT_FFTW OR LGPL21_ONLY)
    set(WITH_FFTW_MODULE "// modules_list = [modules_list; \"fftw\"];")
  else()
    set(WITH_FFTW_MODULE "modules_list = [modules_list; \"fftw\"];")
  endif()
  if(WITHOUT_SLICOT OR LGPL21_ONLY)
    set(WITH_SLICOT_MODULE "// modules_list = [modules_list; \"slicot\"];")
  else()
    set(WITH_SLICOT_MODULE "modules_list = [modules_list; \"slicot\"];")
  endif()
  configure_file("${CMAKE_SOURCE_DIR}/modules/modules.nls.in"
                 "${CMAKE_SOURCE_DIR}/modules/modules.nls")
endif()
# ==============================================================================
include(Clang-cxx-dev-tools)
# ==============================================================================
include(CheckCXXCompilerFlag)
# ==============================================================================
check_cxx_compiler_flag("-std=c++11" COMPILER_SUPPORTS_CXX11)
if(COMPILER_SUPPORTS_CXX11)
  set(CMAKE_CXX_STANDARD 11)
else()
  message(
    FATAL_ERROR
      "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support. Please use a different C++ compiler."
  )
endif()
# ==============================================================================
check_cxx_compiler_flag("-fPIC" COMPILER_SUPPORTS_FPIC)
if(COMPILER_SUPPORTS_FPIC)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fvisibility=hidden")
# ==============================================================================
include(CheckSymbolExists)
check_symbol_exists("fseek64" "stdio.h" HAVE_FSEEK64)
check_symbol_exists("ftell64" "stdio.h" HAVE_FTELL64)
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
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  get_filename_component(
    MYEIGEN ${CMAKE_SOURCE_DIR}/../nelson-thirdparty-linux${BITNESS}/Eigen
    ABSOLUTE)
endif()
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  get_filename_component(
    MYEIGEN ${CMAKE_SOURCE_DIR}/../nelson-thirdparty-macosx/Eigen ABSOLUTE)
endif()
if(EXISTS ${MYEIGEN})
  set(EIGEN3_INCLUDE_DIR ${MYEIGEN})
else()
  find_package(Eigen3 3.3 REQUIRED)
endif()
# ==============================================================================
if(DEFINED ENV{QTDIR})
  set(QTDIR $ENV{QTDIR})
else()
  if(NOT QTDIR)
    if(EXISTS $ENV{HOME}/Qt/5.15/clang_64)
      set(QTDIR $ENV{HOME}/Qt/5.15/clang_64)
    endif()
  endif()
  if(NOT QTDIR)
    if(EXISTS $ENV{HOME}/Qt/5.14.2/clang_64)
      set(QTDIR $ENV{HOME}/Qt/5.14.2/clang_64)
    endif()
  endif()
  if(NOT QTDIR)
    if(EXISTS $ENV{HOME}/Qt/5.12.3/clang_64)
      set(QTDIR $ENV{HOME}/Qt/5.12.3/clang_64)
    endif()
  endif()
endif()
set(CMAKE_PREFIX_PATH ${QTDIR})
if(DEFINED ENV{QTDIR_BINARIES})

else()
  if(EXISTS ${QTDIR}/bin)
    set(ENV{QTDIR_BINARIES} ${QTDIR}/bin)
    set(QTDIR_BINARIES ${QTDIR}/bin)
  else()
    message(WARNING "Please define QTDIR_BINARIES environment variable.")
  endif()
endif()
# ==============================================================================
find_package(MPI REQUIRED)
if(MPI_INCLUDE_PATH AND MPI_LIBRARIES)
  message(STATUS "MPI_COMPILE_FLAGS=${MPI_COMPILE_FLAGS}")
  message(STATUS "MPI_LINK_FLAGS=${MPI_LINK_FLAGS}")
  message(STATUS "MPI_INCLUDE_PATH=${MPI_INCLUDE_PATH}")
  message(STATUS "MPI_LIBRARIES=${MPI_LIBRARIES}")
endif(MPI_INCLUDE_PATH AND MPI_LIBRARIES)
check_symbol_exists("MPI_Get_library_version" "mpi.h"
                    HAVE_MPI_GET_LIBRARY_VERSION)
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  execute_process(
    COMMAND brew --prefix icu4c
    OUTPUT_VARIABLE BREW_ICU_PREFIX
    RESULT_VARIABLE NONZERO_BREW_EXIT_CODE
    ERROR_VARIABLE BREW_ERROR
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE)
  if(NOT (NONZERO_BREW_EXIT_CODE))
    set(CMAKE_ICU_PATH ${BREW_ICU_PREFIX})
  else()
    message(
      FATAL_ERROR
        "Brew reported an error:\n${BREW_ERROR}.\nPlease resolve this error.")
  endif()

  if(NOT ICU_INCLUDE_DIRS)
    if(EXISTS "/usr/local/Cellar/icu4c/67.1/include")
      set(ICU_INCLUDE_DIRS "/usr/local/Cellar/icu4c/67.1/include")
    endif()
  endif()
  if(NOT ICU_INCLUDE_DIRS)
    if(EXISTS "/usr/local/Cellar/icu4c/64.2/include")
      set(ICU_INCLUDE_DIRS "/usr/local/Cellar/icu4c/64.2/include")
    endif()
  endif()
  if(NOT ICU_INCLUDE_DIRS)
    if(EXISTS "/usr/local/Cellar/icu4c/63.1/include")
      set(ICU_INCLUDE_DIRS "/usr/local/Cellar/icu4c/63.1/include")
    endif()
  endif()
  if(NOT ICU_INCLUDE_DIRS)
    if(EXISTS "/usr/local/Cellar/icu4c/60.1/include")
      set(ICU_INCLUDE_DIRS "/usr/local/Cellar/icu4c/60.1/include")
    endif()
  endif()
  if(NOT ICU_INCLUDE_DIRS)
    if(EXISTS "/usr/local/Cellar/icu4c/59.1/include")
      set(ICU_INCLUDE_DIRS "/usr/local/Cellar/icu4c/59.1/include")
    endif()
  endif()
  if(NOT ICU_INCLUDE_DIRS)
    if(EXISTS "${CMAKE_ICU_PATH}/include")
      set(ICU_INCLUDE_DIRS "${CMAKE_ICU_PATH}/include")
    endif()
  endif()
  if(NOT ICU_LIBRARIES)
    if(EXISTS "/usr/local/Cellar/icu4c/67.1/lib/libicuuc.dylib")
      set(ICU_LIBRARIES /usr/local/Cellar/icu4c/67.1/lib/libicuuc.dylib)
    endif()
    if(EXISTS "/usr/local/Cellar/icu4c/67.1/lib/libicui18n.dylib")
      set(ICU_LIBRARIES
          "${ICU_LIBRARIES};/usr/local/Cellar/icu4c/67.1/lib/libicui18n.dylib")
    endif()
  endif()
  if(NOT ICU_LIBRARIES)
    if(EXISTS "/usr/local/Cellar/icu4c/64.2/lib/libicuuc.dylib")
      set(ICU_LIBRARIES /usr/local/Cellar/icu4c/64.2/lib/libicuuc.dylib)
    endif()
    if(EXISTS "/usr/local/Cellar/icu4c/64.2/lib/libicui18n.dylib")
      set(ICU_LIBRARIES
          "${ICU_LIBRARIES};/usr/local/Cellar/icu4c/64.2/lib/libicui18n.dylib")
    endif()
  endif()
  if(NOT ICU_LIBRARIES)
    if(EXISTS "/usr/local/Cellar/icu4c/63.1/lib/libicuuc.dylib")
      set(ICU_LIBRARIES /usr/local/Cellar/icu4c/63.1/lib/libicuuc.dylib)
    endif()
    if(EXISTS "/usr/local/Cellar/icu4c/63.1/lib/libicui18n.dylib")
      set(ICU_LIBRARIES
          "${ICU_LIBRARIES};/usr/local/Cellar/icu4c/63.1/lib/libicui18n.dylib")
    endif()
  endif()
  if(NOT ICU_LIBRARIES)
    if(EXISTS "/usr/local/Cellar/icu4c/60.1/lib/libicuuc.dylib")
      set(ICU_LIBRARIES /usr/local/Cellar/icu4c/60.1/lib/libicuuc.dylib)
    endif()
    if(EXISTS "/usr/local/Cellar/icu4c/60.1/lib/libicui18n.dylib")
      set(ICU_LIBRARIES
          "${ICU_LIBRARIES};/usr/local/Cellar/icu4c/60.1/lib/libicui18n.dylib")
    endif()
  endif()
  if(NOT ICU_LIBRARIES)
    if(EXISTS "/usr/local/Cellar/icu4c/59.1/lib/libicuuc.dylib")
      set(ICU_LIBRARIES /usr/local/Cellar/icu4c/59.1/lib/libicuuc.dylib)
    endif()
    if(EXISTS "/usr/local/Cellar/icu4c/59.1/lib/libicui18n.dylib")
      set(ICU_LIBRARIES
          "${ICU_LIBRARIES};/usr/local/Cellar/icu4c/59.1/lib/libicui18n.dylib")
    endif()
  endif()
  if(NOT ICU_LIBRARIES)
    if(EXISTS "${CMAKE_ICU_PATH}/lib/libicuuc.dylib")
      set(ICU_LIBRARIES ${CMAKE_ICU_PATH}/lib/libicuuc.dylib)
    endif()
    if(EXISTS ${CMAKE_ICU_PATH}/lib/libicui18n.dylib)
      set(ICU_LIBRARIES
          "${ICU_LIBRARIES};${CMAKE_ICU_PATH}/lib/libicui18n.dylib")
    endif()
  endif()
endif()
# ==============================================================================
if(NOT ICU_INCLUDE_DIRS OR NOT ICU_LIBRARIES)
  find_package(
    ICU
    COMPONENTS uc i18n
    REQUIRED)
endif()
# ==============================================================================
if(ICU_INCLUDE_DIRS AND ICU_LIBRARIES)
  message(STATUS "ICU_INCLUDE_DIRS=${ICU_INCLUDE_DIRS}")
  message(STATUS "ICU_LIBRARIES=${ICU_LIBRARIES}")
else()
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    message(FATAL_ERROR "Please install: brew install icu4c.")
  else()
    message(FATAL_ERROR "Please install icu.")
  endif()
endif()
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  execute_process(
    COMMAND brew --prefix libffi
    OUTPUT_VARIABLE BREW_LIBFFI_PREFIX
    RESULT_VARIABLE NONZERO_BREW_EXIT_CODE
    ERROR_VARIABLE BREW_ERROR
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE)
  if(NOT (NONZERO_BREW_EXIT_CODE))
    set(CMAKE_LIBFFI_PATH ${BREW_LIBFFI_PREFIX})
  else()
    message(
      FATAL_ERROR
        "Brew reported an error:\n${BREW_ERROR}.\nPlease resolve this error.")
  endif()
  if(NOT LIBFFI_INCLUDE_DIR)
    if(EXISTS "/usr/local/opt/libffi/include/ffi.h")
      set(LIBFFI_INCLUDE_DIR /usr/local/opt/libffi/include)
    endif()
  endif()
  if(NOT LIBFFI_INCLUDE_DIR)
    if(EXISTS "${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/include")
      set(LIBFFI_INCLUDE_DIR ${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/include)
    endif()
  endif()
  if(NOT LIBFFI_LIBRARY)
    if(EXISTS "${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/lib/libffi.dylib")
      set(LIBFFI_LIBRARY
          "${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/lib/libffi.dylib")
    endif()
    if(EXISTS "${CMAKE_LIBFFI_PATH}/lib/libffi.dylib")
      set(LIBFFI_LIBRARY ${CMAKE_LIBFFI_PATH}/lib/libffi.dylib)
    endif()
  endif()
endif()
# ==============================================================================
if(NOT LIBFFI_LIBRARY)
  find_package(LIBFFI REQUIRED)
endif()
# ==============================================================================
if(LIBFFI_LIBRARY)
  message(STATUS "LIBFFI_INCLUDE_DIR=${LIBFFI_INCLUDE_DIR}")
  message(STATUS "LIBFFI_LIBRARY=${LIBFFI_LIBRARY}")
else()
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    message(FATAL_ERROR "Please install: brew install libffi.")
  else()
    message(FATAL_ERROR "Please install libffi.")
  endif()
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
find_package(Portaudio REQUIRED)
if(PORTAUDIO_FOUND)
  # workaround for github CI with catalina
  if(EXISTS "/usr/local/Cellar/portaudio/19.6.0/include")
    set(PORTAUDIO_INCLUDE_DIRS /usr/local/Cellar/portaudio/19.6.0/include)
  endif()
  if(EXISTS "/usr/local/Cellar/portaudio/19.6.0/include")
    set(PORTAUDIO_LIBRARIES
        /usr/local/Cellar/portaudio/19.6.0/lib/libportaudio.dylib)
  endif()
  message(STATUS "Found portaudio: ${PORTAUDIO_LIBRARIES}")
endif()
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  find_package(LAPACKE REQUIRED)
endif()
# ==============================================================================
if(OPENMP_FOUND)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif()
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
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin/macosx64)
  set(CMAKE_OSX_ARCHITECTURES "x86_64")
  set(Boost_NO_SYSTEM_PATHS "TRUE")
  set(MAC_FRAMEWORK_FOUNDATION_LIBRARY "-framework Foundation")
  set(MAC_FRAMEWORK_APPKIT_LIBRARY "-framework AppKit")
  if(EXISTS "/usr/local/lib/liblapacke.a")
    link_directories(/usr/local/lib)
    set(MAC_LAPACKE_LIBRARY -lblas -llapack -llapacke)
  else()
    get_filename_component(
      LAPACKE_LIB
      ${CMAKE_SOURCE_DIR}/../Nelson-thirdparty-macosx/LAPACKE/sierra/lib
      ABSOLUTE)
    if(EXISTS ${LAPACKE_LIB})
      link_directories(${LAPACKE_LIB})
      set(MAC_LAPACKE_LIBRARY -lblas -llapack -llapacke)
    else()
      message(SEND_ERROR "LAPACKE not detected.")
    endif()
  endif()
  # macports paths
  if(EXISTS "/opt/local/include")
    include_directories(/opt/local/include)
  endif()
  if(EXISTS "/opt/local/lib")
    link_directories(/opt/local/lib)
  endif()
endif()
# ==============================================================================
set(INSTALL_DIRECTORY
    "${CMAKE_INSTALL_PREFIX}Nelson-${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}"
)
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(LIBRARY_OUTPUT
      "Nelson-${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}/bin/macosx64/"
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
set(CPACK_PACKAGE_NAME "Nelson")
set(CPACK_PACKAGE_VENDOR "Allan CORNET")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "An numerical software")
set(CPACK_PACKAGE_VERSION_MAJOR ${Nelson_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${Nelson_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${Nelson_VERSION_MAINTENANCE})
set(CPACK_PACKAGE_VERSION
    "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}.${Nelson_VERSION_BUILD}"
)
set(CPACK_PACKAGE_INSTALL_DIRECTORY "Nelson-${CPACK_PACKAGE_VERSION}")
set(CPACK_GENERATOR "TZ")
include(CPack)
# ==============================================================================
add_subdirectory(modules)
add_subdirectory(etc)
add_subdirectory(locale)
add_subdirectory(resources)
# ==============================================================================
if(EXISTS "${CMAKE_SOURCE_DIR}/module_skeleton")
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/etc/startup.nls
          DESTINATION ${ROOT_OUTPUT}/module_skeleton/etc)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/etc/finish.nls
          DESTINATION ${ROOT_OUTPUT}/module_skeleton/etc)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/loader.nls
          DESTINATION ${ROOT_OUTPUT}/module_skeleton)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/builder.nls
          DESTINATION ${ROOT_OUTPUT}/module_skeleton)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/module.json
          DESTINATION ${ROOT_OUTPUT}/module_skeleton)
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/functions
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.nlf")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/help
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/builtin
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/src
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/tests
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
endif()
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/COPYING DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/COPYING.LGPLv2.1 DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.1.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.2.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CHANGELOG-0.3.x.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CONTRIBUTORS.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/README.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/THIRDPARTY.md DESTINATION ${ROOT_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/CLA.md DESTINATION ${ROOT_OUTPUT})
# ==============================================================================
