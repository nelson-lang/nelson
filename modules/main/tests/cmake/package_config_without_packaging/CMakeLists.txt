cmake_minimum_required(VERSION 3.16)

project(package_config_without_packaging VERSION 2.0.0 LANGUAGES NONE)

get_filename_component(NELSON_SOURCE_ROOT
  "${CMAKE_CURRENT_LIST_DIR}/../../../../.."
  ABSOLUTE
)
set(CMAKE_SOURCE_DIR "${NELSON_SOURCE_ROOT}")
set(Nelson_VERSION_MAJOR 2)
set(Nelson_VERSION_MINOR 0)
set(Nelson_VERSION_MAINTENANCE 0)
set(Nelson_VERSION_BUILD 0)
set(ENABLE_PACKAGING OFF)
set(NELSON_DMG_LAYOUT_BACKEND DMGBUILD CACHE STRING "" FORCE)
set(Python3_EXECUTABLE /usr/bin/false CACHE FILEPATH "" FORCE)

include(GNUInstallDirs)
include("${NELSON_SOURCE_ROOT}/CMake/PackageConfig.cmake")
