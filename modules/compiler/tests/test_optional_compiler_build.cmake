# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
cmake_minimum_required(VERSION 3.20)
get_filename_component(REPOSITORY_ROOT
  "${CMAKE_CURRENT_LIST_DIR}/../../.."
  ABSOLUTE
)
set(CMAKE_SOURCE_DIR "${REPOSITORY_ROOT}")
set(CMAKE_INSTALL_LIBDIR lib)
# GNUInstallDirs needs an architecture even though this test creates no targets.
set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SIZEOF_VOID_P 8)
set(PROJECT_NAME Nelson)

# Exercise the actual module selector without configuring its native dependencies.
function(add_subdirectory name)
  set_property(GLOBAL APPEND PROPERTY selected_modules "${name}")
endfunction()
function(install)
endfunction()

function(check_selection compiler archive dynamic json expected)
  set(WITHOUT_COMPILER_MODULE ${compiler})
  set(WITHOUT_FILE_ARCHIVER_MODULE ${archive})
  set(WITHOUT_DYNAMIC_LINK_MODULE ${dynamic})
  set(WITHOUT_JSON_MODULE ${json})
  set_property(GLOBAL PROPERTY selected_modules "")
  include("${REPOSITORY_ROOT}/modules/CMakeLists.txt")
  get_property(selected GLOBAL PROPERTY selected_modules)
  list(FIND selected compiler index)
  if(expected AND index EQUAL -1)
    message(FATAL_ERROR "Compiler missing with all dependencies enabled")
  elseif(NOT expected AND NOT index EQUAL -1)
    message(FATAL_ERROR
      "Compiler selected despite exclusion or missing dependency"
    )
  endif()
  list(GET selected -1 last)
  if(NOT last STREQUAL main)
    message(FATAL_ERROR "Main must follow compiler and all other modules")
  endif()
endfunction()

foreach(compiler OFF ON)
  foreach(archive OFF ON)
    foreach(dynamic OFF ON)
      foreach(json OFF ON)
        set(expected OFF)
        if(NOT compiler AND NOT archive AND NOT dynamic AND NOT json)
          set(expected ON)
        endif()
        check_selection(${compiler} ${archive} ${dynamic} ${json} ${expected})
      endforeach()
    endforeach()
  endforeach()
endforeach()
message(STATUS "PASS optional compiler CMake selection: 16 combinations")
