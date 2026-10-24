# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
cmake_minimum_required(VERSION 3.20)
project(NelsonRuntimeLibrarySearch LANGUAGES CXX)
enable_testing()
get_filename_component(NELSON_SOURCE
  "${CMAKE_CURRENT_LIST_DIR}/../../../../.."
  ABSOLUTE
)
add_executable(runtime_library_search runtime_library_search.cpp)
target_compile_features(runtime_library_search PRIVATE cxx_std_17)
target_include_directories(runtime_library_search
  PRIVATE "${NELSON_SOURCE}/modules/main/nelsonc"
)
add_test(NAME runtime_library_search COMMAND runtime_library_search)
if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_library(runtime_search_leaf SHARED leaf.cpp)
  add_library(runtime_search_bridge SHARED bridge.cpp)
  target_link_libraries(runtime_search_bridge PRIVATE runtime_search_leaf)
  set_target_properties(
    runtime_search_leaf
    runtime_search_bridge
    PROPERTIES
      SKIP_BUILD_RPATH TRUE
      LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/relocated runtime"
  )
  add_executable(runtime_search_probe runtime_search_probe.cpp)
  target_compile_features(runtime_search_probe PRIVATE cxx_std_17)
  target_include_directories(runtime_search_probe
    PRIVATE "${NELSON_SOURCE}/modules/main/nelsonc"
  )
  target_link_libraries(runtime_search_probe PRIVATE ${CMAKE_DL_LIBS})
  file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/invoking directory")
  foreach(mode
    fresh
    inherited
    already
    failed-exec
    failed-exec-empty
    failed-exec-inherited
  )
    set(environment --unset=LD_LIBRARY_PATH)
    if(mode STREQUAL "inherited")
      set(environment "LD_LIBRARY_PATH=/unused:/also-unused")
    elseif(mode STREQUAL "already")
      set(environment
        "LD_LIBRARY_PATH=$<TARGET_FILE_DIR:runtime_search_bridge>"
      )
    elseif(mode STREQUAL "failed-exec-empty")
      set(environment "LD_LIBRARY_PATH=")
    elseif(mode STREQUAL "failed-exec-inherited")
      set(environment "LD_LIBRARY_PATH=/unused:/also-unused")
    endif()
    add_test(
      NAME runtime_library_search_${mode}
      COMMAND "${CMAKE_COMMAND}" -E env ${environment}
        --unset=NELSONC_SEARCH_PROBE_STAGE "$<TARGET_FILE:runtime_search_probe>"
        "${mode}" "$<TARGET_FILE:runtime_search_bridge>" ""
        "argument with spaces" "--literal=$value"
    )
    set_tests_properties(runtime_library_search_${mode}
      PROPERTIES
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/invoking directory"
        TIMEOUT 15
    )
  endforeach()
endif()
