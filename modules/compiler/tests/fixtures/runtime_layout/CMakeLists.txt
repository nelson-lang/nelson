# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
cmake_minimum_required(VERSION 3.20)
project(Nelson LANGUAGES CXX)
enable_testing()
get_filename_component(NELSON_SOURCE
  "${CMAKE_CURRENT_LIST_DIR}/../../../../.."
  ABSOLUTE
)
if(NOT TEST_ROOT)
  set(TEST_ROOT "${CMAKE_CURRENT_BINARY_DIR}/cases")
endif()
add_library(nlsCompiler_builtin INTERFACE)
function(_nelson_configure_main_executable target output)
  set_target_properties(${target} PROPERTIES OUTPUT_NAME "${output}")
  if(WIN32)
    target_link_options(${target} PRIVATE /STACK:62914560)
  endif()
endfunction()
add_subdirectory("${NELSON_SOURCE}/modules/main/nelsonc" launchers)
foreach(layout standard custom)
  add_executable(runtime_layout_${layout} runtime_layout.cpp)
  target_compile_features(runtime_layout_${layout} PRIVATE cxx_std_17)
  if(WIN32)
    target_link_libraries(runtime_layout_${layout} PRIVATE advapi32)
    target_sources(runtime_layout_${layout}
      PRIVATE "${NELSON_SOURCE}/modules/main/nelsonc/Packager.manifest"
    )
  endif()
  target_include_directories(runtime_layout_${layout}
    PRIVATE
      "${NELSON_SOURCE}/modules/main/nelsonc"
      "${NELSON_SOURCE}/modules/file_archiver/src/include"
      "${NELSON_SOURCE}/modules/core/src/include"
  )
  add_test(
    NAME runtime_layout_${layout}
    COMMAND runtime_layout_${layout} "${TEST_ROOT}/${layout}"
  )
endforeach()
target_compile_definitions(runtime_layout_standard
  PRIVATE $<TARGET_PROPERTY:nelson-app,COMPILE_DEFINITIONS>
)
target_compile_definitions(runtime_layout_custom
  PRIVATE
    NELSONC_INSTALL_DATADIR="assets/custom data"
    NELSONC_INSTALL_LIBDIR="private/lib/multiarch"
    NELSONC_INSTALL_BINDIR="tools/native/bin"
)
