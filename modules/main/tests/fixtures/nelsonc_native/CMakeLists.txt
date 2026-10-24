cmake_minimum_required(VERSION 3.20)
project(nelsonc_native_fixture LANGUAGES C)
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
add_library(nc_native_leaf SHARED native_leaf.c)
add_library(nc_native_bridge SHARED native_bridge.c)
target_link_libraries(nc_native_bridge PRIVATE nc_native_leaf)
set(fixture_targets nc_native_leaf nc_native_bridge)
if(NELSON_SOURCE_DIR AND NELSON_MEX_LIBRARY AND NATIVE_MEX_EXTENSION)
  add_library(nc_native_mex SHARED native_mex.c)
  target_include_directories(nc_native_mex
    PRIVATE "${NELSON_SOURCE_DIR}/modules/mex/src/include"
  )
  target_link_libraries(nc_native_mex
    PRIVATE
      nc_native_bridge
      "${NELSON_MEX_LIBRARY}"
  )
  set_target_properties(nc_native_mex
    PROPERTIES
      SUFFIX ".${NATIVE_MEX_EXTENSION}"
      WINDOWS_EXPORT_ALL_SYMBOLS ON
  )
  list(APPEND fixture_targets nc_native_mex)
endif()
foreach(target IN LISTS fixture_targets)
  set_target_properties(${target}
    PROPERTIES
      PREFIX ""
      RUNTIME_OUTPUT_DIRECTORY "${NATIVE_OUTPUT_DIR}/$<0:>"
      LIBRARY_OUTPUT_DIRECTORY "${NATIVE_OUTPUT_DIR}/$<0:>"
      BUILD_RPATH "$ORIGIN"
      INSTALL_RPATH "$ORIGIN"
  )
endforeach()
