cmake_minimum_required(VERSION 3.16)
project(nelson_runtime_registry_tests LANGUAGES CXX)
if(NOT NELSON_SOURCE)
  get_filename_component(NELSON_SOURCE
    "${CMAKE_CURRENT_SOURCE_DIR}/../../../../.."
    ABSOLUTE
  )
endif()
add_executable(runtime-registry-probe registry.cpp)
target_compile_features(runtime-registry-probe PRIVATE cxx_std_17)
target_include_directories(runtime-registry-probe
  PRIVATE "${NELSON_SOURCE}/modules/main/nelsonc"
)
if(WIN32)
  target_link_libraries(runtime-registry-probe PRIVATE advapi32)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  target_compile_options(runtime-registry-probe
    PRIVATE
      -Wall
      -Wextra
      -Werror
      -pedantic
  )
endif()
include(CTest)
if(BUILD_TESTING AND CMAKE_SYSTEM_NAME STREQUAL "Linux")
  find_package(Python3 REQUIRED COMPONENTS Interpreter)
  file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test-work")
  add_test(
    NAME linux-runtime-registry
    COMMAND ${Python3_EXECUTABLE}
      "${CMAKE_CURRENT_SOURCE_DIR}/../../test_runtime_registry_linux.py"
      $<TARGET_FILE:runtime-registry-probe>
      "${CMAKE_CURRENT_BINARY_DIR}/test-work"
  )
  set_tests_properties(linux-runtime-registry PROPERTIES TIMEOUT 60)
endif()
