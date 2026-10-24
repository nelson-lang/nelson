cmake_minimum_required(VERSION 3.16)

project(main_macos_bundle_tests NONE)

set(NELSON_SOURCE_ROOT "${CMAKE_CURRENT_LIST_DIR}/../../../..")
set(
  NELSON_BIN_DIRECTORY
  "${NELSON_SOURCE_ROOT}/bin/macOS"
  CACHE
    PATH
    "Directory containing the staged macOS applications"
)

enable_testing()
if(APPLE)
  add_test(
    NAME main_macos_bundle_tests
    COMMAND ${CMAKE_COMMAND} -DNELSON_BIN_DIRECTORY=${NELSON_BIN_DIRECTORY} -P
      ${CMAKE_CURRENT_LIST_DIR}/macos_bundle_tests.cmake
  )
  add_test(
    NAME main_package_config_without_packaging_tests
    COMMAND ${CMAKE_COMMAND} -S
      ${CMAKE_CURRENT_LIST_DIR}/package_config_without_packaging -B
      ${CMAKE_CURRENT_BINARY_DIR}/package_config_without_packaging
  )
endif()
