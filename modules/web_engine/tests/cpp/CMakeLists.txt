cmake_minimum_required(VERSION 3.16)

project(webview_helpers_tests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(WEB_ENGINE_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")
set(NELSON_SOURCE_ROOT "${WEB_ENGINE_DIR}/../..")

add_executable(webview_helpers_tests
  application_icon_tests.cpp
  webui_stubs.cpp
  "${WEB_ENGINE_DIR}/src/cpp/WebUiHelpers.cpp"
)

add_executable(webview_examples_catalog_tests
  examples_catalog_tests.cpp
  "${NELSON_SOURCE_ROOT}/modules/gui/src/cpp/ExamplesBridge.cpp"
  "${WEB_ENGINE_DIR}/src/cpp/ExamplesCatalog.cpp"
)

target_include_directories(webview_helpers_tests
  PRIVATE
    "${WEB_ENGINE_DIR}/src/include"
    "${WEB_ENGINE_DIR}/src/webui/include"
    "${NELSON_SOURCE_ROOT}/modules/nelson_manager/src/include"
    "${NELSON_SOURCE_ROOT}/modules/characters_encoding/src/include"
)
target_include_directories(webview_examples_catalog_tests
  PRIVATE
    "${WEB_ENGINE_DIR}/src/include"
    "${NELSON_SOURCE_ROOT}/modules/gui/src/include"
    "${NELSON_SOURCE_ROOT}/modules/commons/src/include"
)
target_compile_definitions(webview_helpers_tests
  PRIVATE
    NELSON_SOURCE_ROOT="${NELSON_SOURCE_ROOT}"
    # Plain extern declarations so the local webui stub satisfies the linker.
    WEBUI_EXPORT=extern
)
target_compile_definitions(webview_examples_catalog_tests
  PRIVATE
    NLSWEB_ENGINE_STATIC
    NELSON_SOURCE_ROOT="${NELSON_SOURCE_ROOT}"
)

if(MSVC)
  target_compile_options(webview_helpers_tests PRIVATE /W4 /permissive-)
else()
  target_compile_options(webview_helpers_tests PRIVATE -Wall -Wextra -Wpedantic)
endif()

if(MSVC)
  target_compile_options(webview_examples_catalog_tests
    PRIVATE
      /W4
      /permissive-
  )
else()
  target_compile_options(webview_examples_catalog_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

enable_testing()
add_test(NAME webview_helpers_tests COMMAND webview_helpers_tests)
add_test(
  NAME webview_examples_catalog_tests
  COMMAND webview_examples_catalog_tests
)
