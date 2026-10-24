cmake_minimum_required(VERSION 3.16)

project(graphics_qt_key_event_tests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(Qt6 6.2 REQUIRED COMPONENTS Widgets OpenGLWidgets)

set(GRAPHICS_QT_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")

add_executable(graphics_qt_key_event_tests
  key_event_tests.cpp
)
add_executable(graphics_qt_figure_workspace_tests figure_workspace_tests.cpp)
target_include_directories(graphics_qt_figure_workspace_tests
  PRIVATE "${GRAPHICS_QT_DIR}/src/include"
)
target_link_libraries(graphics_qt_figure_workspace_tests PRIVATE Qt6::Widgets)
add_library(graphics_qt_workspace_probe SHARED
  figure_workspace_probe.cpp
  figure_frame_probe.cpp
)
target_link_libraries(graphics_qt_workspace_probe
  PRIVATE
    Qt6::Widgets
    Qt6::OpenGLWidgets
)
add_executable(graphics_qt_frame_sample_tests figure_frame_sample_tests.cpp)
add_executable(graphics_qt_key_release_tests
  key_release_event_state_tests.cpp
)
add_executable(graphics_qt_native_resize_contract_tests
  native_resize_contract_tests.cpp
)
add_executable(graphics_qt_native_presentation_contract_tests
  native_presentation_contract_tests.cpp
)

target_include_directories(graphics_qt_key_event_tests
  PRIVATE "${GRAPHICS_QT_DIR}/src/include"
)
target_include_directories(graphics_qt_key_release_tests
  PRIVATE "${GRAPHICS_QT_DIR}/src/include"
)

target_link_libraries(graphics_qt_key_event_tests PRIVATE Qt6::Widgets)
target_link_libraries(graphics_qt_key_release_tests PRIVATE Qt6::Gui)

if(MSVC)
  target_compile_options(graphics_qt_key_event_tests PRIVATE /W4 /permissive-)
  target_compile_options(graphics_qt_key_release_tests PRIVATE /W4 /permissive-)
  target_compile_options(graphics_qt_native_resize_contract_tests
    PRIVATE
      /W4
      /permissive-
  )
  target_compile_options(graphics_qt_native_presentation_contract_tests
    PRIVATE
      /W4
      /permissive-
  )
else()
  target_compile_options(graphics_qt_key_event_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
  target_compile_options(graphics_qt_key_release_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
  target_compile_options(graphics_qt_native_resize_contract_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
  target_compile_options(graphics_qt_native_presentation_contract_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()
target_compile_definitions(graphics_qt_native_resize_contract_tests
  PRIVATE NELSON_SOURCE_ROOT="${GRAPHICS_QT_DIR}/../.."
)
target_compile_definitions(graphics_qt_native_presentation_contract_tests
  PRIVATE NELSON_SOURCE_ROOT="${GRAPHICS_QT_DIR}/../.."
)

enable_testing()
add_test(
  NAME graphics_qt_frame_sample_tests
  COMMAND graphics_qt_frame_sample_tests
)
add_test(
  NAME graphics_qt_figure_workspace_tests
  COMMAND graphics_qt_figure_workspace_tests
)
add_test(NAME graphics_qt_key_event_tests COMMAND graphics_qt_key_event_tests)
add_test(
  NAME graphics_qt_key_release_tests
  COMMAND graphics_qt_key_release_tests
)
add_test(
  NAME graphics_qt_native_resize_contract_tests
  COMMAND graphics_qt_native_resize_contract_tests
)
add_test(
  NAME graphics_qt_native_presentation_contract_tests
  COMMAND graphics_qt_native_presentation_contract_tests
)
set_tests_properties(graphics_qt_key_event_tests
  PROPERTIES ENVIRONMENT "QT_QPA_PLATFORM=offscreen"
)
