cmake_minimum_required(VERSION 3.16)

project(nelson_desktop_shell_tests LANGUAGES CXX)

set(CMAKE_AUTOMOC ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(Qt6 6.2 REQUIRED COMPONENTS Widgets)
message(STATUS "Qt6 detected and used for desktop shell tests.")

set(GUI_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")
set(REPOSITORY_DIR "${GUI_DIR}/../..")

add_executable(nelson_desktop_shell_tests
  desktop_shell_tests.cpp
  "${GUI_DIR}/src/cpp/NelsonApplication.cpp"
  "${GUI_DIR}/src/include/NelsonApplication.hpp"
  "${GUI_DIR}/src/cpp/CurrentDirectoryWidget.cpp"
  "${GUI_DIR}/src/cpp/CurrentDirectoryWidget.h"
  "${GUI_DIR}/src/cpp/DesktopCommandPalette.cpp"
  "${GUI_DIR}/src/cpp/DesktopCommandPalette.h"
  "${GUI_DIR}/src/cpp/DesktopPanelRail.cpp"
  "${GUI_DIR}/src/cpp/DesktopPanelRail.h"
  "${GUI_DIR}/src/cpp/DesktopUiStyle.cpp"
  "${GUI_DIR}/src/cpp/DesktopUiStyle.h"
  "${GUI_DIR}/src/cpp/QtDockTitleBar.cpp"
  "${GUI_DIR}/src/cpp/QtDockTitleBar.h"
  "${GUI_DIR}/../nelson_manager/src/cpp/RecentDirectoriesStore.cpp"
  "${GUI_DIR}/../nelson_manager/src/cpp/RecentDirectoriesStore.hpp"
  "${GUI_DIR}/../nelson_manager/src/cpp/DialogDirectoriesStore.cpp"
  "${GUI_DIR}/../nelson_manager/src/cpp/DialogDirectoriesStore.hpp"
)

target_include_directories(nelson_desktop_shell_tests
  PRIVATE
    "${GUI_DIR}/src/cpp"
    "${GUI_DIR}/src/include"
    "${GUI_DIR}/../commons/src/include"
    "${GUI_DIR}/../nelson_manager/src/cpp"
)

target_compile_definitions(nelson_desktop_shell_tests
  PRIVATE "NELSON_TEST_REPOSITORY_DIR=\"${REPOSITORY_DIR}\""
)

target_link_libraries(nelson_desktop_shell_tests
  PRIVATE Qt6::Widgets
)

enable_testing()
add_test(NAME nelson_desktop_shell_tests COMMAND nelson_desktop_shell_tests)
add_test(
  NAME nflow_file_icon_test
  COMMAND ${CMAKE_COMMAND} -DNELSON_TEST_REPOSITORY_DIR=${REPOSITORY_DIR} -P
    ${CMAKE_CURRENT_LIST_DIR}/nflow_file_icon_test.cmake
)
add_test(
  NAME application_icon_assets_test
  COMMAND ${CMAKE_COMMAND} -DNELSON_TEST_REPOSITORY_DIR=${REPOSITORY_DIR} -P
    ${CMAKE_CURRENT_LIST_DIR}/application_icon_assets_test.cmake
)
add_test(
  NAME graphics_toolbar_dispatch_test
  COMMAND ${CMAKE_COMMAND} -DNELSON_TEST_REPOSITORY_DIR=${REPOSITORY_DIR} -P
    ${CMAKE_CURRENT_LIST_DIR}/graphics_toolbar_dispatch_test.cmake
)
add_test(
  NAME desktop_docking_layout_test
  COMMAND ${CMAKE_COMMAND} -DNELSON_TEST_REPOSITORY_DIR=${REPOSITORY_DIR} -P
    ${CMAKE_CURRENT_LIST_DIR}/desktop_docking_layout_test.cmake
)
