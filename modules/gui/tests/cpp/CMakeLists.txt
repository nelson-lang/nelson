cmake_minimum_required(VERSION 3.16)

project(gui_json_tests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(GUI_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")

add_executable(gui_json_tests json_serialization_tests.cpp)
add_executable(gui_native_close_contract_tests
  native_close_contract_tests.cpp
)
add_executable(gui_native_launch_contract_tests
  native_launch_contract_tests.cpp
)

target_include_directories(gui_json_tests
  PRIVATE
    "${GUI_DIR}/src/cpp"
    "${GUI_DIR}/../commons/src/include"
)
target_compile_definitions(gui_native_close_contract_tests
  PRIVATE NELSON_SOURCE_ROOT="${GUI_DIR}/../.."
)
target_compile_definitions(gui_native_launch_contract_tests
  PRIVATE NELSON_SOURCE_ROOT="${GUI_DIR}/../.."
)

if(MSVC)
  target_compile_options(gui_json_tests PRIVATE /W4 /permissive-)
  target_compile_options(gui_native_close_contract_tests
    PRIVATE
      /W4
      /permissive-
  )
  target_compile_options(gui_native_launch_contract_tests
    PRIVATE
      /W4
      /permissive-
  )
else()
  target_compile_options(gui_json_tests PRIVATE -Wall -Wextra -Wpedantic)
  target_compile_options(gui_native_close_contract_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
  target_compile_options(gui_native_launch_contract_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

enable_testing()
add_test(NAME gui_json_tests COMMAND gui_json_tests)
add_test(
  NAME gui_native_close_contract_tests
  COMMAND gui_native_close_contract_tests
)
add_test(
  NAME gui_native_launch_contract_tests
  COMMAND gui_native_launch_contract_tests
)
