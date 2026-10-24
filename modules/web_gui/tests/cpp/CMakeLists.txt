cmake_minimum_required(VERSION 3.16)

project(web_gui_json_tests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(WEB_GUI_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")

add_executable(web_gui_json_tests json_serialization_tests.cpp)
add_executable(web_gui_native_close_contract_tests
  native_close_contract_tests.cpp
)
add_executable(web_gui_native_launch_contract_tests
  native_launch_contract_tests.cpp
)

target_include_directories(web_gui_json_tests
  PRIVATE
    "${WEB_GUI_DIR}/src/cpp"
    "${WEB_GUI_DIR}/../commons/src/include"
)
target_compile_definitions(web_gui_native_close_contract_tests
  PRIVATE NELSON_SOURCE_ROOT="${WEB_GUI_DIR}/../.."
)
target_compile_definitions(web_gui_native_launch_contract_tests
  PRIVATE NELSON_SOURCE_ROOT="${WEB_GUI_DIR}/../.."
)

if(MSVC)
  target_compile_options(web_gui_json_tests PRIVATE /W4 /permissive-)
  target_compile_options(web_gui_native_close_contract_tests
    PRIVATE
      /W4
      /permissive-
  )
  target_compile_options(web_gui_native_launch_contract_tests
    PRIVATE
      /W4
      /permissive-
  )
else()
  target_compile_options(web_gui_json_tests PRIVATE -Wall -Wextra -Wpedantic)
  target_compile_options(web_gui_native_close_contract_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
  target_compile_options(web_gui_native_launch_contract_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

enable_testing()
add_test(NAME web_gui_json_tests COMMAND web_gui_json_tests)
add_test(
  NAME web_gui_native_close_contract_tests
  COMMAND web_gui_native_close_contract_tests
)
add_test(
  NAME web_gui_native_launch_contract_tests
  COMMAND web_gui_native_launch_contract_tests
)
