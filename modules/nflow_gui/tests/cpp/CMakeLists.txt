cmake_minimum_required(VERSION 3.16)

project(nflow_gui_worker_tests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(nflow_gui_worker_tests background_worker_pool_tests.cpp)

target_include_directories(nflow_gui_worker_tests
  PRIVATE "${CMAKE_CURRENT_LIST_DIR}/../../builtin/include"
)

if(MSVC)
  target_compile_options(nflow_gui_worker_tests PRIVATE /W4 /permissive-)
else()
  target_compile_options(nflow_gui_worker_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

enable_testing()
add_test(NAME nflow_gui_worker_tests COMMAND nflow_gui_worker_tests)
