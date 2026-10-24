cmake_minimum_required(VERSION 3.16)

project(nelson_ipc_file_association_tests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(IPC_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")
set(STREAM_MANAGER_DIR "${IPC_DIR}/../stream_manager")

add_executable(nelson_ipc_file_association_tests
  file_association_message_tests.cpp
  "${IPC_DIR}/src/cpp/DataInterProcessToExchange.cpp"
)

target_include_directories(nelson_ipc_file_association_tests
  PRIVATE
    "${IPC_DIR}/src/include"
    "${STREAM_MANAGER_DIR}/src/include"
)

if(MSVC)
  target_compile_options(nelson_ipc_file_association_tests
    PRIVATE
      /W4
      /permissive-
  )
else()
  target_compile_options(nelson_ipc_file_association_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

enable_testing()
add_test(
  NAME nelson_ipc_file_association_tests
  COMMAND nelson_ipc_file_association_tests
)
