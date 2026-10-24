cmake_minimum_required(VERSION 3.16)

project(interpreter_lexer_tests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(INTERPRETER_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")

add_executable(interpreter_lexer_tests lexer_semantic_value_tests.cpp)

target_include_directories(interpreter_lexer_tests
  PRIVATE
    "${INTERPRETER_DIR}/src/include"
    "${INTERPRETER_DIR}/../types/src/include"
)

if(MSVC)
  target_compile_options(interpreter_lexer_tests PRIVATE /W4 /permissive-)
else()
  target_compile_options(interpreter_lexer_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

find_package(Threads REQUIRED)

add_executable(interpreter_runtime_dispatch_tests
  runtime_dispatch_idle_task_tests.cpp
  "${INTERPRETER_DIR}/src/cpp/RuntimeDispatch.cpp"
)

target_include_directories(interpreter_runtime_dispatch_tests
  PRIVATE "${INTERPRETER_DIR}/src/include"
)

# The dispatch source is compiled into the test, so it exports its symbols
# instead of importing them from the interpreter library.
target_compile_definitions(interpreter_runtime_dispatch_tests
  PRIVATE NLSINTERPRETER_EXPORTS
)

target_link_libraries(interpreter_runtime_dispatch_tests
  PRIVATE Threads::Threads
)

if(MSVC)
  target_compile_options(interpreter_runtime_dispatch_tests
    PRIVATE
      /W4
      /permissive-
  )
else()
  target_compile_options(interpreter_runtime_dispatch_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

enable_testing()
add_test(NAME interpreter_lexer_tests COMMAND interpreter_lexer_tests)
add_test(
  NAME interpreter_runtime_dispatch_tests
  COMMAND interpreter_runtime_dispatch_tests
)
