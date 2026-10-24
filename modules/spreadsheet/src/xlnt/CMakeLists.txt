cmake_minimum_required(VERSION 3.1...4.3)
project(xlnt_all)

add_compile_options("$<$<C_COMPILER_ID:MSVC>:/utf-8>")
add_compile_options("$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")

# CTest setup
# include (CTest) # Add this for valgrind support; CTest works without it
enable_testing()

# This indicates to CMakeLists in subdirectories that they are part of a larger project
set(COMBINED_PROJECT TRUE)

# Library type
option(STATIC
  "Set to ON to build xlnt as a static library instead of a shared library"
  OFF
)

# C++ language standard to use
set(XLNT_CXX_VALID_LANGS 11 14 17 20 23 26)
set(XLNT_CXX_LANG "14" CACHE STRING "C++ language features to compile with")
# enumerate allowed values for cmake gui
set_property(CACHE XLNT_CXX_LANG PROPERTY STRINGS ${XLNT_CXX_VALID_LANGS})
# validate value is in XLNT_CXX_VALID_LANGS
list(FIND XLNT_CXX_VALID_LANGS ${XLNT_CXX_LANG} index)
if(index EQUAL -1)
  message(FATAL_ERROR "XLNT_CXX_LANG must be one of ${XLNT_CXX_VALID_LANGS}")
endif()
if(XLNT_CXX_LANG EQUAL 26)
  message(WARNING
    "Support for C++26 in XLNT is still experimental. Please report any C++26-related warnings or errors!"
  )
endif()

# C language standard to use
set(XLNT_C_VALID_LANGS 99 11 17 23)
set(XLNT_C_LANG "11" CACHE STRING "C language features to compile with")
mark_as_advanced(XLNT_C_LANG)
# enumerate allowed values for cmake gui
set_property(CACHE XLNT_C_LANG PROPERTY STRINGS ${XLNT_C_VALID_LANGS})
# validate value is in XLNT_C_VALID_LANGS
list(FIND XLNT_C_VALID_LANGS ${XLNT_C_LANG} index)
if(index EQUAL -1)
  message(FATAL_ERROR "XLNT_C_LANG must be one of ${XLNT_C_VALID_LANGS}")
endif()

# Optional components
option(LIBRARY "Set to ON to build the main XLNT library" ON)
option(TESTS "Set to ON to build test executable (in ./tests)" OFF)
option(SAMPLES "Set to ON to build executable code samples (in ./samples)" OFF)
option(BENCHMARKS
  "Set to ON to build performance benchmarks (in ./benchmarks)"
  OFF
)
option(PYTHON "Set to ON to build Arrow conversion functions (in ./python)" OFF)
option(XLNT_SANITIZE "Sanitize addresses" OFF)
mark_as_advanced(PYTHON)
option(DOCUMENTATION
  "Set to ON to build API reference documentation (in ./api-reference)"
  OFF
)

if(XLNT_SANITIZE)
  if(MSVC)
    # Require VS 2019 16.9+ (cl with /fsanitize=address)
    add_compile_options(/fsanitize=address /Zi)
  else()
    set(SANITIZER_FLAGS
      -fno-omit-frame-pointer
      -fsanitize=address
      -fsanitize=undefined
      -g
    )

    # LSan (leak sanitizer) is not supported on macOS with AppleClang
    if(NOT (CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang"))
      list(APPEND SANITIZER_FLAGS -fsanitize=leak)
    endif()

    add_compile_options(${SANITIZER_FLAGS})
    add_link_options(${SANITIZER_FLAGS})
  endif()
endif()

# Platform specific options
if(MSVC)
  option(STATIC_CRT "Link with the static version of MSVCRT (/MT[d])" OFF)

  if(STATIC_CRT AND NOT STATIC)
    message(FATAL_ERROR "STATIC_CRT is only supported for STATIC builds.")
  endif()
else()
  option(COVERAGE "Generate coverage data using gcov" OFF)
endif()

if(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  option(XLNT_ALL_WARNINGS_AS_ERRORS
    "Set to ON to convert all warnings into errors. This option is used during xlnt development/CI."
    OFF
  )
  mark_as_advanced(XLNT_ALL_WARNINGS_AS_ERRORS)
endif()

# Add components according to selected options
if(SAMPLES)
  add_subdirectory(samples)
endif()

if(BENCHMARKS)
  add_subdirectory(benchmarks)
endif()

if(TESTS)
  add_subdirectory(tests)
endif()

if(BENCHMARKS OR TESTS)
  option(XLNT_USE_LOCALE_COMMA_DECIMAL_SEPARATOR
    "Set to ON to enable unit tests and/or benchmarks that use a locale containing a comma as the decimal separator."
    OFF
  )
  option(XLNT_USE_LOCALE_ARABIC_DECIMAL_SEPARATOR
    "Set to ON to enable unit tests and/or benchmarks that use a locale containing the arabic decimal separator."
    OFF
  )
  set(
    XLNT_LOCALE_COMMA_DECIMAL_SEPARATOR
    "de_DE"
    CACHE
      STRING
      "The locale used to run unit tests and/or benchmarks using a comma as decimal separator"
  )
  set(
    XLNT_LOCALE_ARABIC_DECIMAL_SEPARATOR
    "ps_AF"
    CACHE
      STRING
      "The locale used to run unit tests and/or benchmarks using the arabic decimal separator"
  )
endif()

if(PYTHON)
  message(WARNING
    "The Python integration is currently not functional. We would accept a PR to fix the current issues."
  )
  add_subdirectory(python)
endif()

if(DOCUMENTATION)
  find_package(Doxygen REQUIRED)
  # set input and output files
  set(DOXYGEN_IN ${CMAKE_CURRENT_SOURCE_DIR}/docs/tools/Doxyfile)
  set(DOXYGEN_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

  # request to configure the file
  configure_file(${DOXYGEN_IN} ${DOXYGEN_OUT} @ONLY)

  # note the option ALL which allows to build the docs together with the application
  add_custom_target(doc_doxygen ALL
    COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating API documentation with Doxygen"
  )
endif()

if(LIBRARY)
  add_subdirectory(source)
endif()
