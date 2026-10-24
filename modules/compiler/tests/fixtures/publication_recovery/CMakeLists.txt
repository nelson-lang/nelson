cmake_minimum_required(VERSION 3.18)
project(publication_recovery LANGUAGES CXX)
if(NOT UNIX)
  message(FATAL_ERROR
    "This fixture exercises POSIX publication; use the Nelson process-recovery tests on Windows."
  )
endif()
if(NOT DEFINED NELSON_SOURCE)
  get_filename_component(NELSON_SOURCE
    "${CMAKE_CURRENT_SOURCE_DIR}/../../../../.."
    ABSOLUTE
  )
endif()
add_executable(publication_recovery
  publication_recovery.cpp
  "${NELSON_SOURCE}/modules/compiler/src/cpp/ApplicationPublication.cpp"
)
target_compile_features(publication_recovery PRIVATE cxx_std_17)
target_include_directories(publication_recovery
  PRIVATE
    "${NELSON_SOURCE}/modules/compiler/src/cpp"
    "${NELSON_SOURCE}/modules/compiler/src/include"
    "${NELSON_SOURCE}/modules/file_archiver/src/include"
    "${NELSON_SOURCE}/modules/characters_encoding/src/include"
    "${NELSON_SOURCE}/modules/commons/src/include"
    "${NELSON_SOURCE}/modules/core/src/include"
)
enable_testing()
add_test(
  NAME publication_recovery
  COMMAND publication_recovery "${CMAKE_CURRENT_BINARY_DIR}/work"
)
