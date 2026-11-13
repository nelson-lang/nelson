cmake_minimum_required(VERSION 3.16)

project(nelson_file_archiver_fuzz LANGUAGES C CXX)

# NELSON_FUZZ=ON: libFuzzer targets (clang, or MSVC with /fsanitize=fuzzer).
# NELSON_FUZZ=OFF: the same targets replay the corpus once, as ctest regression tests.
option(NELSON_FUZZ "Build libFuzzer targets" OFF)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(FILE_ARCHIVER_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")

if(APPLE)
  # Homebrew keeps libarchive out of the default prefix (keg-only).
  execute_process(
    COMMAND brew --prefix libarchive
    OUTPUT_VARIABLE _libarchive_prefix
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  if(_libarchive_prefix AND IS_DIRECTORY "${_libarchive_prefix}")
    list(PREPEND CMAKE_PREFIX_PATH "${_libarchive_prefix}")
  endif()
endif()
find_package(LibArchive 3.6 REQUIRED)

if(NELSON_FUZZ)
  if(MSVC)
    set(_fuzz_flags /fsanitize=fuzzer /fsanitize=address /Zi)
    set(_fuzz_link_flags /DEBUG)
  else()
    set(_fuzz_flags
      -fsanitize=fuzzer,address,undefined
      -fno-omit-frame-pointer
      -g
    )
    set(_fuzz_link_flags -fsanitize=fuzzer,address,undefined)
  endif()
endif()

set(_corpus "${CMAKE_CURRENT_LIST_DIR}/corpus/zip")

# name: target name; ARGN: sources besides the fuzz target file.
function(nelson_add_fuzzer name)
  set(_sources "${CMAKE_CURRENT_LIST_DIR}/${name}.cpp" ${ARGN})
  if(NOT NELSON_FUZZ)
    list(APPEND _sources "${CMAKE_CURRENT_LIST_DIR}/replay_main.cpp")
  endif()
  add_executable(${name} ${_sources})
  target_compile_definitions(${name} PRIVATE NLSFILE_ARCHIVER_EXPORTS)
  target_include_directories(${name}
    PRIVATE
      "${FILE_ARCHIVER_DIR}/src/include"
      "${FILE_ARCHIVER_DIR}/src/cpp"
      ${LibArchive_INCLUDE_DIRS}
  )
  target_link_libraries(${name} PRIVATE ${LibArchive_LIBRARIES})
  if(NELSON_FUZZ)
    target_compile_options(${name} PRIVATE ${_fuzz_flags})
    target_link_options(${name} PRIVATE ${_fuzz_link_flags})
  else()
    add_test(NAME ${name}_corpus COMMAND ${name} "${_corpus}")
  endif()
endfunction()

nelson_add_fuzzer(
  zip_central_directory_fuzzer
  "${FILE_ARCHIVER_DIR}/src/cpp/ZipCentralDirectory.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ArchivePath.cpp"
)
nelson_add_fuzzer(
  zip_archive_fuzzer
  "${FILE_ARCHIVER_DIR}/src/cpp/ZipCentralDirectory.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ZipArchive.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ArchivePath.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/LibarchiveSupport.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ArchiveInterruption.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ArchiveStreams.cpp"
)

enable_testing()
