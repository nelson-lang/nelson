cmake_minimum_required(VERSION 3.16)

project(nelson_file_archiver_tests LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(FILE_ARCHIVER_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")

find_package(ZLIB REQUIRED)
if(APPLE)
  # Homebrew keeps libarchive out of the default prefix (keg-only).
  execute_process(
    COMMAND brew --prefix libarchive
    OUTPUT_VARIABLE _libarchive_prefix
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
  if(_libarchive_prefix AND IS_DIRECTORY "${_libarchive_prefix}")
    list(PREPEND CMAKE_PREFIX_PATH "${_libarchive_prefix}")
  endif()
endif()
find_package(LibArchive 3.6 REQUIRED)
find_package(PkgConfig QUIET)
if(PKG_CONFIG_FOUND)
  pkg_check_modules(PC_ZSTD QUIET libzstd)
endif()
find_path(ZSTD_INCLUDE_DIR zstd.h HINTS ${PC_ZSTD_INCLUDE_DIRS})
find_library(ZSTD_LIBRARY NAMES zstd libzstd HINTS ${PC_ZSTD_LIBRARY_DIRS})
set(WITH_ZSTD FALSE)
if(ZSTD_INCLUDE_DIR AND ZSTD_LIBRARY)
  set(WITH_ZSTD TRUE)
endif()

add_executable(nelson_file_archiver_tests
  zip_archive_tests.cpp
  "${FILE_ARCHIVER_DIR}/src/cpp/ZipArchive.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ZipCentralDirectory.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ZstdCodec.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ArchivePath.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ArchiveStreams.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/GzipCodec.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/TarArchive.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/TarLibarchiveReader.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/LibarchiveSupport.cpp"
  "${FILE_ARCHIVER_DIR}/src/cpp/ArchiveInterruption.cpp"
)

target_compile_definitions(nelson_file_archiver_tests
  PRIVATE
    NLSFILE_ARCHIVER_EXPORTS
    $<$<BOOL:${WITH_ZSTD}>:HAVE_ZSTD>
)

target_include_directories(nelson_file_archiver_tests
  PRIVATE
    "${FILE_ARCHIVER_DIR}/src/include"
    "${FILE_ARCHIVER_DIR}/src/cpp"
    ${LibArchive_INCLUDE_DIRS}
    $<$<BOOL:${WITH_ZSTD}>:${ZSTD_INCLUDE_DIR}>
)

target_link_libraries(nelson_file_archiver_tests
  PRIVATE
    ${LibArchive_LIBRARIES}
    ZLIB::ZLIB
    $<$<BOOL:${WITH_ZSTD}>:${ZSTD_LIBRARY}>
)

enable_testing()
add_test(
  NAME nelson_file_archiver_tests
  COMMAND nelson_file_archiver_tests
)
