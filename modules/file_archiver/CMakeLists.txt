# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name file_archiver)
set(module_library_name nlsFile_archiver)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  check_include_file(stdint.h HAVE_STDINT_H)
  check_include_file(inttypes.h HAVE_INTTYPES_H)
endif()
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  set(_minizip_dir "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip")
  file(
    GLOB
    _minizip_srcs
    "${_minizip_dir}/mz_compat.c"
    "${_minizip_dir}/mz_crypt.c"
    "${_minizip_dir}/mz_os.c"
    "${_minizip_dir}/mz_os_posix.c"
    "${_minizip_dir}/mz_strm_os_posix.c"
    "${_minizip_dir}/mz_strm.c"
    "${_minizip_dir}/mz_strm_buf.c"
    "${_minizip_dir}/mz_strm_mem.c"
    "${_minizip_dir}/mz_strm_split.c"
    "${_minizip_dir}/mz_strm_zlib.c"
    "${_minizip_dir}/mz_zip.c")
  nelson_module_sources_from_glob(
    FILE_ARCHIVER_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)
  list(
    APPEND
    FILE_ARCHIVER_SRC
    ${_minizip_srcs})
else()
  nelson_module_sources_from_glob(
    FILE_ARCHIVER_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp)
endif()

add_library(${module_library_name} ${FILE_ARCHIVER_SRC})

if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  target_compile_definitions(
    ${module_library_name}
    PRIVATE MZ_ZIP_NO_ENCRYPTION
            ZLIB_COMPAT
            HAVE_ZLIB
            $<$<BOOL:${HAVE_STDINT_H}>:HAVE_STDINT_H>
            $<$<BOOL:${HAVE_INTTYPES_H}>:HAVE_INTTYPES_H>)
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${_minizip_dir})
  target_link_libraries(
    ${module_library_name}
    PRIVATE nlsCharacters_encoding
            nlsCommons
            nlsError_manager
            nlsI18n
            ${ZLIB_LIBRARIES})
else()
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/no-src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
endif()
# ==============================================================================
nelson_install_library(${module_library_name})
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  nelson_module_sources_from_glob(
    FILE_ARCHIVER_BUILTIN
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

  add_library(${module_library_builtin_name} ${FILE_ARCHIVER_BUILTIN})

  target_include_directories(
    ${module_library_builtin_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include)

  target_link_libraries(
    ${module_library_builtin_name}
    PRIVATE ${module_library_name}
            nlsError_manager
            nlsI18n
            nlsInterpreter
            nlsTypes)

  nelson_install_library(${module_library_builtin_name})
  nelson_install_module_data(
    ${module_name}
    EXTRA_TEST_PATTERNS
    "*.zip")
endif()
# ==============================================================================
