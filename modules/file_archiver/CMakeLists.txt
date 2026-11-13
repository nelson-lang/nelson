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
  nelson_module_sources_from_glob(
    FILE_ARCHIVER_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
  )
else()
  nelson_module_sources_from_glob(
    FILE_ARCHIVER_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp
  )
endif()

add_library(${module_library_name} ${FILE_ARCHIVER_SRC})

if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  target_include_directories(${module_library_name}
    PUBLIC
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
      $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
      ${LibArchive_INCLUDE_DIRS}
      ${ZLIB_INCLUDE_DIRS}
      ${ZLIB_INCLUDE_DIR}
  )
  target_link_libraries(${module_library_name}
    PRIVATE
      nlsCharacters_encoding
      nlsCommons
      nlsError_manager
      nlsI18n
      ${LibArchive_LIBRARIES}
      ${ZLIB_LIBRARIES}
  )
  if(ZSTD_FOUND)
    target_compile_definitions(${module_library_name} PRIVATE HAVE_ZSTD)
    target_include_directories(${module_library_name}
      PRIVATE ${ZSTD_INCLUDE_DIR}
    )
    target_link_libraries(${module_library_name} PRIVATE ${ZSTD_LIBRARY})
  endif()
else()
  target_include_directories(${module_library_name}
    PUBLIC
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/no-src/include>
      $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  )
endif()
# ==============================================================================
nelson_install_library(${module_library_name})
if(WIN32 AND ZSTD_FOUND AND ZSTD_RUNTIME_DIR)
  install(
    FILES "${ZSTD_RUNTIME_DIR}/zstd.dll"
    DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
endif()
if(WIN32 AND NOT WITHOUT_FILE_ARCHIVER_MODULE AND LIBARCHIVE_RUNTIME_DIR)
  file(GLOB _nelson_libarchive_dlls "${LIBARCHIVE_RUNTIME_DIR}/*.dll")
  install(FILES ${_nelson_libarchive_dlls} DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  nelson_module_sources_from_glob(
    FILE_ARCHIVER_BUILTIN
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
  )

  add_library(${module_library_builtin_name} ${FILE_ARCHIVER_BUILTIN})

  target_include_directories(${module_library_builtin_name}
    PUBLIC
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
      $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
  )

  target_link_libraries(${module_library_builtin_name}
    PRIVATE
      ${module_library_name}
      nlsCharacters_encoding
      nlsCommons
      nlsError_manager
      nlsI18n
      nlsInterpreter
      nlsNelson_manager
      nlsStream_manager
      nlsTypes
  )

  nelson_install_library(${module_library_builtin_name})
  nelson_install_module_data(
    ${module_name}
    EXTRA_TEST_PATTERNS
    "*.zip"
  )
endif()
