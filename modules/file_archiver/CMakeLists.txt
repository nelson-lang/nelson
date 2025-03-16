# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
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
  add_definitions(-DMZ_ZIP_NO_ENCRYPTION)
  add_definitions(-DZLIB_COMPAT)
  add_definitions(-DHAVE_ZLIB)
  if(HAVE_STDINT_H)
    add_definitions(-DHAVE_STDINT_H)
  endif()
  if(HAVE_INTTYPES_H)
    add_definitions(-DHAVE_INTTYPES_H)
  endif()
endif()
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  file(
    GLOB
    FILE_ARCHIVER_SRC
    "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_compat.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_crypt.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_os.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_os_posix.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_strm_os_posix.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_strm.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_strm_buf.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_strm_mem.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_strm_split.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_strm_zlib.c"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip/mz_zip.c")

  list(FILTER FILE_ARCHIVER_SRC EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
else()
  file(GLOB FILE_ARCHIVER_SRC "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/*.cpp")
  list(FILTER FILE_ARCHIVER_SRC EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/dllMain.cpp")
endif()
# ==============================================================================
add_library(${module_library_name} ${FILE_ARCHIVER_SRC})
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
  target_include_directories(
    ${module_library_name}
    PRIVATE ${Boost_INCLUDE_DIRS}
            ${CMAKE_CURRENT_SOURCE_DIR}/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/src/c/minizip)

  target_link_libraries(${module_library_name} nlsCharacters_encoding
                        nlsCommons nlsError_manager nlsI18n ${ZLIB_LIBRARIES})
else()
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/no-src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
  target_include_directories(${module_library_name}
                             PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/no-src/include)

  target_link_libraries(${module_library_name})
endif()
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  file(GLOB FILE_ARCHIVER_BUILTIN
       "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
  list(FILTER FILE_ARCHIVER_BUILTIN EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
  # ==============================================================================
  add_library(${module_library_builtin_name} ${FILE_ARCHIVER_BUILTIN})
  target_include_directories(
    ${module_library_builtin_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
  target_include_directories(
    ${module_library_builtin_name}
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
            ${CMAKE_CURRENT_SOURCE_DIR}/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include)
  target_link_libraries(
    ${module_library_builtin_name}
    ${module_library_name}
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsTypes)
endif()
# ==============================================================================
include(GNUInstallDirs)
# ==============================================================================
install(
  TARGETS ${module_library_name}
  EXPORT ${PROJECT_NAME}-targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${module_name})
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  install(
    TARGETS ${module_library_builtin_name}
    EXPORT ${PROJECT_NAME}-targets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER
      DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${module_name})
  # ==============================================================================
  install(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/loader.m
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/etc
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.zip")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.qch"
    PATTERN "xml" EXCLUDE
    PATTERN "md" EXCLUDE)
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.m")
endif()
# ==============================================================================
