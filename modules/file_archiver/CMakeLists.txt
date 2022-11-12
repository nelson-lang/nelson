# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN SPDX-License-Identifier: LGPL-3.0-or-later
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
if(WITHOUT_FILE_ARCHIVER_MODULE)
  include_directories(${CMAKE_CURRENT_SOURCE_DIR}/no-src/include)
else()
  include_directories(
    ${Boost_INCLUDE_DIRS}
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
endif()
# ==============================================================================
if(WITHOUT_FILE_ARCHIVER_MODULE)
  file(GLOB FILE_ARCHIVER_SRC "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/*.cpp")
  list(FILTER FILE_ARCHIVER_SRC EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/dllMain.cpp")
else()
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
endif()
# ==============================================================================
add_library(${module_library_name} SHARED ${FILE_ARCHIVER_SRC})
# ==============================================================================
if(WITHOUT_FILE_ARCHIVER_MODULE)
  target_link_libraries(${module_library_name})
else()
  target_link_libraries(
    ${module_library_name}
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsI18n
    ${ZLIB_LIBRARIES})
endif()
# ==============================================================================
set_target_properties(
  ${module_library_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
# ==============================================================================
if(NOT WITHOUT_FILE_ARCHIVER_MODULE)
  # ==============================================================================
  file(GLOB FILE_ARCHIVER_BUILTIN
       "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
  list(FILTER FILE_ARCHIVER_BUILTIN EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
  # ==============================================================================
  include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include)
  # ==============================================================================
  add_library(${module_library_builtin_name} SHARED ${FILE_ARCHIVER_BUILTIN})
  # ==============================================================================
  target_link_libraries(
    ${module_library_builtin_name}
    ${module_library_name}
    nlsApi_nelson
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsTypes)
  # ==============================================================================
  set_target_properties(
    ${module_library_builtin_name}
    PROPERTIES
      VERSION
      ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
      SOVERSION ${Nelson_VERSION_MAJOR})
  # ==============================================================================
endif()
# ==============================================================================
if(WITHOUT_FILE_ARCHIVER_MODULE)
  install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
else()
  install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
  install(TARGETS ${module_library_builtin_name}
          LIBRARY DESTINATION ${LIBRARY_OUTPUT})
  install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/startup.m
          DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/etc)
  install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/finish.m
          DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/etc)
  install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/loader.m
          DESTINATION ${ROOT_OUTPUT}/modules/${module_name})
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/help
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.qch"
    PATTERN "xml" EXCLUDE
    PATTERN "md" EXCLUDE)
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.zip")
endif()
# ==============================================================================
