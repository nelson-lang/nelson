# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name elementary_mathematics)
set(module_library_name nlsElementary_mathematics)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
file(GLOB ELEMENTARY_MATHEMATICS_SRC
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
list(FILTER ELEMENTARY_MATHEMATICS_SRC EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_name} ${ELEMENTARY_MATHEMATICS_SRC})
# ==============================================================================
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
target_include_directories(
  ${module_library_name}
  PRIVATE ${OpenMP_CXX_INCLUDE_DIRS}
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include)
target_link_libraries(${module_library_name} ${OpenMP_CXX_LIBRARIES})
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
