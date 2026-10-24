# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name qml_engine)
set(module_library_name nlsQml_engine)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
set(CMAKE_AUTOMOC ON)
# ==============================================================================
nelson_module_sources_from_glob(
  QML_ENGINE_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
  EXCLUDE
  "moc_.*\\.cpp$"
)

add_library(${module_library_name} ${QML_ENGINE_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${EIGEN3_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../handle/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../display_format/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsNelson_manager
    nlsCharacters_encoding
    nlsCommons
    nlsDisplay_format
    nlsError_manager
    nlsGui
    nlsI18n
    nlsInterpreter
    nlsTypes
    Qt6::Core
    Qt6::Widgets
    Qt6::Qml
    Qt6::Quick
    ${CMAKE_DL_LIBS}
)
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  QML_ENGINE_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${QML_ENGINE_BUILTIN})

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../handle/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsError_manager
    nlsHandle
    nlsI18n
    nlsInterpreter
    nlsTypes
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.js"
  "*.qml"
  EXTRA_DIR_PATTERNS
  # "*.*" ships every file with an extension (*.qml, *.js, LICENSE.md, ...); the
  # extensionless "qmldir" must be listed explicitly or it is dropped. Without
  # it, qcharts_demo/QChartGallery_qt6.qml's `import "."` cannot resolve its
  # local Chart type and QQuickView creation fails (test_qml_demo_qcharts).
  "examples:*.*,qmldir"
)
