# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name graphics_qt)
set(module_library_name nlsGraphics_qt)
# ==============================================================================
find_package(OpenGL REQUIRED)
# ==============================================================================
# Qt/OpenGL render backends: RenderQt (QPainter), RenderOpenGL, their BSP/patch
# helpers and the shared render helpers (symbol drawing, DPI/point conversion).
# Split out of graphics so the render backend is a module of its own, mirroring
# render_web. Needs only header-only bits of graphics (RenderInterface,
# GOPropertyValues, RenderQtHelpers) — no symbol back-dependency, so the link
# graph stays acyclic: graphics/graphics_io link nlsRenderQt, not the reverse.
# See modules/web_gui/ZEROQT.md.
# ==============================================================================
set(CMAKE_AUTOMOC ON)
# ==============================================================================
nelson_module_sources_from_glob(
  GRAPHICS_QT_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
  EXCLUDE
  "moc_.*\\.cpp$"
)
# ==============================================================================
add_library(${module_library_name} ${GRAPHICS_QT_SRC})
# ==============================================================================
target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../graphics/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../render_qt/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../render_web/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../font_metrics/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../text_editor/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
)
# ==============================================================================
target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsNelson_manager
    nlsGraphics
    nlsRenderQt
    nlsI18n
    nlsInterpreter
    nlsError_manager
    nlsTypes
    nlsEngine
    nlsText_editor
    nlsGui
    Qt6::Core
    Qt6::Gui
    Qt6::Widgets
    Qt6::OpenGL
    Qt6::PrintSupport
    Qt6::OpenGLWidgets
    OpenGL::GL
    ${CMAKE_DL_LIBS}
)
# ==============================================================================
set_target_properties(${module_library_name}
  PROPERTIES
    VERSION ${Nelson_VERSION}
    SOVERSION ${Nelson_VERSION_MAJOR}
)
# ==============================================================================
nelson_install_library(${module_library_name})
