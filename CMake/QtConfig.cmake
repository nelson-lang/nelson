# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(DEFINED ENV{QTDIR})
  set(QTDIR "$ENV{QTDIR}")
endif()
if(QTDIR)
  list(PREPEND CMAKE_PREFIX_PATH "${QTDIR}")
endif()
if(DEFINED ENV{QTDIR_BINARIES})
  set(QTDIR_BINARIES "$ENV{QTDIR_BINARIES}")
else()
  if(QTDIR AND EXISTS "${QTDIR}/bin")
    set(ENV{QTDIR_BINARIES} "${QTDIR}/bin")
    set(QTDIR_BINARIES "${QTDIR}/bin")
  else()
    message(WARNING "Please define QTDIR_BINARIES environment variable.")
  endif()
endif()
# ==============================================================================
list(APPEND QtPackagesRequired
  Core
  Widgets
  Svg
  Gui
  Network
  OpenGL
  OpenGLWidgets
  Quick
  PrintSupport
)
# ==============================================================================
if(NOT WITHOUT_TEXT_EDITOR_MODULE)
  list(APPEND QtPackagesRequired PrintSupport Concurrent)
endif()
if(NOT WITHOUT_QML_ENGINE_MODULE)
  list(APPEND QtPackagesRequired Qml)
endif()
# ==============================================================================
find_package(
  Qt6
  6.2 QUIET REQUIRED
  COMPONENTS ${QtPackagesRequired}
)
message(STATUS "Qt6 detected and used.")
