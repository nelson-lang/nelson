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
  set(QTDIR $ENV{QTDIR})
else()
  if(NOT QTDIR)
    if(EXISTS $ENV{HOME}/Qt/5.15/clang_64)
      set(QTDIR $ENV{HOME}/Qt/5.15/clang_64)
    endif()
  endif()
endif()
set(CMAKE_PREFIX_PATH ${QTDIR})
if(DEFINED ENV{QTDIR_BINARIES})

else()
  if(EXISTS ${QTDIR}/bin)
    set(ENV{QTDIR_BINARIES} ${QTDIR}/bin)
    set(QTDIR_BINARIES ${QTDIR}/bin)
  else()
    message(WARNING "Please define QTDIR_BINARIES environment variable.")
  endif()
endif()
# ==============================================================================
find_package(QT NAMES Qt6 Qt5 REQUIRED COMPONENTS Core) 
if (FORCE_QT5)
  set(QT_VERSION_MAJOR 5)
else()
  find_package(QT NAMES Qt6 Qt5 REQUIRED COMPONENTS Core) 
endif()
# ==============================================================================
list(
  APPEND
  QtPackagesRequired
  Core
  Widgets
  Svg
  Gui
  Quick
  PrintSupport
)
# ==============================================================================
if (NOT WITHOUT_TEXT_EDITOR_MODULE)
list(APPEND QtPackagesRequired PrintSupport)
endif()
if (NOT WITHOUT_QML_ENGINE_MODULE)
list(APPEND QtPackagesRequired Qml)
endif()
# ==============================================================================
find_package(Qt${QT_VERSION_MAJOR} 5.15 REQUIRED COMPONENTS ${QtPackagesRequired})
message(STATUS "Qt${QT_VERSION_MAJOR} detected and used.")
# ==============================================================================
