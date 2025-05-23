# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
#Install the .desktop file
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  option(ENABLE_DESKTOP_ICONS_ASSOCIATION "Enable desktop icons association" ON)
  message(STATUS "Enable desktop icons association: ${ENABLE_DESKTOP_ICONS_ASSOCIATION}")
  if (ENABLE_DESKTOP_ICONS_ASSOCIATION)
    include(GNUInstallDirs)
    message(STATUS "CMAKE_INSTALL_DATADIR destination: ${CMAKE_INSTALL_DATADIR}")
    install(FILES ${CMAKE_SOURCE_DIR}/desktop/${NELSON_APP_ID}.appdata.xml DESTINATION ${CMAKE_INSTALL_DATADIR}/metainfo)
    install(FILES ${CMAKE_SOURCE_DIR}/desktop/${NELSON_APP_ID}.desktop DESTINATION ${CMAKE_INSTALL_DATADIR}/applications)
    install(DIRECTORY ${CMAKE_SOURCE_DIR}/desktop/icons DESTINATION ${CMAKE_INSTALL_DATADIR})
  endif()
endif()
# ==============================================================================
