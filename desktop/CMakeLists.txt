# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software; you can redistribute it
# and/or modify it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the License,
# or (at your option) any later version.
#
# Alternatively, you can redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the Free Software Foundation;
# either version 2 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.
# LICENCE_BLOCK_END
# ==============================================================================
#Install the .desktop file
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  option(ENABLE_DESKTOP_ICONS_ASSOCIATION "Enable desktop icons association" OFF)
  if (ENABLE_PACKAGING)
    set(ENABLE_DESKTOP_ICONS_ASSOCIATION ON)
  endif()
  message(STATUS "Enable desktop icons association: ${ENABLE_DESKTOP_ICONS_ASSOCIATION}")
  if (ENABLE_DESKTOP_ICONS_ASSOCIATION)
    include(GNUInstallDirs)
    message(STATUS "CMAKE_INSTALL_DATADIR destination: ${CMAKE_INSTALL_DATADIR}")

    install(FILES ${CMAKE_SOURCE_DIR}/desktop/org.nelson.Nelson.appdata.xml DESTINATION ${CMAKE_INSTALL_DATADIR}/metainfo)
    install(FILES ${CMAKE_SOURCE_DIR}/desktop/org.nelson.Nelson.desktop DESTINATION ${CMAKE_INSTALL_DATADIR}/applications)
    install(DIRECTORY ${CMAKE_SOURCE_DIR}/desktop/icons DESTINATION ${CMAKE_INSTALL_DATADIR})

  endif()
endif()
# ==============================================================================
