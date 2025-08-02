# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
include(GNUInstallDirs)
# ==============================================================================
if (NOT WITHOUT_GUI_MODULE)
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/resources
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}
  FILES_MATCHING
  PATTERN "*.png")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/resources
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}
  FILES_MATCHING
  PATTERN "*.ico")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/resources
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}
  FILES_MATCHING
  PATTERN "*.svg")
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/resources/fonts/Hack-Regular.ttf DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/resources/fonts)
install(FILES ${CMAKE_SOURCE_DIR}/resources/fonts/LICENSE DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/resources/fonts)
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/resources/qss/Combinear.qss DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/resources/qss)
install(FILES ${CMAKE_SOURCE_DIR}/resources/qss/License.txt DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/resources/qss)
# ==============================================================================
endif()
# ==============================================================================
