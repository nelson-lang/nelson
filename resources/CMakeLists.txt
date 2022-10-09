# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if (NOT WITHOUT_GUI_MODULE)
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/resources
  DESTINATION ${ROOT_OUTPUT}
  FILES_MATCHING
  PATTERN "*.png")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/resources
  DESTINATION ${ROOT_OUTPUT}
  FILES_MATCHING
  PATTERN "*.ico")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/resources
  DESTINATION ${ROOT_OUTPUT}
  FILES_MATCHING
  PATTERN "*.svg")
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/resources/fonts/Hack-Regular.ttf DESTINATION ${ROOT_OUTPUT}/resources/fonts)
install(FILES ${CMAKE_SOURCE_DIR}/resources/fonts/LICENSE DESTINATION ${ROOT_OUTPUT}/resources/fonts)
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/resources/qss/Combinear.qss DESTINATION ${ROOT_OUTPUT}/resources/qss)
install(FILES ${CMAKE_SOURCE_DIR}/resources/qss/License.txt DESTINATION ${ROOT_OUTPUT}/resources/qss)
# ==============================================================================
endif()
# ==============================================================================
