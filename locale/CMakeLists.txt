# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
#
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# LICENCE_BLOCK_END
# ==============================================================================
if(NOT WITHOUT_LOCALIZATION_MODULE)
  include(GNUInstallDirs)
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/en_US/LC_MESSAGES/nelson.mo
          DESTINATION ${CMAKE_INSTALL_LOCALEDIR}/en_US/LC_MESSAGES)
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/fr_FR/LC_MESSAGES/nelson.mo
          DESTINATION ${CMAKE_INSTALL_LOCALEDIR}/fr_FR/LC_MESSAGES)
endif()
# ==============================================================================
