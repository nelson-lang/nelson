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
  install(FILES ${CMAKE_SOURCE_DIR}/locale/nelson-en_US.json
        DESTINATION ${CMAKE_INSTALL_DATADIR}/locale)
  install(FILES ${CMAKE_SOURCE_DIR}/locale/nelson-fr_FR.json
        DESTINATION ${CMAKE_INSTALL_DATADIR}/locale)
endif()
# ==============================================================================

