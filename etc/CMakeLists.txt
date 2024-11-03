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
include(GNUInstallDirs)
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/etc/finish.m
        DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/etc)
install(FILES ${CMAKE_SOURCE_DIR}/etc/defaults.conf
        DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/etc)
install(FILES ${CMAKE_SOURCE_DIR}/etc/startup.m
        DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/etc)
# ==============================================================================
