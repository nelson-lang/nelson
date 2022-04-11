# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/locale/en_US/LC_MESSAGES/nelson.mo
        DESTINATION ${ROOT_OUTPUT}/locale/en_US/LC_MESSAGES)
install(FILES ${CMAKE_SOURCE_DIR}/locale/fr_FR/LC_MESSAGES/nelson.mo
        DESTINATION ${ROOT_OUTPUT}/locale/fr_FR/LC_MESSAGES)
# ==============================================================================
