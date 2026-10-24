# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name overload)
set(module_library_name nlsOverload)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
# Note: overload has no compiled src or builtin libraries (all commented out).
# Only data files are installed.
# ==============================================================================
# overload is header-only, so there is no target to carry PUBLIC_HEADER; install
# its public headers directly so they join the Nelson C++ SDK include set
# (dlgetnelsonincludes) like every other SDK module.
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/include/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${module_name}
  FILES_MATCHING
    PATTERN "*.hpp"
    PATTERN "*.h"
)
# ==============================================================================
nelson_install_module_data(
  ${module_name}
  EXTRA_DIRS
  examples
  EXTRA_DIR_PATTERNS
  "tests/overload:*.m"
)
