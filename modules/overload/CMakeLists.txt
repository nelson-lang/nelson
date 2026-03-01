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
nelson_install_module_data(
  ${module_name}
  EXTRA_DIRS
  examples
  EXTRA_DIR_PATTERNS
  "tests/overload:*.m")
# ==============================================================================
