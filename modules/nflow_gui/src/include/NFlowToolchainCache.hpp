//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "nlsNflow_gui_exports.h"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Last known C/Rust toolchain availability. Persisted next to the nflow prefs so
// nflow() can seed the codegen UI instantly instead of running the expensive
// probe (a full CMake compiler-detection configure) on the launch path.
struct ToolchainCache
{
    bool haveC = false;
    bool haveRust = false;
};
//=============================================================================
// Read the cached toolchain availability (its own file, NOT nflow-ui-prefs.json
// which the JS UI owns, so the two sides never race on writes). Missing/corrupt
// cache yields {false, false}.
NLSNFLOW_GUI_IMPEXP ToolchainCache
loadToolchainCache(const std::string& preferencesDirectory);
//=============================================================================
NLSNFLOW_GUI_IMPEXP void
saveToolchainCache(const std::string& preferencesDirectory, bool haveC, bool haveRust);
//=============================================================================
// Build the JS snippet that writes the toolchain flags into session/local
// storage and dispatches a synthetic `storage` event, so the codegen panel's
// storage listener re-evaluates (a same-window setItem does not fire one).
NLSNFLOW_GUI_IMPEXP std::string
buildToolchainStoragePushJs(bool haveC, bool haveRust);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
