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
// Inputs for the nflow editor's bootstrap page (seeded into web storage before
// the real UI loads): the model to open, the UI root, the preferences path, the
// locale/theme, and the detected C/Rust toolchain availability.
struct BootstrapVars
{
    std::string modelFilename;
    // Identity of the model in NFlowModelRegistry when the window is bound to a
    // live document. Empty for a plain file open (legacy path):
    // the editor then reads/writes the file directly instead of the registry.
    std::string modelName;
    std::string uiPath;
    std::string nflowPreferencesPath;
    std::string locale = "en_US";
    bool darkMode = false;
    bool haveCToolChain = false;
    bool haveRustToolChain = false;
};
//=============================================================================
// Build the tiny bootstrap HTML document served to the WebView: it seeds the
// theme/locale/model/root/preferences and toolchain flags into session and local
// storage, then redirects to ./nflow.html (the real editor).
NLSNFLOW_GUI_IMPEXP std::string
buildNFlowBootstrapHtml(const BootstrapVars& vars);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
