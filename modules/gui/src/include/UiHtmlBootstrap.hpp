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
//=============================================================================
namespace Nelson {
//=============================================================================
// Path the bootstrap is served from. The entry document gets a <script> tag
// pointing here, plus one pointing at "/webui.js": WebUI never injects its own
// bridge on its own, `globalThis.webui` only exists when the served document
// asks for it.
extern const char* UIHTML_BOOTSTRAP_PATH;
//=============================================================================
// The script that builds `htmlComponent` and connects it to the host.
const std::string&
UiHtmlBootstrapScript();
//=============================================================================
// Insert the two <script> tags into a user document: before </head> when there
// is one, else just after <body ...>, else at the very top.
std::string
UiHtmlInjectBootstrap(const std::string& document);
//=============================================================================
} // namespace Nelson
//=============================================================================
