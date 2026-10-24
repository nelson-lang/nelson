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
#include "nlsGui_exports.h"
//=============================================================================
namespace Nelson {
NLSGUI_IMPEXP bool
configureDefaultFont();
//=============================================================================
// Re-read the "Font/Name" and "Font/Size" user settings (after the preferences
// dialog changed them) so the next getDefaultFontName()/Size() reflect them.
NLSGUI_IMPEXP void
reloadDefaultFontSettings();
//=============================================================================
NLSGUI_IMPEXP std::wstring
getDefaultFontName();
//=============================================================================
// Default point size, overridable by the "Font/Size" user setting.
NLSGUI_IMPEXP int
getDefaultFontSize();
//=============================================================================
} // namespace Nelson
//=============================================================================
