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
#include <QtGui/QPalette>
#include <string>
#include "nlsGui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGUI_IMPEXP void
createNelsonPalette();
//=============================================================================
// User theme preference persisted in the settings: L"system" (follow the OS,
// default), L"light" or L"dark".
NLSGUI_IMPEXP std::wstring
getThemePreference();
//=============================================================================
// Persist the theme preference and apply it immediately.
NLSGUI_IMPEXP void
setThemePreference(const std::wstring& preference);
//=============================================================================
// Apply (or re-apply) the light/dark theme. Used at startup, on system color
// scheme changes and after a QStyle change (lookandfeel).
NLSGUI_IMPEXP void
applyNelsonTheme(bool dark);
//=============================================================================
NLSGUI_IMPEXP void
refreshNelsonTheme();
//=============================================================================
NLSGUI_IMPEXP bool
isDarkPalette();
//=============================================================================
NLSGUI_IMPEXP QPalette
getNelsonPalette();
//=============================================================================
} // namespace Nelson
//=============================================================================
