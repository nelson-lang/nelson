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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "nlsGui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGUI_IMPEXP ArrayOf
InputDialog(const ArrayOfVector& argIn);
//=============================================================================
NLSGUI_IMPEXP ArrayOfVector
ListDialog(const ArrayOfVector& argIn);
//=============================================================================
NLSGUI_IMPEXP ArrayOfVector
UiGetFile(const ArrayOfVector& argIn, bool saveMode);
//=============================================================================
NLSGUI_IMPEXP ArrayOf
UiSetColor(const ArrayOfVector& argIn);
//=============================================================================
NLSGUI_IMPEXP ArrayOf
UiSetFont(const ArrayOfVector& argIn);
//=============================================================================
NLSGUI_IMPEXP ArrayOf
UiAlert(const ArrayOfVector& argIn);
//=============================================================================
NLSGUI_IMPEXP ArrayOf
UiConfirm(const ArrayOfVector& argIn);
//=============================================================================
struct UiPreferenceDialogOptions
{
    std::wstring title;
    std::wstring question;
    wstringVector buttonLabels;
    wstringVector buttonValues;
    wstringVector extraButtons;
    std::wstring defaultButton;
    std::wstring checkboxText;
    bool checkboxState = false;
    std::wstring helpString;
    std::wstring helpFcn;
};
//=============================================================================
struct UiPreferenceDialogResult
{
    std::wstring value;
    bool dialogOpened = false;
    bool checkboxChecked = false;
    bool preferenceButton = false;
};
//=============================================================================
NLSGUI_IMPEXP UiPreferenceDialogResult
UiPreferenceDialog(const UiPreferenceDialogOptions& options);
//=============================================================================
NLSGUI_IMPEXP void
ShowPreferencesDialog(const std::wstring& page);
//=============================================================================
NLSGUI_IMPEXP ArrayOf
UiProgressDialog(const ArrayOfVector& argIn);
//=============================================================================
NLSGUI_IMPEXP void
UiOpen(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
NLSGUI_IMPEXP void
UiSave(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
