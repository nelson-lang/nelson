//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "preferencesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#ifdef WITH_NELSON_GUI_PREFS
#include "GuiDialogs.hpp"
#endif
//=============================================================================
namespace Nelson::PreferencesGateway {
//=============================================================================
ArrayOfVector
preferencesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 0, 1);
    std::wstring page;
    if (!argIn.empty()) {
        page = argIn[0].asWideString();
    }
#ifdef WITH_NELSON_GUI_PREFS
    ShowPreferencesDialog(page);
#else
    Error(L"Nelson:preferences:dialogNotAvailable",
        L"Preferences dialog is not available in this build.");
#endif
    return {};
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
