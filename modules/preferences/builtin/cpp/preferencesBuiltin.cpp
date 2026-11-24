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
#include "InputOutputArgumentsCheckers.hpp"
#include "GuiDialogProvider.hpp"
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
    // Ask the desktop front end to open its preferences panel (non-blocking).
    // Without a front end (headless), there is nothing to open.
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"showpreferences";
        request.defaultPath = page;
        GuiDialogResponse response;
        runGuiDialog(request, response);
    }
    return {};
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
