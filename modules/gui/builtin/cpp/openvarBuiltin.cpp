//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "openvarBuiltin.hpp"
#include "GuiDialogProvider.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::WebviewGateway {
//=============================================================================
ArrayOfVector
openvarBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    const std::wstring variableName = argIn[0].asWideString();
    // Open the variable editor of the desktop on this variable.
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"openvariable";
        request.defaultPath = variableName;
        GuiDialogResponse response;
        runGuiDialog(request, response);
    }
    return {};
}
//=============================================================================
} // namespace Nelson::WebviewGateway
//=============================================================================
