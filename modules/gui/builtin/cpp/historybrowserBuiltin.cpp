//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "historybrowserBuiltin.hpp"
#include "GuiDialogProvider.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::WebviewGateway {
//=============================================================================
ArrayOfVector
historybrowserBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 0);
    // Open (or focus) the docked panel in the desktop.
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"showpanel";
        request.defaultPath = L"history";
        GuiDialogResponse response;
        runGuiDialog(request, response);
    }
    return {};
}
//=============================================================================
} // namespace Nelson::WebviewGateway
//=============================================================================
