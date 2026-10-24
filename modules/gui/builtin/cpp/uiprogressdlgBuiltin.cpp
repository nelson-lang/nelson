//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uiprogressdlgBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GuiDialogs.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GuiGateway {
//=============================================================================
ArrayOfVector
uiprogressdlgBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:gui:invalidFigureHandle",
            L"Invalid or deleted figure handle. First argument must be a valid figure handle.");
    }
    retval << UiProgressDialog(argIn);
    return retval;
}
//=============================================================================
} // namespace Nelson::GuiGateway
//=============================================================================
