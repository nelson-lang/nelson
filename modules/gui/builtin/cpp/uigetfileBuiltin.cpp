//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uigetfileBuiltin.hpp"
#include "GuiDialogs.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GuiGateway {
//=============================================================================
ArrayOfVector
uigetfileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 3);
    nargincheck(argIn, 0);
    ArrayOfVector retval = UiGetFile(argIn, false);
    while ((int)retval.size() < nLhs) {
        retval << ArrayOf::empty();
    }
    return retval;
}
//=============================================================================
} // namespace Nelson::GuiGateway
//=============================================================================
