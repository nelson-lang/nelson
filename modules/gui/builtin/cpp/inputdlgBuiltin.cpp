//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "inputdlgBuiltin.hpp"
#include "GuiDialogs.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GuiGateway {
//=============================================================================
ArrayOfVector
inputdlgBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 5);
    retval << InputDialog(argIn);
    return retval;
}
//=============================================================================
} // namespace Nelson::GuiGateway
//=============================================================================
