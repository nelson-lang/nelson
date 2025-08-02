//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "qt_verboseBuiltin.hpp"
#include "Error.hpp"
#include "MainGuiObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::qt_verboseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    bool previous = IsQtMessageVerbose();
    ArrayOfVector retval;
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        logical newVerbose = param1.getContentAsLogicalScalar();
        QtMessageVerbose((newVerbose == 1));
    }
    retval << ArrayOf::logicalConstructor(previous);
    return retval;
}
//=============================================================================
