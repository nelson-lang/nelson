//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "inserthtmlBuiltin.hpp"
#include "Error.hpp"
#include "GuiTerminal.hpp"
#include "Interface.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::inserthtmlBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];

    if (!(param1.isRowVectorCharacterArray() || param1.isScalarStringArray())) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    if (eval) {
        Interface* io = eval->getInterface();
        if (io) {
            std::wstring msg = param1.getContentAsWideString();
            auto* gtio = (GuiTerminal*)io;
            gtio->insertHtml(msg);
        }
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
