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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Interface.hpp"
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
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (eval) {
        Interface* io = eval->getInterface();
        if (io) {
            // Through the Interface: casting to the Qt terminal was undefined
            // behaviour under any other front end (web desktop, adv-cli).
            if (!io->insertHtml(param1.asWideString())) {
                Error(L"Nelson:inserthtml:notSupportedTerminal",
                    L"HTML display is not supported by this terminal.");
            }
        }
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
