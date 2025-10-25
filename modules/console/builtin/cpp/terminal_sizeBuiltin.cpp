//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "terminal_sizeBuiltin.hpp"
#include "Interface.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConsoleGateway::terminal_sizeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector res;
    if (eval) {
        Interface* io = eval->getInterface();
        if (io) {
            if (nLhs < 2) {
                ArrayOf v = ArrayOf::doubleRowVectorConstructor(2);
                double* ptrDouble = (double*)v.getDataPointer();
                ptrDouble[0] = (double)io->getTerminalHeight();
                ptrDouble[1] = (double)io->getTerminalWidth();
                res << v;
            } else {
                res << ArrayOf::doubleConstructor((double)io->getTerminalHeight());
                res << ArrayOf::doubleConstructor((double)io->getTerminalWidth());
            }
        }
    }
    return res;
}
//=============================================================================
