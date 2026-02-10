//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "tocBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "TicToc.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::tocBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        ArrayOf paramOne = argIn[0];
        uint64 t = paramOne.getContentAsUnsignedInteger64Scalar();
        double r = 0;
        if (Toc(t, r)) {
            ArrayOfVector retval(1);
            if (nLhs == 0) {
                std::wstring msg = fmt::format(_W("Elapsed time is {0:.6f} seconds."), r);
                eval->getInterface()->outputMessage(msg + L"\n");
            } else {
                retval << ArrayOf::doubleConstructor(r);
            }
            return retval;
        }
        raiseError(L"Nelson:time:ERROR_CANNOT_CALL_TOC", ERROR_CANNOT_CALL_TOC);

    } else // argIn.size() == 0
    {
        if (eval->TimerValue == 0) {
            raiseError(L"Nelson:time:ERROR_TIC_TOC_REQUIREMENT", ERROR_TIC_TOC_REQUIREMENT);
        }
        double r = 0;
        if (Toc(eval, r)) {
            ArrayOfVector retval;
            if (nLhs == 0) {
                std::wstring msg = fmt::format(_W("Elapsed time is {0:.6f} seconds."), r) + L"\n";
                eval->getInterface()->outputMessage(msg);
            } else {
                retval << ArrayOf::doubleConstructor(r);
            }
            return retval;
        }
        raiseError(L"Nelson:time:ERROR_TIC_TOC_REQUIREMENT", ERROR_TIC_TOC_REQUIREMENT);
    }
    // NEVER HERE
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
