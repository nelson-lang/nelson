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
#include <fmt/printf.h>
#include <fmt/format.h>
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
                std::wstring msg = fmt::sprintf(_W("Elapsed time is %f seconds."), r);
                eval->getInterface()->outputMessage(msg + L"\n");
            } else {
                retval << ArrayOf::doubleConstructor(r);
            }
            return retval;
        }
        Error(_W("Cannot call toc."));

    } else // argIn.size() == 0
    {
        if (eval->TimerValue == 0) {
            Error(_W("You must call \'tic\' without an output argument before calling \'toc\' "
                     "without an input argument."));
        }
        double r = 0;
        if (Toc(eval, r)) {
            ArrayOfVector retval;
            if (nLhs == 0) {
                std::wstring msg = fmt::sprintf(_W("Elapsed time is %f seconds."), r);
                eval->getInterface()->outputMessage(msg + L"\n");
            } else {
                retval << ArrayOf::doubleConstructor(r);
            }
            return retval;
        }
        Error(_W("You must call \'tic\' without an output argument before calling \'toc\' "
                 "without an input argument."));
    }
    // NEVER HERE
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
