//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "betaincBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "BetaIncomplete.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::betaincBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn[0].isSparse() || argIn[0].getDataClass() == NLS_DCOMPLEX
        || argIn[0].getDataClass() == NLS_SCOMPLEX) {
        Error(_W("Input argument must be dense and real."), L"Nelson:betainc:notFullReal");
    }
    bool isLower = true;
    if (argIn.size() == 4) {
        std::wstring tail = argIn[3].getContentAsWideString();
        if (tail.compare(L"upper") == 0 || tail.compare(L"lower") == 0) {
            if (tail.compare(L"upper") == 0) {
                isLower = false;
            } else {
                isLower = true;
            }
        } else {
            Error(_("Wrong value of the fourth argument 'upper' or 'lower' expected."));
        }
    }
    retval << BetaIncomplete(argIn[0], argIn[1], argIn[2], isLower);
    return retval;
}
//=============================================================================
