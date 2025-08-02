//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "num2strBuiltin.hpp"
#include "Error.hpp"
#include "OverloadRequired.hpp"
#include "NumberToString.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::num2strBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    ArrayOf res;
    bool needToOverload;
    if (argIn.size() == 2) {
        ArrayOf arg1 = argIn[1];
        if (argIn[1].isNumeric()) {
            double N = arg1.getContentAsDoubleScalar();
            res = NumberToString(argIn[0], N, needToOverload);
        } else {
            std::wstring format = arg1.getContentAsWideString();
            res = NumberToString(argIn[0], format, needToOverload);
        }
    } else {
        res = NumberToString(argIn[0], needToOverload);
    }
    if (needToOverload) {
        OverloadRequired("num2str");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
