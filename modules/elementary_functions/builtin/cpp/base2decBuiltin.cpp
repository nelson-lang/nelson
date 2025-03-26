//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "base2decBuiltin.hpp"
#include "Error.hpp"
#include "BaseToDecimal.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::bin2decBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isSparse() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired("bin2dec");
    }
    bool needToOverload;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = ArrayOf::doubleConstructor(2.);
    ArrayOf res = BaseToDecimal(param1, param2, needToOverload);
    if (needToOverload) {
        OverloadRequired("bin2dec");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::hex2decBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isSparse() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired("hex2dec");
    }
    bool needToOverload;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = ArrayOf::doubleConstructor(16.);
    ArrayOf res = BaseToDecimal(param1, param2, needToOverload);
    if (needToOverload) {
        OverloadRequired("hex2dec");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::base2decBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isSparse() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired("base2dec");
    }
    bool needToOverload;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf res = BaseToDecimal(param1, param2, needToOverload);
    if (needToOverload) {
        OverloadRequired("base2dec");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
