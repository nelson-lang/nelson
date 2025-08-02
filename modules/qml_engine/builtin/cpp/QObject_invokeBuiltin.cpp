//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QObject_invokeBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "invokeQObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::QObject_invokeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param2 = argIn[1];
    std::wstring methodname = param2.getContentAsWideString();
    ArrayOfVector params;
    for (size_t k = 2; k < argIn.size(); k++) {
        params.push_back(argIn[k]);
    }
    ArrayOfVector retval;
    bool haveFunctionReturn = false;
    ArrayOf res = invokeQObject(argIn[0], methodname, params, haveFunctionReturn);
    if (haveFunctionReturn) {
        retval.push_back(res);
    }
    return retval;
}
//=============================================================================
