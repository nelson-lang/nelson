//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "COM_invokeBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "invokeComHandleObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_invokeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param2 = argIn[1];
    std::wstring methodname = param2.getContentAsWideString();
    ArrayOfVector params;
    for (size_t k = 2; k < argIn.size(); k++) {
        params << argIn[k];
    }
    ArrayOfVector retval;
    bool haveFunctionReturn = false;
    ArrayOf res = invokeComHandleObject(argIn[0], methodname, params, haveFunctionReturn);
    if (haveFunctionReturn) {
        retval << res;
    }
    return retval;
}
//=============================================================================
