//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "COM_getBuiltin.hpp"
#include "GetComHandleObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOfVector params(argIn);
    params.pop_front();
    params.pop_front();
    std::wstring propertyName = param2.getContentAsWideString();
    ArrayOfVector retval(1);
    retval << GetComHandleObject(param1, propertyName, params);
    return retval;
}
//=============================================================================
