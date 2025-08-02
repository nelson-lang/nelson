//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QObject_findchildrenBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "findchildrenQObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::QObject_findchildrenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring fieldname = param2.getContentAsWideString();
    bool bRecursively = false;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        logical l = param3.getContentAsLogicalScalar();
        bRecursively = (l == 1);
    }
    retval.push_back(findchildrenQObject(param1, fieldname, bRecursively));
    return retval;
}
//=============================================================================
