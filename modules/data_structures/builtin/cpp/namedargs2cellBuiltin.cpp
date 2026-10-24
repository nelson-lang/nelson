//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "namedargs2cellBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::namedargs2cellBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (!param1.isStruct()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"struct");
    }
    if (!param1.isScalar()) {
        Error(L"Nelson:scalarExpected", L"Wrong size for argument #%d: scalar expected.", (1));
    }
    stringVector fieldnames = param1.fieldNames();
    ArrayOf* elements
        = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, fieldnames.size() * 2));
    Dimensions dims(1, fieldnames.size() * 2);
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    indexType k = 0;
    for (const std::string& name : fieldnames) {
        elements[k] = ArrayOf::characterArray(name);
        elements[k + 1] = param1.field(name);
        k = k + 2;
    }
    retval << res;
    return retval;
}
//=============================================================================
