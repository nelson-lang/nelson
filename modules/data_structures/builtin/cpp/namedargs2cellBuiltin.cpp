//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "namedargs2cellBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "PredefinedErrorMessages.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::namedargs2cellBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "namedargs2cell", bSuccess);
    }
    if (!bSuccess) {
        nargincheck(argIn, 1, 1);
        ArrayOf param1 = argIn[0];
        if (!param1.isStruct()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED);
        }
        if (!param1.isScalar()) {
            Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_EXPECTED);
        }
        stringVector fieldnames = param1.getFieldNames();
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, fieldnames.size() * 2);
        Dimensions dims(1, fieldnames.size() * 2);
        ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        indexType k = 0;
        for (const std::string& name : fieldnames) {
            elements[k] = ArrayOf::characterArrayConstructor(name);
            elements[k + 1] = param1.getField(name);
            k = k + 2;
        }
        retval << res;
    }
    return retval;
}
//=============================================================================
