//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fieldnamesBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::fieldnamesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf arg1 = argIn[0];
    if (arg1.isClassType() || arg1.isHandle()) {
        OverloadRequired("fieldnames");
    } else {
        if (arg1.isStruct()) {
            stringVector fieldnames = arg1.getFieldNames();
            if (fieldnames.empty()) {
                Dimensions dim(0, 1);
                ArrayOf res = ArrayOf::emptyConstructor(dim);
                res.promoteType(NLS_CELL_ARRAY);
                retval << res;
            } else {
                retval << ArrayOf::toCellArrayOfCharacterColumnVectors(fieldnames);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
