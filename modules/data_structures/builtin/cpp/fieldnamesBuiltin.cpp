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
#include "ClassdefParser.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "ErrorID.hpp"
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
        std::string className;
        if (arg1.isClassType()) {
            className = arg1.classType();
        } else if (arg1.isHandle()) {
            className = arg1.handleClassName();
        }
        if (!className.empty() && classdefLoadClass(className)) {
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
                classdefProperties(className));
            return retval;
        }
        if (arg1.isClassType()) {
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(arg1.fieldNames());
            return retval;
        }
        OverloadRequired("fieldnames");
    } else {
        if (arg1.isStruct()) {
            stringVector fieldnames = arg1.fieldNames();
            if (fieldnames.empty()) {
                Dimensions dim(0, 1);
                ArrayOf res = ArrayOf::empty(dim);
                res.promoteType(NLS_CELL_ARRAY);
                retval << res;
            } else {
                retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(fieldnames);
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"struct");
        }
    }
    return retval;
}
//=============================================================================
