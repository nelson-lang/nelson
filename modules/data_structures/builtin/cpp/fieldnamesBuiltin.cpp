//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fieldnamesBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "ToCellString.hpp"
#include "characters_encoding.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::fieldnamesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "fieldnames", bSuccess);
    }
    if (!bSuccess) {
        nargincheck(argIn, 1, 1);
        ArrayOf arg1 = argIn[0];
        if (arg1.isClassStruct() || arg1.isHandle()) {
            retval = OverloadFunction(eval, nLhs, argIn, "fieldnames", bSuccess);
            if (bSuccess) {
                return retval;
            }
            Error(utf8_to_wstring(arg1.getStructType()) + L"_fieldnames " + _W("not defined."));
        } else {
            if (arg1.isStruct()) {
                stringVector fieldnames = arg1.getFieldNames();
                if (fieldnames.empty()) {
                    Dimensions dim(0, 1);
                    ArrayOf res = ArrayOf::emptyConstructor(dim);
                    res.promoteType(NLS_CELL_ARRAY);
                    retval << res;
                } else {
                    retval << ToCellStringAsColumn(fieldnames);
                }
            } else {
                retval = OverloadFunction(eval, nLhs, argIn, "fieldnames", bSuccess);
                if (bSuccess) {
                    return retval;
                }
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED);
            }
        }
    }
    return retval;
}
//=============================================================================
