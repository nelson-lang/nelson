//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "structBuiltin.hpp"
#include "Error.hpp"
#include "IsValidFieldname.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::structBuiltin(Evaluator * eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    if (argIn.empty()) {
        retval << ArrayOf::emptyStructWithoutFields();
        return retval;
    }
    if (argIn.size() == 1) {
        if (argIn[0].isHandle()) {
            bool bSuccess = false;
            retval = OverloadFunction(eval, nLhs, argIn, "struct", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        if (argIn[0].isClassStruct()) {
            if (argIn[0].isFunctionHandle()) {
                Error(_("Conversion to 'struct' to 'function_handle' is not possible."),
                    "Nelson:invalidConversion");
            } else {
                ArrayOf asStruct = argIn[0];
                asStruct.ensureSingleOwner();
                asStruct.setStructType(NLS_STRUCT_ARRAY_STR);
                retval << asStruct;
                return retval;
            }
        } else if (!argIn[0].isEmpty()) {
            Error(_W("struct([]) expected."));
        }
        Dimensions dim = argIn[0].getDimensions();
        wstringVector fieldnames;
        retval << ArrayOf::emptyStructConstructor(fieldnames, dim);
    } else {
        if (argIn.size() % 2) {
            Error(_W("requires pairs of field names and values."));
        }
        size_t pairCount = argIn.size() / 2;
        stringVector names;
        ArrayOfVector values;
        for (size_t i = 0; i < pairCount; i++) {
            values.push_back(ArrayOf());
        }
        for (size_t i = 0; i < pairCount * 2; i += 2) {
            if (!(argIn[i].isRowVectorCharacterArray())) {
                Error(_W("requires pairs of field names and values."));
            }
            std::string field = argIn[i].getContentAsCString();
            if (!IsValidFieldname(field)) {
                Error(_W("requires a valid fieldname."));
            }
            names.push_back(field);
            values[i / 2] = argIn[i + 1];
        }
        retval << ArrayOf::structConstructor(names, values);
    }
    return retval;
}
//=============================================================================
