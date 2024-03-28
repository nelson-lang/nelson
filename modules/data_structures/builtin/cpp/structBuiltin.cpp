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
#include "i18n.hpp"
#include "IsValidFieldname.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::structBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    if (argIn.empty()) {
        retval << ArrayOf::emptyStructWithoutFields();
        return retval;
    }
    if (argIn.size() == 1) {
        if (argIn[0].isHandle()) {
            OverloadRequired("struct");
        }
        if (argIn[0].isFunctionHandle()) {
            Error(_("Conversion to 'struct' to 'function_handle' is not possible."),
                "Nelson:invalidConversion");
        }
        if (argIn[0].isClassType()) {
            ArrayOf asStruct = argIn[0];
            asStruct.ensureSingleOwner();
            asStruct.promoteType(NLS_STRUCT_ARRAY);
            retval << asStruct;
            return retval;
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
            if (!(argIn[i].isRowVectorCharacterArray() || argIn[i].isScalarStringArray())) {
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
