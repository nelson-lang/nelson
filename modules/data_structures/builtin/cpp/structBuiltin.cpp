//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "structBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
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
            raiseError(
                L"Nelson:data_structures:ERROR_CONVERSION_STRUCT_TO_FUNCTION_HANDLE_NOT_POSSIBLE",
                ERROR_CONVERSION_STRUCT_TO_FUNCTION_HANDLE_NOT_POSSIBLE);
        }
        if (argIn[0].isClassType()) {
            ArrayOf asStruct = argIn[0];
            asStruct.ensureSingleOwner();
            asStruct.promoteType(NLS_STRUCT_ARRAY);
            retval << asStruct;
            return retval;
        } else if (!argIn[0].isEmpty()) {
            raiseError(
                L"Nelson:data_structures:ERROR_STRUCT_EMPTY_EXPECTED", ERROR_STRUCT_EMPTY_EXPECTED);
        }
        Dimensions dim = argIn[0].getDimensions();
        wstringVector fieldnames;
        retval << ArrayOf::emptyStructConstructor(fieldnames, dim);
    } else {
        if (argIn.size() % 2) {
            raiseError(L"Nelson:data_structures:ERROR_REQUIRES_PAIRS_OF_FIELD_NAMES_AND_VALUES",
                ERROR_REQUIRES_PAIRS_OF_FIELD_NAMES_AND_VALUES);
        }
        size_t pairCount = argIn.size() / 2;
        stringVector names;
        ArrayOfVector values;
        for (size_t i = 0; i < pairCount; i++) {
            values.push_back(ArrayOf());
        }
        for (size_t i = 0; i < pairCount * 2; i += 2) {
            if (!(argIn[i].isRowVectorCharacterArray() || argIn[i].isScalarStringArray())) {
                raiseError(L"Nelson:data_structures:ERROR_REQUIRES_PAIRS_OF_FIELD_NAMES_AND_VALUES",
                    ERROR_REQUIRES_PAIRS_OF_FIELD_NAMES_AND_VALUES);
            }
            std::string field = argIn[i].getContentAsCString();
            if (!IsValidFieldname(field)) {
                raiseError(L"Nelson:data_structures:ERROR_REQUIRES_A_VALID_FIELDNAME",
                    ERROR_REQUIRES_A_VALID_FIELDNAME);
            }
            names.push_back(field);
            values[i / 2] = argIn[i + 1];
        }
        retval << ArrayOf::structConstructor(names, values);
    }
    return retval;
}
//=============================================================================
