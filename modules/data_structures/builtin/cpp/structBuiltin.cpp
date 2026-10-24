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
#include "ClassdefSerialization.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IsValidFieldname.hpp"
#include "NewWithException.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::structBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    if (argIn.empty()) {
        retval << ArrayOf::emptyStruct();
        return retval;
    }
    if (argIn.size() == 1) {
        if (argIn[0].isHandle()) {
            OverloadRequired("struct");
        }
        if (argIn[0].isFunctionHandle()) {
            Error("Nelson:invalidConversion",
                "Conversion to 'struct' to 'function_handle' is not possible.");
        }
        if (argIn[0].isClassType()) {
            ArrayOf asStruct;
            if (!classdefToStruct(argIn[0], asStruct)) {
                asStruct = argIn[0];
                asStruct.ensureSingleOwner();
                asStruct.promoteType(NLS_STRUCT_ARRAY);
            }
            retval << asStruct;
            return retval;
        } else if (!argIn[0].isEmpty()) {
            Error(L"Nelson:data_structures:structExpected", L"struct([]) expected.");
        }
        Dimensions dim = argIn[0].dimensions();
        wstringVector fieldnames;
        retval << ArrayOf::emptyStruct(fieldnames, dim);
    } else {
        if (argIn.size() % 2) {
            Error(L"Nelson:data_structures:requiresPairsFieldNamesValues",
                L"requires pairs of field names and values.");
        }
        size_t pairCount = argIn.size() / 2;
        stringVector names;
        ArrayOfVector values;
        for (size_t i = 0; i < pairCount; i++) {
            values.push_back(ArrayOf());
        }
        for (size_t i = 0; i < pairCount * 2; i += 2) {
            if (!(argIn[i].isRowVectorCharacterArray() || argIn[i].isScalarStringArray())) {
                Error(L"Nelson:data_structures:requiresPairsFieldNamesValues",
                    L"requires pairs of field names and values.");
            }
            std::string field = argIn[i].asUtf8String();
            if (!IsValidFieldname(field)) {
                Error(L"Nelson:data_structures:requiresValidFieldname",
                    L"requires a valid fieldname.");
            }
            names.push_back(field);
            values[i / 2] = argIn[i + 1];
            if (argIn[i + 1].isStringArray()) {
                // a string array is one value, not a cell of values
                auto* element = new_with_exception<ArrayOf>(1, false);
                element[0] = argIn[i + 1];
                values[i / 2] = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 1), element);
            }
        }
        retval << ArrayOf::structArray(names, values);
    }
    return retval;
}
//=============================================================================
