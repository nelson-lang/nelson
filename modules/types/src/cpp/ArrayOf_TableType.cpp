//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define NLS_TABLE_TYPE "table"
#define NLS_TABLE_VERSION 1
//=============================================================================
bool
ArrayOf::isTable() const
{
    if (dp == nullptr) {
        return false;
    }
    return (dp->dataClass == NLS_CLASS_ARRAY || dp->classTypeName == "table");
}
//=============================================================================
ArrayOf
ArrayOf::tableConstructor(const ArrayOfVector& columnValues, const stringVector& variableNames,
    const stringVector& rowNames)
{
    Dimensions dims(1, 1);
    stringVector fieldnames = { "data", "Version", "Properties" };

    ArrayOf* table = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CLASS_ARRAY, 1, fieldnames);
    ArrayOf tableArrayOf = ArrayOf(NLS_CLASS_ARRAY, dims, table, false, fieldnames);
    tableArrayOf.setClassType("table");

    ArrayOf data;
    if (columnValues.empty()) {
        data = ArrayOf::emptyStructWithoutFields();
    } else {
        data = ArrayOf::structScalarConstructor(variableNames, columnValues);
    }
    tableArrayOf.setField(fieldnames[0], data);

    ArrayOf version = ArrayOf::doubleConstructor(NLS_TABLE_VERSION);
    tableArrayOf.setField(fieldnames[1], version);

    ArrayOf properties;
    stringVector propertyNames = { "VariableNames", "RowNames" };
    ArrayOfVector propertyValues;
    propertyValues.push_back(ArrayOf::toCellArrayOfCharacterRowVectors(variableNames));
    propertyValues.push_back(ArrayOf::toCellArrayOfCharacterRowVectors(rowNames));
    properties = ArrayOf::structScalarConstructor(propertyNames, propertyValues);

    tableArrayOf.setField(fieldnames[2], properties);

    return tableArrayOf;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
