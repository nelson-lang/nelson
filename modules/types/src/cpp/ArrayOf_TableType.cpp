//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "ClassName.hpp"
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

    stringVector variableTypes = ClassName(columnValues);
    Dimensions dimsVariableTypes(1, variableTypes.size());

    ArrayOf properties;
    stringVector propertyNames = { "VariableNames", "VariableTypes", "RowNames" };
    ArrayOfVector propertyValues;
    propertyValues.push_back(ArrayOf::toCellArrayOfCharacterRowVectors(variableNames));
    propertyValues.push_back(ArrayOf::stringArrayConstructor(variableTypes, dimsVariableTypes));
    propertyValues.push_back(ArrayOf::toCellArrayOfCharacterRowVectors(rowNames));
    properties = ArrayOf::structScalarConstructor(propertyNames, propertyValues);

    tableArrayOf.setField(fieldnames[2], properties);

    return tableArrayOf;
}
//=============================================================================
ArrayOf
ArrayOf::getTableData() const
{
    return getField("data");
}
//=============================================================================
ArrayOf
ArrayOf::getTableProperties() const
{
    return getField("Properties");
}
//=============================================================================
stringVector
ArrayOf::getTableVariableNames() const
{
    ArrayOf properties = getTableProperties();
    ArrayOf varNames = properties.getField("VariableNames");

    stringVector names;
    if (varNames.isCellArrayOfCharacterVectors() || varNames.isStringArray()) {
        names = varNames.getContentAsCStringVector();
    }
    return names;
}
//=============================================================================
stringVector
ArrayOf::getTableVariableTypes() const
{

    ArrayOf properties = getTableProperties();
    ArrayOf varTypes = properties.getField("VariableTypes");

    stringVector types;
    if (varTypes.isStringArray()) {
        types = varTypes.getContentAsCStringVector();
    }
    return types;
}
//=============================================================================
stringVector
ArrayOf::getTableRowNames() const
{
    ArrayOf properties = getTableProperties();
    ArrayOf rowNames = properties.getField("RowNames");

    stringVector names;
    if (rowNames.isCellArrayOfCharacterVectors()) {
        names = rowNames.getContentAsCStringVector();
    }
    return names;
}
//=============================================================================
indexType
ArrayOf::getTableWidth() const
{
    return getTableVariableNames().size();
}
//=============================================================================
indexType
ArrayOf::getTableHeight() const
{
    ArrayOf data = getTableData();
    if (data.isEmpty()) {
        return 0;
    }

    stringVector varNames = getTableVariableNames();
    if (varNames.empty()) {
        return 0;
    }

    // Get first column to determine row count
    ArrayOf firstColumn = data.getField(varNames[0]);
    return firstColumn.getElementCount();
}
//=============================================================================
bool
ArrayOf::isTableEmpty() const
{
    return getTableHeight() == 0 || getTableWidth() == 0;
}
//=============================================================================
Dimensions
ArrayOf::getTableDimensions() const
{
    return Dimensions(getTableHeight(), getTableWidth());
}
//=============================================================================
ArrayOf
ArrayOf::getTableColumn(const std::string& columnName) const
{
    ArrayOf data = getTableData();
    int64 ndx = data.getFieldIndex(columnName);

    if (ndx < 0) {
        Error(_("Column '") + columnName + _("' not found in table."));
    }

    return data.getField(columnName);
}
//=============================================================================
ArrayOf
ArrayOf::getTableColumn(indexType columnIndex) const
{
    stringVector varNames = getTableVariableNames();
    if (columnIndex >= varNames.size()) {
        Error(_("Column index out of bounds."));
    }

    return getTableColumn(varNames[columnIndex]);
}
//=============================================================================
void
ArrayOf::setTableColumn(const std::string& columnName, const ArrayOf& columnData)
{
    ArrayOf data = getTableData();

    // Check if column exists
    stringVector varNames = getTableVariableNames();
    bool columnExists = std::find(varNames.begin(), varNames.end(), columnName) != varNames.end();

    if (!columnExists) {
        // Add new column name to variable names
        varNames.push_back(columnName);

        // Update variable types
        stringVector varTypes = getTableVariableTypes();
        varTypes.push_back(ClassName(columnData));

        // Update properties
        ArrayOf properties = getTableProperties();

        ArrayOf newVariableNames = ArrayOf::toCellArrayOfCharacterRowVectors(varNames);
        properties.setField("VariableNames", newVariableNames);

        Dimensions dimsVariableTypes(1, varTypes.size());
        ArrayOf newVariableTypes = ArrayOf::stringArrayConstructor(varTypes, dimsVariableTypes);
        properties.setField("VariableTypes", newVariableTypes);

        setField("Properties", properties);
    }

    // Set the column data
    ArrayOf copyData = columnData;
    data.setField(columnName, copyData);
    setField("data", data);
}
//=============================================================================
void
ArrayOf::addTableColumn(const std::string& columnName, const ArrayOf& columnData)
{
    stringVector varNames = getTableVariableNames();
    if (std::find(varNames.begin(), varNames.end(), columnName) != varNames.end()) {
        Error(_("Column '") + columnName + _("' already exists. Use setTableColumn to modify."));
    }

    // Validate column data size matches table height
    if (!isTableEmpty() && columnData.getElementCount() != getTableHeight()) {
        Error(_("Column data size must match table height."));
    }

    setTableColumn(columnName, columnData);
}
//=============================================================================
//=============================================================================
} // namespace Nelson
//=============================================================================
