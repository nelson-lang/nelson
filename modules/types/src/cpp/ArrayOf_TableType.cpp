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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ClassName.hpp"
#include <algorithm>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
#define NLS_TABLE_TYPE "table"
#define NLS_TABLE_VERSION 2
//=============================================================================
static indexType
tableColumnHeight(const ArrayOf& column);
//=============================================================================
static bool
containsStringValue(const stringVector& values, const std::string& value);
//=============================================================================
class TableLookup
{
public:
    explicit TableLookup(const ArrayOf& table);

    [[nodiscard]] const ArrayOf&
    data() const;

    [[nodiscard]] const ArrayOf&
    properties() const;

    [[nodiscard]] const stringVector&
    variableNames() const;

    [[nodiscard]] const stringVector&
    variableTypes() const;

    [[nodiscard]] const stringVector&
    rowNames() const;

    [[nodiscard]] const stringVector&
    columnNames() const;

    [[nodiscard]] bool
    hasColumn(const std::string& columnName) const;

    [[nodiscard]] ArrayOf
    column(const std::string& columnName) const;

    [[nodiscard]] ArrayOf
    column(indexType columnIndex) const;

private:
    ArrayOf data_;
    ArrayOf properties_;
    stringVector variableNames_;
    stringVector variableTypes_;
    stringVector rowNames_;
    stringVector columnNames_;
    std::unordered_map<std::string, indexType> columnIndexes_;
};
//=============================================================================
bool
ArrayOf::isTable() const
{
    if (dp == nullptr) {
        return false;
    }
    return dp->dataClass == NLS_CLASSDEF_ARRAY && dp->classTypeName_ == "table";
}
//=============================================================================
bool
ArrayOf::isTimetable() const
{
    if (dp == nullptr) {
        return false;
    }
    return dp->dataClass == NLS_CLASSDEF_ARRAY && dp->classTypeName_ == "timetable";
}
//=============================================================================
ArrayOf
ArrayOf::table(const ArrayOfVector& columnValues, const stringVector& variableNames,
    const stringVector& rowNames)
{
    Dimensions dims(1, 1);
    stringVector fieldnames = { "data", "Version", "VariableCustomProperties", "Properties" };

    ArrayOf* table = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, 1, fieldnames);
    ArrayOf tableArrayOf = ArrayOf(NLS_CLASSDEF_ARRAY, dims, table, false, fieldnames);
    tableArrayOf.setClassType("table");

    ArrayOf data;
    if (columnValues.empty()) {
        data = ArrayOf::emptyStruct();
    } else {
        data = ArrayOf::structScalar(variableNames, columnValues);
    }
    tableArrayOf.setField(fieldnames[0], data);

    ArrayOf version = ArrayOf::doubleScalar(NLS_TABLE_VERSION);
    tableArrayOf.setField(fieldnames[1], version);

    ArrayOf variableCustomProperties = ArrayOf::emptyCell(Dimensions(0, 0));
    tableArrayOf.setField(fieldnames[2], variableCustomProperties);

    stringVector variableTypes = ClassName(columnValues);
    Dimensions dimsVariableTypes(1, variableTypes.size());

    ArrayOf properties;
    stringVector propertyNames = { "VariableNames", "VariableTypes", "RowNames" };
    ArrayOfVector propertyValues;
    propertyValues.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(variableNames));
    propertyValues.push_back(ArrayOf::stringArray(variableTypes, dimsVariableTypes));
    propertyValues.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(rowNames));
    properties = ArrayOf::structScalar(propertyNames, propertyValues);

    tableArrayOf.setField(fieldnames[3], properties);

    return tableArrayOf;
}
//=============================================================================
ArrayOf
ArrayOf::timetable(const ArrayOf& tableValue, const ArrayOf& rowTimes)
{
    stringVector propertyNames = { "DimensionNames", "Description", "UserData", "RowTimes",
        "StartTime", "SampleRate", "TimeStep", "Events", "VariableNames", "VariableTypes",
        "VariableDescriptions", "VariableUnits", "VariableContinuity", "CustomProperties" };

    ArrayOf tableProperties = tableValue.tableProperties();
    ArrayOf variableNames = tableProperties.field("VariableNames");
    ArrayOf variableTypes = tableProperties.field("VariableTypes");

    ArrayOfVector propertyValues;
    propertyValues.push_back(
        ArrayOf::cellArrayFromArrayOfCharacterRowVectors(stringVector { "Time", "Variables" }));
    propertyValues.push_back(ArrayOf::characterArray(""));
    propertyValues.push_back(ArrayOf::empty());
    propertyValues.push_back(rowTimes);
    propertyValues.push_back(ArrayOf::empty());
    propertyValues.push_back(ArrayOf::empty());
    propertyValues.push_back(ArrayOf::empty());
    propertyValues.push_back(ArrayOf::empty());
    propertyValues.push_back(variableNames);
    propertyValues.push_back(variableTypes);
    propertyValues.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(stringVector {}));
    propertyValues.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(stringVector {}));
    propertyValues.push_back(ArrayOf::empty());
    propertyValues.push_back(ArrayOf::emptyStruct());

    ArrayOf properties = ArrayOf::structScalar(propertyNames, propertyValues);
    stringVector fieldNames = { "Table", "Version", "Properties" };
    ArrayOfVector fieldValues;
    fieldValues.push_back(tableValue);
    fieldValues.push_back(ArrayOf::doubleScalar(1));
    fieldValues.push_back(properties);
    return ArrayOf::classdefArray("timetable", fieldNames, fieldValues);
}
//=============================================================================
ArrayOf
ArrayOf::tableData() const
{
    return field("data");
}
//=============================================================================
ArrayOf
ArrayOf::tableProperties() const
{
    return field("Properties");
}
//=============================================================================
void
ArrayOf::setTableData(const ArrayOf& data)
{
    ArrayOf copyData = data;
    setField("data", copyData);
}
//=============================================================================
void
ArrayOf::setTableProperties(const ArrayOf& properties)
{
    ArrayOf copyProperties = properties;
    setField("Properties", copyProperties);
}
//=============================================================================
stringVector
ArrayOf::tableVariableNames() const
{
    TableLookup lookup(*this);
    return lookup.variableNames();
}
//=============================================================================
stringVector
ArrayOf::tableVariableTypes() const
{
    TableLookup lookup(*this);
    return lookup.variableTypes();
}
//=============================================================================
stringVector
ArrayOf::tableRowNames() const
{
    TableLookup lookup(*this);
    return lookup.rowNames();
}
//=============================================================================
indexType
ArrayOf::tableWidth() const
{
    TableLookup lookup(*this);
    const stringVector& names = lookup.columnNames();
    return names.empty() ? lookup.variableNames().size() : names.size();
}
//=============================================================================
indexType
ArrayOf::tableHeight() const
{
    TableLookup lookup(*this);
    const ArrayOf& data = lookup.data();
    if (lookup.columnNames().empty()) {
        // A table without variables keeps its height in its field-less data
        // struct array (n-by-0); the scalar struct() means 0 rows.
        return data.isEmpty() ? data.rows() : 0;
    }
    if (data.isEmpty()) {
        return 0;
    }

    ArrayOf firstColumn = lookup.column(static_cast<indexType>(0));
    return tableColumnHeight(firstColumn);
}
//=============================================================================
bool
ArrayOf::isTableEmpty() const
{
    return tableHeight() == 0 || tableWidth() == 0;
}
//=============================================================================
Dimensions
ArrayOf::tableDimensions() const
{
    return Dimensions(tableHeight(), tableWidth());
}
//=============================================================================
ArrayOf
ArrayOf::tableColumn(const std::string& columnName) const
{
    TableLookup lookup(*this);
    return lookup.column(columnName);
}
//=============================================================================
ArrayOf
ArrayOf::tableColumn(indexType columnIndex) const
{
    TableLookup lookup(*this);
    return lookup.column(columnIndex);
}
//=============================================================================
void
ArrayOf::setTableColumn(const std::string& columnName, const ArrayOf& columnData)
{
    ArrayOf data = tableData();

    TableLookup lookup(*this);
    stringVector varNames = lookup.variableNames();
    bool columnExists = lookup.hasColumn(columnName) || containsStringValue(varNames, columnName);

    if (!columnExists) {
        varNames.push_back(columnName);

        stringVector varTypes = lookup.variableTypes();
        varTypes.push_back(ClassName(columnData));

        ArrayOf properties = lookup.properties();

        ArrayOf newVariableNames = ArrayOf::cellArrayFromArrayOfCharacterRowVectors(varNames);
        properties.setField("VariableNames", newVariableNames);

        Dimensions dimsVariableTypes(1, varTypes.size());
        ArrayOf newVariableTypes = ArrayOf::stringArray(varTypes, dimsVariableTypes);
        properties.setField("VariableTypes", newVariableTypes);

        setTableProperties(properties);
    }

    ArrayOf copyData = columnData;
    data.setField(columnName, copyData);
    setTableData(data);
}
//=============================================================================
void
ArrayOf::addTableColumn(const std::string& columnName, const ArrayOf& columnData)
{
    TableLookup lookup(*this);
    if (lookup.hasColumn(columnName) || containsStringValue(lookup.variableNames(), columnName)) {
        Error("Nelson:types:fieldAlreadyExists",
            "Column '%s' already exists. Use setTableColumn to modify.", columnName);
    }

    if (!isTableEmpty() && columnData.elementCount() != tableHeight()) {
        Error("Nelson:dimagree:columnDataSizeMatchTableHeight",
            "Column data size must match table height.");
    }

    setTableColumn(columnName, columnData);
}
//=============================================================================
indexType
tableColumnHeight(const ArrayOf& column)
{
    if (!column.isClassdefType() || column.elementCount() != 1) {
        return column.elementCount();
    }
    int64 serialField = column.fieldIndex("SerialValue");
    if (serialField >= 0) {
        return column.field("SerialValue").elementCount();
    }
    int64 secondsField = column.fieldIndex("SecondsValue");
    if (secondsField >= 0) {
        return column.field("SecondsValue").elementCount();
    }
    return column.elementCount();
}
//=============================================================================
static bool
containsStringValue(const stringVector& values, const std::string& value)
{
    return std::find(values.begin(), values.end(), value) != values.end();
}
//=============================================================================
TableLookup::TableLookup(const ArrayOf& table)
    : data_(table.tableData()), properties_(table.tableProperties())
{
    ArrayOf varNames = properties_.field("VariableNames");
    if (varNames.isCellArrayOfCharacterVectors() || varNames.isStringArray()) {
        variableNames_ = varNames.asUtf8StringVector();
    }

    ArrayOf varTypes = properties_.field("VariableTypes");
    if (varTypes.isStringArray()) {
        variableTypes_ = varTypes.asUtf8StringVector();
    }

    ArrayOf rowNames = properties_.field("RowNames");
    if (rowNames.isCellArrayOfCharacterVectors()) {
        rowNames_ = rowNames.asUtf8StringVector();
    }

    columnNames_ = data_.fieldNames();
    const stringVector& lookupNames = columnNames_.empty() ? variableNames_ : columnNames_;
    for (indexType i = 0; i < static_cast<indexType>(lookupNames.size()); i++) {
        columnIndexes_.emplace(lookupNames[i], i);
    }
}
//=============================================================================
const ArrayOf&
TableLookup::data() const
{
    return data_;
}
//=============================================================================
const ArrayOf&
TableLookup::properties() const
{
    return properties_;
}
//=============================================================================
const stringVector&
TableLookup::variableNames() const
{
    return variableNames_;
}
//=============================================================================
const stringVector&
TableLookup::variableTypes() const
{
    return variableTypes_;
}
//=============================================================================
const stringVector&
TableLookup::rowNames() const
{
    return rowNames_;
}
//=============================================================================
const stringVector&
TableLookup::columnNames() const
{
    return columnNames_;
}
//=============================================================================
bool
TableLookup::hasColumn(const std::string& columnName) const
{
    return columnIndexes_.find(columnName) != columnIndexes_.end();
}
//=============================================================================
ArrayOf
TableLookup::column(const std::string& columnName) const
{
    if (!hasColumn(columnName)) {
        Error("Nelson:types:fieldNotFound:columnValueNotFoundTable",
            "Column '%s' not found in table.", columnName);
    }
    int64 ndx = data_.fieldIndex(columnName);
    if (ndx < 0) {
        Error("Nelson:types:fieldNotFound:columnValueNotFoundTable",
            "Column '%s' not found in table.", columnName);
    }
    return data_.field(columnName);
}
//=============================================================================
ArrayOf
TableLookup::column(indexType columnIndex) const
{
    const stringVector& names = columnNames_.empty() ? variableNames_ : columnNames_;
    if (columnIndex >= names.size()) {
        Error("Nelson:badsubscript:columnIndexOutBounds", "Column index out of bounds.");
    }
    return column(names[columnIndex]);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
