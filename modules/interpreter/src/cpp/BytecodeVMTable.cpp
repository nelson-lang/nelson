//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// table and timetable indexing fast paths. Each
// one gives the result of the table or timetable subsref/subsasgn without
// calling it, and declines (returns false) whenever that result is not
// certain: the overloaded method then runs, errors included.
//=============================================================================
#include <algorithm>
#include <string>
#include "BytecodeVMInternal.hpp"
#include "ClassName.hpp"
#include "Evaluator.hpp"
#include "NelsonException.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
// The Properties fields of a table refreshed by its methods, in the order of
// table defaultProperties (table.m). A timetable keeps its variables in a
// table storage struct with the same layout.
static const stringVector REFRESHED_TABLE_PROPERTY_NAMES = { "DimensionNames", "Description",
    "UserData", "VariableNames", "VariableTypes", "VariableDescriptions", "VariableUnits",
    "VariableContinuity", "RowNames", "CustomProperties" };
//=============================================================================
struct RefreshedTableStorage
{
    ArrayOf data;
    ArrayOf properties;
    stringVector names;
    stringVector types;
    indexType rows = 0;
};
//=============================================================================
static ArrayOf
sliceTableColumnRows(ArrayOf column, ArrayOf rowIndex);
//=============================================================================
static bool
tableVariableIndices(ArrayOf index, const stringVector& names, std::vector<indexType>& vars);
//=============================================================================
static bool
readTimetableVariable(
    Evaluator* eval, const ArrayOf& base, const std::string& field, ArrayOf& result);
//=============================================================================
static bool
isFastTableObject(const ArrayOf& base);
//=============================================================================
static bool
isFastTimetableObject(const ArrayOf& base);
//=============================================================================
static bool
isOverloadedTableAccess(Evaluator* eval, const ArrayOf& base);
//=============================================================================
static bool
isPlainValue(const ArrayOf& value);
//=============================================================================
static bool
readNames(const ArrayOf& value, stringVector& names);
//=============================================================================
static bool
contains(const stringVector& names, const std::string& name);
//=============================================================================
static bool
readRefreshedTableStorage(const ArrayOf& storage, RefreshedTableStorage& view);
//=============================================================================
static bool
isTimetableVariableTarget(
    const ArrayOf& properties, const RefreshedTableStorage& view, const std::string& field);
//=============================================================================
static bool
tryFastTablePropertiesAssign(ArrayOf& base, const ArrayOf& value);
//=============================================================================
static bool
isSameStoredValue(const ArrayOf& a, const ArrayOf& b);
//=============================================================================
static bool
isTimetableRowTimeSubscript(const ArrayOf& index);
//=============================================================================
bool
tryFastTableParenSubset(const ArrayOf& base, const ArrayOfVector& indices, ArrayOf& result)
{
    if (!isFastTableObject(base) || indices.size() != 2 || !base.tableRowNames().empty()) {
        return false;
    }
    ArrayOf rowIndex = indices[0];
    if (!isColonIndex(rowIndex)
        && (!rowIndex.isNumeric() || rowIndex.isComplex() || rowIndex.isSparse())) {
        return false;
    }
    stringVector names = base.tableVariableNames();
    std::vector<indexType> vars;
    // A zero-width result keeps its row count: left to the table class.
    if (!tableVariableIndices(indices[1], names, vars) || vars.empty()) {
        return false;
    }
    // Repeated variables are renamed and per-variable metadata is indexed by
    // the table class.
    std::vector<indexType> sortedVars(vars);
    std::sort(sortedVars.begin(), sortedVars.end());
    if (std::adjacent_find(sortedVars.begin(), sortedVars.end()) != sortedVars.end()) {
        return false;
    }
    ArrayOf properties = base.tableProperties();
    for (const char* metadata : { "VariableDescriptions", "VariableUnits", "VariableContinuity" }) {
        if (!properties.isField(metadata) || !properties.field(metadata).isEmpty()) {
            return false;
        }
    }
    // custom properties may be per-variable: indexed by the table class
    if (properties.isField("CustomProperties")
        && !properties.field("CustomProperties").fieldNames().empty()) {
        return false;
    }
    ArrayOf data = base.tableData();
    ArrayOfVector columns;
    stringVector selectedNames;
    columns.reserve(vars.size());
    selectedNames.reserve(vars.size());
    for (indexType varIndex : vars) {
        const std::string& name = names[static_cast<size_t>(varIndex)];
        ArrayOf column = data.field(name);
        if (column.isClassType() || column.isHandle()) {
            return false;
        }
        try {
            columns.push_back(sliceTableColumnRows(column, rowIndex));
        } catch (const NelsonException&) {
            return false; // an invalid row index: the table class reports it
        }
        selectedNames.push_back(name);
    }
    result = ArrayOf::table(columns, selectedNames, stringVector {});
    ArrayOf variableNames = ArrayOf::cellArrayFromArrayOfCharacterRowVectors(selectedNames);
    properties.setField("VariableNames", variableNames);
    stringVector types = ClassName(columns);
    Dimensions typeDimensions(1, types.size());
    ArrayOf variableTypes = ArrayOf::stringArray(types, typeDimensions);
    properties.setField("VariableTypes", variableTypes);
    ArrayOf rowNames = ArrayOf::cellArrayFromArrayOfCharacterRowVectors(stringVector {});
    properties.setField("RowNames", rowNames);
    result.setTableProperties(properties);
    return true;
}
//=============================================================================
bool
tryFastTableDotVariable(
    Evaluator* eval, const ArrayOf& base, const std::string& field, ArrayOf& result)
{
    if (!(isFastTableObject(base) || isFastTimetableObject(base))) {
        return false;
    }
    if (field == "Properties" && base.isField(field)) {
        // as the table and timetable subsref: the stored value, unchecked
        result = base.tableProperties();
        return true;
    }
    if (base.isTimetable()) {
        return readTimetableVariable(eval, base, field, result);
    }
    auto hasNames = [](const stringVector& names, std::initializer_list<const char*> wanted) {
        return std::all_of(wanted.begin(), wanted.end(), [&names](const char* name) {
            return std::find(names.begin(), names.end(), name) != names.end();
        });
    };
    if (!hasNames(base.fieldNames(), { "data", "Properties" })) {
        return false;
    }
    // Properties is [] until the constructor (or set.Properties) first
    // assigns it: such objects are left to the generic path.
    ArrayOf properties = base.tableProperties();
    if (!properties.isStruct() || !properties.isScalar()
        || !hasNames(properties.fieldNames(), { "VariableNames", "VariableTypes", "RowNames" })) {
        return false;
    }
    stringVector names = base.tableVariableNames();
    auto found = std::find(names.begin(), names.end(), field);
    if (found == names.end()) {
        return false;
    }
    ArrayOf data = base.tableData();
    if (!data.isField(field)) {
        return false;
    }
    ArrayOf column = data.field(field);
    // Same reading as tableVariableTypes(), without a second table lookup.
    ArrayOf variableTypes = properties.field("VariableTypes");
    stringVector types
        = variableTypes.isStringArray() ? variableTypes.asUtf8StringVector() : stringVector();
    size_t index = static_cast<size_t>(std::distance(names.begin(), found));
    if (types.size() == names.size() && types[index] != ClassName(column)) {
        return false;
    }
    result = column;
    return true;
}
//=============================================================================
bool
tryFastTableDotVariableIndex(Evaluator* eval, const ArrayOf& base, const std::string& field,
    const ArrayOfVector& params, ArrayOf& result)
{
    // T.name(indices) as the subsref: the variable, indexed as a value. A
    // timetable row subscript given as times, a timerange, a withtol or text
    // is resolved by the timetable subsref.
    ArrayOf variable;
    if (params.empty() || field == "Properties"
        || (base.isTimetable() && isTimetableRowTimeSubscript(params[0]))
        || !tryFastTableDotVariable(eval, base, field, variable) || !isPlainValue(variable)) {
        return false;
    }
    ArrayOfVector indices = params;
    result = indices.size() == 1 ? variable.vectorSubset(indices[0]) : variable.nDimSubset(indices);
    return true;
}
//=============================================================================
bool
tryFastTableDotAssign(
    Evaluator* eval, ArrayOf& base, const std::string& field, const ArrayOf& value)
{
    // T.name = value and TT.name = value for an existing variable name: the
    // subsasgn rebuilds the object, which changes nothing but the variable
    // and its type when the object is already refreshed and value keeps the
    // size of the variable (0x0 deletes it).
    if (!(isFastTableObject(base) || isFastTimetableObject(base)) || !isPlainValue(value)
        || (value.is2D() && value.rows() == 0 && value.columns() == 0)
        || !isOverloadedTableAccess(eval, base)) {
        return false;
    }
    if (field == "Properties") {
        return tryFastTablePropertiesAssign(base, value);
    }
    const bool isTimetable = base.isTimetable();
    if (!base.isField(isTimetable ? "Table" : "data") || !base.isField("Properties")) {
        return false;
    }
    ArrayOf storage = isTimetable ? base.field("Table") : base;
    ArrayOf properties = base.field("Properties");
    RefreshedTableStorage view;
    if ((isTimetable && !(storage.isStruct() && storage.isScalar()))
        || !readRefreshedTableStorage(storage, view)
        || (isTimetable && !isTimetableVariableTarget(properties, view, field))) {
        return false;
    }
    auto found = std::find(view.names.begin(), view.names.end(), field);
    if (found == view.names.end()) {
        return false;
    }
    ArrayOf variable = view.data.field(field);
    if (!isPlainValue(variable) || !variable.dimensions().equals(value.dimensions())
        || value.rows() != view.rows) {
        return false;
    }
    ArrayOf assigned = value;
    view.data.setField(field, assigned);
    storage.setField("data", view.data);
    const std::string type = ClassName(value);
    size_t index = static_cast<size_t>(std::distance(view.names.begin(), found));
    if (view.types[index] != type) {
        view.types[index] = type;
        ArrayOf types = ArrayOf::stringArray(view.types, Dimensions(1, view.types.size()));
        view.properties.setField("VariableTypes", types);
        storage.setField("Properties", view.properties);
        if (isTimetable) {
            properties.setField("VariableTypes", types);
            base.setField("Properties", properties);
        }
    }
    if (isTimetable) {
        base.setField("Table", storage);
    } else {
        base = storage;
    }
    return true;
}
//=============================================================================
bool
isTableDotIndexAssignTarget(Evaluator* eval, const ArrayOf& base)
{
    // T.name(indices) = value and T.name{indices} = value give both
    // subscripts to the subsasgn of a table, a timetable or an eventtable
    // (JUMP_IF_NOT_TABLE).
    return (isFastTableObject(base) || isFastTimetableObject(base)
               || (base.isClassType() && base.classType() == "eventtable"))
        && base.isScalar() && isOverloadedTableAccess(eval, base);
}
//=============================================================================
ArrayOf
readTableDynamicDotIndex(
    Evaluator* eval, const ArrayOf& base, const ArrayOf& fieldValue, const ArrayOfVector& indices)
{
    // T.(name)(indices) on a value accepted by isTableDotIndexAssignTarget:
    // a variable indexed as a value when the fast path applies, otherwise the
    // subsref of T with both subscripts (timetable row time subscripts).
    ArrayOf field = fieldValue;
    if (fieldValue.isStringArray() && fieldValue.isScalar()) {
        field = ArrayOf::characterArray(fieldValue.asUtf8String());
    }
    ArrayOf result;
    if (field.isRowVectorCharacterArray()
        && tryFastTableDotVariableIndex(eval, base, field.asUtf8String(), indices, result)) {
        return result;
    }
    stringVector subtypes = { ".", "()" };
    ArrayOfVector subsindices;
    subsindices.push_back(field);
    subsindices.push_back(classIndexCell(indices));
    bool haveFunction = false;
    ArrayOfVector values = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction, 1);
    if (!haveFunction) {
        RaiseError(L"Invalid indexing.", L"Nelson:interpreter:invalidIndexing");
    }
    return values.empty() ? ArrayOf::empty() : values[0];
}
//=============================================================================
ArrayOf
assignTableDynamicDotIndex(Evaluator* eval, const ArrayOf& base, const ArrayOf& fieldValue,
    const ArrayOfVector& indices, const ArrayOf& value, bool isBraceIndex)
{
    // T.(name)(indices) = value on a target of isTableDotIndexAssignTarget: a
    // string name is given as a char vector, a variable number and any other
    // field value as is, for the subsasgn to check.
    ArrayOf field = fieldValue;
    if (fieldValue.isStringArray() && fieldValue.isScalar()) {
        field = ArrayOf::characterArray(fieldValue.asUtf8String());
    }
    stringVector subtypes = { ".", isBraceIndex ? "{}" : "()" };
    ArrayOfVector subsindices;
    subsindices.push_back(field);
    subsindices.push_back(classIndexCell(indices));
    bool haveFunction = false;
    ArrayOf assigned = eval->bytecodeAssignClass(base, subtypes, subsindices, value, haveFunction);
    if (!haveFunction) {
        RaiseError(L"Cannot apply A.field_name = B to non struct-array object A.",
            L"Nelson:interpreter:invalidFieldAssignment");
    }
    return assigned;
}
//=============================================================================
static ArrayOf
sliceTableColumnRows(ArrayOf column, ArrayOf rowIndex)
{
    ArrayOfVector indices;
    indices.push_back(rowIndex);
    ArrayOf colon = ArrayOf::characterArray(":");
    indexType dimensionCount = std::max<indexType>(2, column.dimensions().length());
    for (indexType k = 1; k < dimensionCount; ++k) {
        indices.push_back(colon);
    }
    return column.nDimSubset(indices);
}
//=============================================================================
static bool
tableVariableIndices(ArrayOf index, const stringVector& names, std::vector<indexType>& vars)
{
    if (isColonIndex(index)) {
        vars.reserve(names.size());
        for (indexType k = 0; k < static_cast<indexType>(names.size()); ++k) {
            vars.push_back(k);
        }
        return true;
    }
    if (!index.isNumeric() || index.isComplex() || index.isSparse()) {
        return false;
    }
    std::vector<indexType> requested;
    try {
        requested = index.asIndexVector();
    } catch (const NelsonException&) {
        return false; // an invalid variable index: the table class reports it
    }
    vars.reserve(requested.size());
    for (indexType value : requested) {
        if (value == 0 || value > static_cast<indexType>(names.size())) {
            return false;
        }
        vars.push_back(value - 1);
    }
    return true;
}
//=============================================================================
static bool
readTimetableVariable(
    Evaluator* eval, const ArrayOf& base, const std::string& field, ArrayOf& result)
{
    // The timetable subsref refreshes the object first, which changes
    // nothing on a timetable left by its methods, then reads a variable of
    // its table storage unless field names a dimension. The refresh renames
    // the data fields by position when a variable name is not one of them.
    if (!isOverloadedTableAccess(eval, base) || !base.isField("Table")) {
        return false;
    }
    ArrayOf properties = base.field("Properties");
    ArrayOf storage = base.field("Table");
    if (!properties.isStruct() || !properties.isScalar() || !properties.isField("DimensionNames")
        || !storage.isStruct() || !storage.isScalar() || !storage.isField("data")
        || !storage.isField("Properties")) {
        return false;
    }
    stringVector dimensionNames;
    if (!readNames(properties.field("DimensionNames"), dimensionNames) || dimensionNames.size() != 2
        || contains(dimensionNames, field)) {
        return false;
    }
    ArrayOf storageProperties = storage.field("Properties");
    stringVector names;
    if (!storageProperties.isStruct() || !storageProperties.isScalar()
        || !storageProperties.isField("VariableNames")
        || !readNames(storageProperties.field("VariableNames"), names) || !contains(names, field)) {
        return false;
    }
    ArrayOf data = storage.field("data");
    if (!data.isStruct() || !data.isScalar()) {
        return false;
    }
    for (const std::string& name : names) {
        if (!data.isField(name)) {
            return false;
        }
    }
    result = data.field(field);
    return true;
}
//=============================================================================
static bool
isFastTableObject(const ArrayOf& base)
{
    if (!base.isTable() || !base.isScalar()) {
        return false;
    }
    return base.isField("data") && base.isField("Version")
        && base.isField("VariableCustomProperties") && base.isField("Properties");
}
//=============================================================================
static bool
isFastTimetableObject(const ArrayOf& base)
{
    if (!base.isTimetable() || !base.isScalar()) {
        return false;
    }
    return base.isField("Table") && base.isField("Version") && base.isField("Properties");
}
//=============================================================================
static bool
isOverloadedTableAccess(Evaluator* eval, const ArrayOf& base)
{
    // Inside the methods of the class, obj.name reaches the property;
    // builtin('subsref'/'subsasgn', ...) clears withOverload.
    return eval != nullptr && eval->withOverload
        && !eval->isClassdefMethodContextOf(base.classType());
}
//=============================================================================
static bool
isPlainValue(const ArrayOf& value)
{
    // A class value may overload size() and indexing.
    return !value.isClassType() && !value.isHandle() && !value.isFunctionHandle()
        && !value.isGraphicsObject();
}
//=============================================================================
static bool
readNames(const ArrayOf& value, stringVector& names)
{
    if (!value.isCell()) {
        return false;
    }
    const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
    indexType count = value.elementCount();
    names.clear();
    names.reserve(static_cast<size_t>(count));
    for (indexType k = 0; k < count; ++k) {
        if (!elements[k].isRowVectorCharacterArray() || elements[k].isEmpty()) {
            return false;
        }
        names.push_back(elements[k].asUtf8String());
    }
    return true;
}
//=============================================================================
static bool
contains(const stringVector& names, const std::string& name)
{
    return std::find(names.begin(), names.end(), name) != names.end();
}
//=============================================================================
static bool
readRefreshedTableStorage(const ArrayOf& storage, RefreshedTableStorage& view)
{
    // What the refresh of a table storage (data, Version, Properties) keeps
    // as it is: the Properties layout of a refreshed table, a row of names
    // naming exactly the data fields, types matching their classes, and
    // dimension names and row names the refresh accepts unchanged.
    if (!storage.isField("data") || !storage.isField("Version") || !storage.isField("Properties")) {
        return false;
    }
    ArrayOf version = storage.field("Version");
    view.properties = storage.field("Properties");
    view.data = storage.field("data");
    if (!version.isDoubleClass() || !version.isScalar() || version.isComplex() || version.isSparse()
        || version.asDoubleScalar() != 2 || !view.properties.isStruct()
        || !view.properties.isScalar()
        || view.properties.fieldNames() != REFRESHED_TABLE_PROPERTY_NAMES || !view.data.isStruct()
        || !view.data.isScalar()) {
        return false;
    }
    ArrayOf variableNames = view.properties.field("VariableNames");
    ArrayOf variableTypes = view.properties.field("VariableTypes");
    if (!variableNames.is2D() || variableNames.rows() != 1 || !readNames(variableNames, view.names)
        || view.names.empty() || !variableTypes.isStringArray()
        || !variableTypes.dimensions().equals(variableNames.dimensions())
        || view.data.fieldNames().size() != view.names.size()) {
        return false;
    }
    view.types = variableTypes.asUtf8StringVector();
    for (size_t k = 0; k < view.names.size(); ++k) {
        if (!view.data.isField(view.names[k])
            || ClassName(view.data.field(view.names[k])) != view.types[k]) {
            return false;
        }
    }
    ArrayOf firstVariable = view.data.field(view.names[0]);
    if (!isPlainValue(firstVariable)) {
        return false;
    }
    view.rows = firstVariable.rows();
    stringVector dimensionNames;
    ArrayOf dimensions = view.properties.field("DimensionNames");
    ArrayOf rowNames = view.properties.field("RowNames");
    if (!dimensions.is2D() || dimensions.rows() != 1 || !readNames(dimensions, dimensionNames)
        || dimensionNames.size() != 2 || contains(view.names, dimensionNames[0])
        || contains(view.names, dimensionNames[1]) || !rowNames.isCell() || !rowNames.is2D()) {
        return false;
    }
    return (rowNames.rows() == 0 && rowNames.columns() == 0)
        || (rowNames.columns() == 1 && rowNames.rows() == view.rows);
}
//=============================================================================
static bool
isTimetableVariableTarget(
    const ArrayOf& properties, const RefreshedTableStorage& view, const std::string& field)
{
    // The timetable Properties carry the storage variable names and types
    // (refreshed), and field is not a dimension name.
    if (!properties.isStruct() || !properties.isScalar() || !properties.isField("DimensionNames")
        || !properties.isField("VariableNames") || !properties.isField("VariableTypes")) {
        return false;
    }
    stringVector dimensionNames;
    stringVector names;
    ArrayOf types = properties.field("VariableTypes");
    return readNames(properties.field("DimensionNames"), dimensionNames)
        && dimensionNames.size() == 2 && !contains(dimensionNames, field)
        && readNames(properties.field("VariableNames"), names) && names == view.names
        && types.isStringArray() && types.asUtf8StringVector() == view.types;
}
//=============================================================================
static bool
tryFastTablePropertiesAssign(ArrayOf& base, const ArrayOf& value)
{
    // T.Properties.Description = x (or UserData): the subsasgn and the
    // Properties setter store such a value as it is, since every other
    // field is the stored one.
    static const std::string description = "Description";
    static const std::string userData = "UserData";
    if (!base.isField("Properties") || !value.isStruct() || !value.isScalar()) {
        return false;
    }
    ArrayOf stored = base.field("Properties");
    if (!stored.isStruct() || !stored.isScalar()) {
        return false;
    }
    stringVector names = stored.fieldNames();
    if (value.fieldNames() != names) {
        return false;
    }
    for (const std::string& name : names) {
        if (name != description && name != userData
            && !isSameStoredValue(value.field(name), stored.field(name))) {
            return false;
        }
    }
    ArrayOf assigned = value;
    base.setField("Properties", assigned);
    return true;
}
//=============================================================================
static bool
isSameStoredValue(const ArrayOf& a, const ArrayOf& b)
{
    return a.dataPointer() == b.dataPointer() && a.dataClass() == b.dataClass()
        && a.dimensions().equals(b.dimensions())
        && (!a.isClassType() || a.classType() == b.classType());
}
//=============================================================================
static bool
isTimetableRowTimeSubscript(const ArrayOf& index)
{
    // datetime, duration, timerange and withtol are objects; text other than
    // ':' names row times.
    if (index.isClassType() || index.isStringArray() || index.isCell()) {
        return true;
    }
    return index.isCharacterArray() && index.asWideString() != L":";
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
