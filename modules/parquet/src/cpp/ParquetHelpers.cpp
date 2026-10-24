//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ParquetHelpers.hpp"
#include "Error.hpp"
#include <arrow/array/array_binary.h>
#include <arrow/array/array_nested.h>
#include <arrow/array/array_primitive.h>
#include <arrow/array/builder_binary.h>
#include <arrow/array/builder_primitive.h>
#include <arrow/compute/api.h>
#include <arrow/result.h>
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
rowTimesName(const ArrayOf& timetableValue, const stringVector& existingNames);
//=============================================================================
static std::string
makeUniqueName(const std::string& requestedName, const stringVector& existingNames);
//=============================================================================
std::wstring
toWideError(const std::string& message)
{
    return utf8_to_wstring(message);
}
//=============================================================================
std::string
toUtf8Path(const std::wstring& filename)
{
    return wstring_to_utf8(filename);
}
//=============================================================================
bool
isDuration(const ArrayOf& value)
{
    return value.isClassType() && value.classType() == "duration";
}
//=============================================================================
bool
isDatetime(const ArrayOf& value)
{
    return value.isClassType() && value.classType() == "datetime";
}
//=============================================================================
static ArrayOf
makeClassVector(const std::string& className, const std::string& valueField,
    const std::vector<double>& values, const std::string& format)
{
    Dimensions dims(values.size(), 1);
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    std::copy(values.begin(), values.end(), data);
    ArrayOfVector fieldValues;
    fieldValues.push_back(ArrayOf(NLS_DOUBLE, dims, data));
    fieldValues.push_back(ArrayOf::characterArray(format));
    if (className == "datetime") {
        fieldValues.push_back(ArrayOf::characterArray(""));
        return ArrayOf::classdefArray(
            className, { valueField, "FormatValue", "TimeZoneValue" }, fieldValues);
    }
    return ArrayOf::classdefArray(className, { valueField, "FormatValue" }, fieldValues);
}
//=============================================================================
ArrayOf
makeDatetimeColumn(const std::vector<double>& serialValues)
{
    return makeClassVector("datetime", "SerialValue", serialValues, "default");
}
//=============================================================================
ArrayOf
makeDurationColumn(const std::vector<double>& secondsValues)
{
    return makeClassVector("duration", "SecondsValue", secondsValues, "default");
}
//=============================================================================
static bool
isColumnVectorLike(const ArrayOf& column)
{
    if (column.isCharacterArray()) {
        return column.rows() == 1 || column.columns() == 1;
    }
    return column.isVector() || column.columns() == 1;
}
//=============================================================================
static std::shared_ptr<arrow::DataType>
arrowListValueTypeForCellColumn(const ArrayOf& column, std::wstring& errorMessage)
{
    const auto* cells = static_cast<const ArrayOf*>(column.dataPointer());
    std::shared_ptr<arrow::DataType> valueType;
    for (indexType k = 0; k < column.elementCount(); ++k) {
        if (cells[k].isEmpty()) {
            continue;
        }
        if (cells[k].isCell() || cells[k].isTable() || cells[k].isStruct()
            || cells[k].isClassType()) {
            errorMessage
                = L"Only homogeneous primitive cell vector columns are supported by parquetwrite.";
            return nullptr;
        }
        if (!isColumnVectorLike(cells[k])) {
            errorMessage = L"Only scalar or vector cell elements are supported by parquetwrite.";
            return nullptr;
        }
        auto candidateType = arrowTypeForNelsonColumn(cells[k], errorMessage);
        if (!errorMessage.empty()) {
            return nullptr;
        }
        if (!valueType) {
            valueType = candidateType;
        } else if (!valueType->Equals(*candidateType)) {
            errorMessage = L"Cell vector columns must contain homogeneous element types.";
            return nullptr;
        }
    }
    if (!valueType) {
        errorMessage = L"Cannot infer Parquet list type from an empty cell column.";
        return nullptr;
    }
    return valueType;
}
//=============================================================================
std::shared_ptr<arrow::DataType>
arrowTypeForNelsonColumn(const ArrayOf& column, std::wstring& errorMessage)
{
    if (column.isTable() || column.isTimetable()) {
        std::vector<std::shared_ptr<arrow::Field>> fields;
        for (const auto& [name, value] : tabularColumns(column)) {
            auto type = arrowTypeForNelsonColumn(value, errorMessage);
            if (!errorMessage.empty()) {
                return nullptr;
            }
            fields.push_back(arrow::field(name, type));
        }
        return arrow::struct_(fields);
    }
    if (column.isSparse() || column.isComplex()) {
        errorMessage = L"Complex and sparse variables are not supported by parquetwrite.";
        return nullptr;
    }
    if (!isColumnVectorLike(column)) {
        errorMessage = L"Only scalar or vector table variables are supported by parquetwrite.";
        return nullptr;
    }
    if (isDatetime(column)) {
        return arrow::timestamp(arrow::TimeUnit::MICRO);
    }
    if (isDuration(column)) {
        return arrow::duration(arrow::TimeUnit::MICRO);
    }
    if (column.isCell()) {
        if (column.isCellArrayOfCharacterVectors()) {
            return arrow::utf8();
        }
        auto valueType = arrowListValueTypeForCellColumn(column, errorMessage);
        if (!errorMessage.empty()) {
            return nullptr;
        }
        return arrow::list(valueType);
    }
    switch (column.dataClass()) {
    case NLS_LOGICAL:
        return arrow::boolean();
    case NLS_INT8:
        return arrow::int8();
    case NLS_UINT8:
        return arrow::uint8();
    case NLS_INT16:
        return arrow::int16();
    case NLS_UINT16:
        return arrow::uint16();
    case NLS_INT32:
        return arrow::int32();
    case NLS_UINT32:
        return arrow::uint32();
    case NLS_INT64:
        return arrow::int64();
    case NLS_UINT64:
        return arrow::uint64();
    case NLS_SINGLE:
        return arrow::float32();
    case NLS_DOUBLE:
        return arrow::float64();
    case NLS_CHAR:
    case NLS_STRING_ARRAY:
        return arrow::utf8();
    default:
        errorMessage = L"Unsupported table variable type for parquetwrite.";
        return nullptr;
    }
}
//=============================================================================
template <typename BuilderType, typename ValueType>
static std::shared_ptr<arrow::Array>
buildPrimitiveArray(const ArrayOf& column, std::wstring& errorMessage)
{
    BuilderType builder;
    const auto* data = static_cast<const ValueType*>(column.dataPointer());
    for (indexType k = 0; k < column.elementCount(); ++k) {
        auto status = builder.Append(data[k]);
        if (!status.ok()) {
            errorMessage = toWideError(status.ToString());
            return nullptr;
        }
    }
    auto result = builder.Finish();
    if (!result.ok()) {
        errorMessage = toWideError(result.status().ToString());
        return nullptr;
    }
    return *result;
}
//=============================================================================
static std::shared_ptr<arrow::Array>
buildStringArray(const ArrayOf& column, std::wstring& errorMessage)
{
    arrow::StringBuilder builder;
    stringVector values = column.asUtf8StringVector();
    for (const auto& value : values) {
        auto status = builder.Append(value);
        if (!status.ok()) {
            errorMessage = toWideError(status.ToString());
            return nullptr;
        }
    }
    auto result = builder.Finish();
    if (!result.ok()) {
        errorMessage = toWideError(result.status().ToString());
        return nullptr;
    }
    return *result;
}
//=============================================================================
template <typename BuilderType, typename ValueType, NelsonType ClassId>
static std::shared_ptr<arrow::Array>
buildPrimitiveListArray(const ArrayOf& column, std::wstring& errorMessage)
{
    auto valueBuilder = std::make_shared<BuilderType>();
    arrow::ListBuilder builder(arrow::default_memory_pool(), valueBuilder);
    const auto* cells = static_cast<const ArrayOf*>(column.dataPointer());
    for (indexType k = 0; k < column.elementCount(); ++k) {
        if (cells[k].dataClass() != ClassId) {
            errorMessage = L"Cell vector columns must contain homogeneous element types.";
            return nullptr;
        }
        if (cells[k].isSparse() || cells[k].isComplex() || !isColumnVectorLike(cells[k])) {
            errorMessage
                = L"Only scalar or vector primitive cell elements are supported by parquetwrite.";
            return nullptr;
        }
        auto status = builder.Append();
        if (!status.ok()) {
            errorMessage = toWideError(status.ToString());
            return nullptr;
        }
        const auto* data = static_cast<const ValueType*>(cells[k].dataPointer());
        for (indexType j = 0; j < cells[k].elementCount(); ++j) {
            status = valueBuilder->Append(data[j]);
            if (!status.ok()) {
                errorMessage = toWideError(status.ToString());
                return nullptr;
            }
        }
    }
    auto result = builder.Finish();
    if (!result.ok()) {
        errorMessage = toWideError(result.status().ToString());
        return nullptr;
    }
    return *result;
}
//=============================================================================
static std::shared_ptr<arrow::Array>
buildListArray(const ArrayOf& column, std::wstring& errorMessage)
{
    auto valueType = arrowListValueTypeForCellColumn(column, errorMessage);
    if (!errorMessage.empty()) {
        return nullptr;
    }
    switch (valueType->id()) {
    case arrow::Type::BOOL:
        return buildPrimitiveListArray<arrow::BooleanBuilder, logical, NLS_LOGICAL>(
            column, errorMessage);
    case arrow::Type::INT8:
        return buildPrimitiveListArray<arrow::Int8Builder, int8, NLS_INT8>(column, errorMessage);
    case arrow::Type::UINT8:
        return buildPrimitiveListArray<arrow::UInt8Builder, uint8, NLS_UINT8>(column, errorMessage);
    case arrow::Type::INT16:
        return buildPrimitiveListArray<arrow::Int16Builder, int16, NLS_INT16>(column, errorMessage);
    case arrow::Type::UINT16:
        return buildPrimitiveListArray<arrow::UInt16Builder, uint16, NLS_UINT16>(
            column, errorMessage);
    case arrow::Type::INT32:
        return buildPrimitiveListArray<arrow::Int32Builder, int32, NLS_INT32>(column, errorMessage);
    case arrow::Type::UINT32:
        return buildPrimitiveListArray<arrow::UInt32Builder, uint32, NLS_UINT32>(
            column, errorMessage);
    case arrow::Type::INT64:
        return buildPrimitiveListArray<arrow::Int64Builder, int64, NLS_INT64>(column, errorMessage);
    case arrow::Type::UINT64:
        return buildPrimitiveListArray<arrow::UInt64Builder, uint64, NLS_UINT64>(
            column, errorMessage);
    case arrow::Type::FLOAT:
        return buildPrimitiveListArray<arrow::FloatBuilder, single, NLS_SINGLE>(
            column, errorMessage);
    case arrow::Type::DOUBLE:
        return buildPrimitiveListArray<arrow::DoubleBuilder, double, NLS_DOUBLE>(
            column, errorMessage);
    default:
        errorMessage = L"Only primitive numeric and logical cell vector columns are supported.";
        return nullptr;
    }
}
//=============================================================================
static std::shared_ptr<arrow::Array>
buildTimeArray(const ArrayOf& column, bool datetime, std::wstring& errorMessage)
{
    arrow::Int64Builder builder;
    ArrayOf values = column.field(datetime ? "SerialValue" : "SecondsValue");
    const auto* data = static_cast<const double*>(values.dataPointer());
    for (indexType k = 0; k < values.elementCount(); ++k) {
        double seconds = datetime ? ((data[k] - SERIAL_UNIX_EPOCH) * SECONDS_PER_DAY) : data[k];
        auto status = builder.Append(static_cast<int64_t>(std::llround(seconds * 1000000.0)));
        if (!status.ok()) {
            errorMessage = toWideError(status.ToString());
            return nullptr;
        }
    }
    auto raw = builder.Finish();
    if (!raw.ok()) {
        errorMessage = toWideError(raw.status().ToString());
        return nullptr;
    }
    auto type = datetime ? arrow::timestamp(arrow::TimeUnit::MICRO)
                         : arrow::duration(arrow::TimeUnit::MICRO);
    auto casted = arrow::compute::Cast(*raw, type);
    if (!casted.ok()) {
        errorMessage = toWideError(casted.status().ToString());
        return nullptr;
    }
    return casted->array_as<arrow::Array>();
}
//=============================================================================
static std::shared_ptr<arrow::Array>
buildStructArray(const ArrayOf& column, std::wstring& errorMessage)
{
    std::vector<std::shared_ptr<arrow::Field>> fields;
    std::vector<std::shared_ptr<arrow::Array>> arrays;
    for (const auto& [name, value] : tabularColumns(column)) {
        auto type = arrowTypeForNelsonColumn(value, errorMessage);
        if (!errorMessage.empty()) {
            return nullptr;
        }
        auto array = arrowArrayFromNelsonColumn(value, errorMessage);
        if (!errorMessage.empty()) {
            return nullptr;
        }
        fields.push_back(arrow::field(name, type));
        arrays.push_back(array);
    }
    auto result = arrow::StructArray::Make(arrays, fields);
    if (!result.ok()) {
        errorMessage = toWideError(result.status().ToString());
        return nullptr;
    }
    return *result;
}
//=============================================================================
std::shared_ptr<arrow::Array>
arrowArrayFromNelsonColumn(const ArrayOf& column, std::wstring& errorMessage)
{
    if (column.isTable() || column.isTimetable()) {
        return buildStructArray(column, errorMessage);
    }
    if (isDatetime(column)) {
        return buildTimeArray(column, true, errorMessage);
    }
    if (isDuration(column)) {
        return buildTimeArray(column, false, errorMessage);
    }
    if (column.isCell() && !column.isCellArrayOfCharacterVectors()) {
        return buildListArray(column, errorMessage);
    }
    switch (column.dataClass()) {
    case NLS_LOGICAL:
        return buildPrimitiveArray<arrow::BooleanBuilder, logical>(column, errorMessage);
    case NLS_INT8:
        return buildPrimitiveArray<arrow::Int8Builder, int8>(column, errorMessage);
    case NLS_UINT8:
        return buildPrimitiveArray<arrow::UInt8Builder, uint8>(column, errorMessage);
    case NLS_INT16:
        return buildPrimitiveArray<arrow::Int16Builder, int16>(column, errorMessage);
    case NLS_UINT16:
        return buildPrimitiveArray<arrow::UInt16Builder, uint16>(column, errorMessage);
    case NLS_INT32:
        return buildPrimitiveArray<arrow::Int32Builder, int32>(column, errorMessage);
    case NLS_UINT32:
        return buildPrimitiveArray<arrow::UInt32Builder, uint32>(column, errorMessage);
    case NLS_INT64:
        return buildPrimitiveArray<arrow::Int64Builder, int64>(column, errorMessage);
    case NLS_UINT64:
        return buildPrimitiveArray<arrow::UInt64Builder, uint64>(column, errorMessage);
    case NLS_SINGLE:
        return buildPrimitiveArray<arrow::FloatBuilder, single>(column, errorMessage);
    case NLS_DOUBLE:
        return buildPrimitiveArray<arrow::DoubleBuilder, double>(column, errorMessage);
    case NLS_CHAR:
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY:
        return buildStringArray(column, errorMessage);
    default:
        errorMessage = L"Unsupported table variable type for parquetwrite.";
        return nullptr;
    }
}
//=============================================================================
std::vector<std::pair<std::string, ArrayOf>>
tabularColumns(const ArrayOf& tabularValue)
{
    bool isTimetable = tabularValue.isTimetable();
    ArrayOf source = isTimetable ? tabularValue.field("Table") : tabularValue;
    stringVector names = source.tableVariableNames();
    std::vector<std::pair<std::string, ArrayOf>> columns;
    columns.reserve(names.size() + (isTimetable ? 1 : 0));
    if (isTimetable) {
        ArrayOf properties = tabularValue.field("Properties");
        columns.emplace_back(rowTimesName(tabularValue, names), properties.field("RowTimes"));
    }
    for (const auto& name : names) {
        columns.emplace_back(name, source.tableColumn(name));
    }
    return columns;
}
//=============================================================================
std::string
makeUniqueName(const std::string& requestedName, const stringVector& existingNames)
{
    if (std::find(existingNames.begin(), existingNames.end(), requestedName)
        == existingNames.end()) {
        return requestedName;
    }
    std::string base = requestedName;
    int index = 1;
    std::string candidate;
    do {
        candidate = base + "_" + std::to_string(index++);
    } while (
        std::find(existingNames.begin(), existingNames.end(), candidate) != existingNames.end());
    return candidate;
}
//=============================================================================
std::string
rowTimesName(const ArrayOf& timetableValue, const stringVector& existingNames)
{
    std::string name = "Time";
    ArrayOf properties = timetableValue.field("Properties");
    if (properties.isField("DimensionNames")) {
        ArrayOf names = properties.field("DimensionNames");
        if (names.isCellArrayOfCharacterVectors() && names.elementCount() > 0) {
            stringVector values = names.asUtf8StringVector(false);
            if (!values.empty() && !values[0].empty()) {
                name = values[0];
            }
        }
    }
    return makeUniqueName(name, existingNames);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
