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
#include <arrow/array/concatenate.h>
#include <arrow/array/array_binary.h>
#include <arrow/array/array_nested.h>
#include <arrow/array/array_primitive.h>
#include <arrow/compute/api.h>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename ArrowArrayType, typename NelsonTypeValue, NelsonType ClassId>
static ArrayOf
makeTypedColumn(const std::shared_ptr<arrow::Array>& array, std::wstring& errorMessage)
{
    auto typed = std::static_pointer_cast<ArrowArrayType>(array);
    if (typed->null_count() > 0) {
        errorMessage = L"Null values in integer or logical columns are not supported.";
        return {};
    }
    auto count = static_cast<indexType>(typed->length());
    Dimensions dims(count, 1);
    NelsonTypeValue* data = static_cast<NelsonTypeValue*>(ArrayOf::allocateArrayOf(ClassId, count));
    for (indexType k = 0; k < count; ++k) {
        data[k] = static_cast<NelsonTypeValue>(typed->Value(k));
    }
    return ArrayOf(ClassId, dims, data);
}
//=============================================================================
template <typename ArrowArrayType, typename NelsonTypeValue, NelsonType ClassId>
static ArrayOf
makeFloatingColumn(const std::shared_ptr<arrow::Array>& array)
{
    auto typed = std::static_pointer_cast<ArrowArrayType>(array);
    auto count = static_cast<indexType>(typed->length());
    Dimensions dims(count, 1);
    NelsonTypeValue* data = static_cast<NelsonTypeValue*>(ArrayOf::allocateArrayOf(ClassId, count));
    for (indexType k = 0; k < count; ++k) {
        data[k] = typed->IsNull(k) ? static_cast<NelsonTypeValue>(NAN)
                                   : static_cast<NelsonTypeValue>(typed->Value(k));
    }
    return ArrayOf(ClassId, dims, data);
}
//=============================================================================
static ArrayOf
makeStringColumn(const std::shared_ptr<arrow::Array>& array)
{
    auto typed = std::static_pointer_cast<arrow::StringArray>(array);
    stringVector values;
    values.reserve(static_cast<size_t>(typed->length()));
    for (int64_t k = 0; k < typed->length(); ++k) {
        values.push_back(typed->IsNull(k) ? "" : typed->GetString(k));
    }
    Dimensions dims(values.size(), 1);
    return ArrayOf::stringArray(values, dims);
}
//=============================================================================
static ArrayOf
makeTimestampColumn(const std::shared_ptr<arrow::Array>& array)
{
    auto typed = std::static_pointer_cast<arrow::TimestampArray>(array);
    std::vector<double> values;
    values.reserve(static_cast<size_t>(typed->length()));
    for (int64_t k = 0; k < typed->length(); ++k) {
        double serial = NAN;
        if (!typed->IsNull(k)) {
            double seconds = static_cast<double>(typed->Value(k)) / 1000000.0;
            serial = seconds / SECONDS_PER_DAY + SERIAL_UNIX_EPOCH;
        }
        values.push_back(serial);
    }
    return makeDatetimeColumn(values);
}
//=============================================================================
static ArrayOf
makeDurationColumnFromArrow(const std::shared_ptr<arrow::Array>& array)
{
    auto typed = std::static_pointer_cast<arrow::DurationArray>(array);
    std::vector<double> values;
    values.reserve(static_cast<size_t>(typed->length()));
    for (int64_t k = 0; k < typed->length(); ++k) {
        values.push_back(typed->IsNull(k) ? NAN : static_cast<double>(typed->Value(k)) / 1000000.0);
    }
    return makeDurationColumn(values);
}
//=============================================================================
static ArrayOf
makeListColumn(const std::shared_ptr<arrow::Array>& array,
    const std::shared_ptr<arrow::Field>& field, std::wstring& errorMessage)
{
    auto typed = std::static_pointer_cast<arrow::ListArray>(array);
    auto type = std::static_pointer_cast<arrow::ListType>(field->type());
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, static_cast<indexType>(typed->length())));
    for (int64_t k = 0; k < typed->length(); ++k) {
        if (typed->IsNull(k)) {
            elements[k] = ArrayOf(NLS_DOUBLE, Dimensions(0, 1), nullptr);
            continue;
        }
        int64_t offset = typed->value_offset(k);
        int64_t length = typed->value_length(k);
        auto childArray = typed->values()->Slice(offset, length);
        auto child = std::make_shared<arrow::ChunkedArray>(childArray);
        elements[k] = nelsonColumnFromArrowArray(child, type->value_field(), errorMessage);
        if (!errorMessage.empty()) {
            return {};
        }
    }
    return ArrayOf(
        NLS_CELL_ARRAY, Dimensions(static_cast<indexType>(typed->length()), 1), elements);
}
//=============================================================================
static ArrayOf
makeStructColumn(const std::shared_ptr<arrow::Array>& array,
    const std::shared_ptr<arrow::Field>& field, std::wstring& errorMessage)
{
    auto typed = std::static_pointer_cast<arrow::StructArray>(array);
    auto type = std::static_pointer_cast<arrow::StructType>(field->type());
    ArrayOfVector columns;
    stringVector names;
    for (int k = 0; k < type->num_fields(); ++k) {
        auto child = std::make_shared<arrow::ChunkedArray>(typed->field(k));
        ArrayOf column = nelsonColumnFromArrowArray(child, type->field(k), errorMessage);
        if (!errorMessage.empty()) {
            return {};
        }
        columns.push_back(column);
        names.push_back(type->field(k)->name());
    }
    return ArrayOf::table(columns, names, {});
}
//=============================================================================
static std::shared_ptr<arrow::Array>
combineColumn(const std::shared_ptr<arrow::ChunkedArray>& column, std::wstring& errorMessage)
{
    auto combined = arrow::Concatenate(column->chunks());
    if (!combined.ok()) {
        errorMessage = toWideError(combined.status().ToString());
        return nullptr;
    }
    return *combined;
}
//=============================================================================
ArrayOf
nelsonColumnFromArrowArray(const std::shared_ptr<arrow::ChunkedArray>& column,
    const std::shared_ptr<arrow::Field>& field, std::wstring& errorMessage)
{
    auto array = combineColumn(column, errorMessage);
    if (!errorMessage.empty()) {
        return {};
    }
    switch (field->type()->id()) {
    case arrow::Type::BOOL:
        return makeTypedColumn<arrow::BooleanArray, logical, NLS_LOGICAL>(array, errorMessage);
    case arrow::Type::INT8:
        return makeTypedColumn<arrow::Int8Array, int8, NLS_INT8>(array, errorMessage);
    case arrow::Type::UINT8:
        return makeTypedColumn<arrow::UInt8Array, uint8, NLS_UINT8>(array, errorMessage);
    case arrow::Type::INT16:
        return makeTypedColumn<arrow::Int16Array, int16, NLS_INT16>(array, errorMessage);
    case arrow::Type::UINT16:
        return makeTypedColumn<arrow::UInt16Array, uint16, NLS_UINT16>(array, errorMessage);
    case arrow::Type::INT32:
        return makeTypedColumn<arrow::Int32Array, int32, NLS_INT32>(array, errorMessage);
    case arrow::Type::UINT32:
        return makeTypedColumn<arrow::UInt32Array, uint32, NLS_UINT32>(array, errorMessage);
    case arrow::Type::INT64:
        return makeTypedColumn<arrow::Int64Array, int64, NLS_INT64>(array, errorMessage);
    case arrow::Type::UINT64:
        return makeTypedColumn<arrow::UInt64Array, uint64, NLS_UINT64>(array, errorMessage);
    case arrow::Type::FLOAT:
        return makeFloatingColumn<arrow::FloatArray, single, NLS_SINGLE>(array);
    case arrow::Type::DOUBLE:
        return makeFloatingColumn<arrow::DoubleArray, double, NLS_DOUBLE>(array);
    case arrow::Type::STRING:
    case arrow::Type::LARGE_STRING:
        return makeStringColumn(array);
    case arrow::Type::TIMESTAMP:
        return makeTimestampColumn(array);
    case arrow::Type::DURATION:
        return makeDurationColumnFromArrow(array);
    case arrow::Type::LIST:
        return makeListColumn(array, field, errorMessage);
    case arrow::Type::STRUCT:
        return makeStructColumn(array, field, errorMessage);
    default:
        errorMessage = L"Unsupported Parquet column type.";
        return {};
    }
}
//=============================================================================
std::vector<int>
selectedColumnIndices(const std::shared_ptr<arrow::Schema>& schema,
    const stringVector& selectedNames, std::wstring& errorMessage)
{
    std::vector<int> indices;
    if (selectedNames.empty()) {
        for (int k = 0; k < schema->num_fields(); ++k) {
            indices.push_back(k);
        }
        return indices;
    }
    for (const auto& name : selectedNames) {
        int index = schema->GetFieldIndex(name);
        if (index < 0) {
            errorMessage = L"Selected variable name not found in Parquet file.";
            return {};
        }
        indices.push_back(index);
    }
    return indices;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
