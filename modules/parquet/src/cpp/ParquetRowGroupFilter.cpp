//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ParquetRowGroupFilter.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <arrow/util/config.h>
#include <parquet/statistics.h>
#include <parquet/types.h>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    enum class MatchState
    {
        No,
        Maybe
    };
    //=============================================================================
    enum class ConstantKind
    {
        Number,
        Text,
        Unsupported
    };
    //=============================================================================
    struct ConstantValue
    {
        ConstantKind kind = ConstantKind::Unsupported;
        double number = 0.;
        std::string text;
    };
    //=============================================================================
    struct NumericRange
    {
        double min = 0.;
        double max = 0.;
        bool hasNulls = false;
    };
    //=============================================================================
    struct TextRange
    {
        std::string min;
        std::string max;
        bool hasNulls = false;
    };
    //=============================================================================
    bool
    hasField(const ArrayOf& value, const std::string& fieldName)
    {
        return value.isScalar() && value.isField(fieldName);
    }
    //=============================================================================
    std::string
    expressionKind(const ArrayOf& expression)
    {
        if (!hasField(expression, "kind")) {
            return {};
        }
        return expression.field("kind").asUtf8String();
    }
    //=============================================================================
    bool
    isExpressionKind(const ArrayOf& expression, const std::string& kind)
    {
        return expressionKind(expression) == kind;
    }
    //=============================================================================
    std::string
    variableName(const ArrayOf& expression)
    {
        if (!isExpressionKind(expression, "var") || !hasField(expression, "name")) {
            return {};
        }
        return expression.field("name").asUtf8String();
    }
    //=============================================================================
    ConstantValue
    constantValue(const ArrayOf& expression)
    {
        ConstantValue result;
        if (!isExpressionKind(expression, "const") || !hasField(expression, "value")) {
            return result;
        }
        ArrayOf value = expression.field("value");
        if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
            result.kind = ConstantKind::Text;
            result.text = value.asUtf8String();
            return result;
        }
        if (value.isLogical()) {
            result.kind = ConstantKind::Number;
            result.number = value.asLogicalScalar() ? 1. : 0.;
            return result;
        }
        if (value.isNumeric() && value.isScalar()) {
            result.kind = ConstantKind::Number;
            result.number = value.asDoubleScalar();
        }
        return result;
    }
    //=============================================================================
    std::string
    reverseOperator(const std::string& op)
    {
        if (op == "gt") {
            return "lt";
        }
        if (op == "ge") {
            return "le";
        }
        if (op == "lt") {
            return "gt";
        }
        if (op == "le") {
            return "ge";
        }
        return op;
    }
    //=============================================================================
    int
    findPrimitiveColumn(
        const std::shared_ptr<parquet::FileMetaData>& metadata, const std::string& name)
    {
        for (int k = 0; k < metadata->num_columns(); ++k) {
            const std::string path = metadata->schema()->Column(k)->path()->ToDotString();
            if (path == name) {
                return k;
            }
        }
        return -1;
    }
    //=============================================================================
    bool
    statsAreExact(const std::shared_ptr<parquet::Statistics>& stats)
    {
#if ARROW_VERSION_MAJOR >= 22
        auto minExact = stats->is_min_value_exact();
        auto maxExact = stats->is_max_value_exact();
        return (!minExact.has_value() || minExact.value())
            && (!maxExact.has_value() || maxExact.value());
#else
        // Older Arrow releases do not expose the exactness flags: trust the bounds.
        return true;
#endif
    }
    //=============================================================================
    bool
    hasNulls(const std::shared_ptr<parquet::Statistics>& stats)
    {
        return stats->HasNullCount() && stats->null_count() > 0;
    }
    //=============================================================================
    std::string
    byteArrayToString(const parquet::ByteArray& value)
    {
        return std::string(reinterpret_cast<const char*>(value.ptr), value.len);
    }
    //=============================================================================
    bool
    numericRangeForStats(const std::shared_ptr<parquet::Statistics>& stats, NumericRange& range)
    {
        if (!stats || !stats->HasMinMax() || !statsAreExact(stats)) {
            return false;
        }
        range.hasNulls = hasNulls(stats);
        switch (stats->physical_type()) {
        case parquet::Type::BOOLEAN: {
            auto typed = std::static_pointer_cast<parquet::BoolStatistics>(stats);
            range.min = typed->min() ? 1. : 0.;
            range.max = typed->max() ? 1. : 0.;
            return true;
        }
        case parquet::Type::INT32: {
            auto typed = std::static_pointer_cast<parquet::Int32Statistics>(stats);
            range.min = static_cast<double>(typed->min());
            range.max = static_cast<double>(typed->max());
            return true;
        }
        case parquet::Type::INT64: {
            auto typed = std::static_pointer_cast<parquet::Int64Statistics>(stats);
            range.min = static_cast<double>(typed->min());
            range.max = static_cast<double>(typed->max());
            return true;
        }
        case parquet::Type::FLOAT: {
            auto typed = std::static_pointer_cast<parquet::FloatStatistics>(stats);
            range.min = static_cast<double>(typed->min());
            range.max = static_cast<double>(typed->max());
            return true;
        }
        case parquet::Type::DOUBLE: {
            auto typed = std::static_pointer_cast<parquet::DoubleStatistics>(stats);
            range.min = typed->min();
            range.max = typed->max();
            return true;
        }
        default:
            return false;
        }
    }
    //=============================================================================
    bool
    textRangeForStats(const std::shared_ptr<parquet::Statistics>& stats, TextRange& range)
    {
        if (!stats || !stats->HasMinMax() || !statsAreExact(stats)
            || stats->physical_type() != parquet::Type::BYTE_ARRAY) {
            return false;
        }
        auto typed = std::static_pointer_cast<parquet::ByteArrayStatistics>(stats);
        range.min = byteArrayToString(typed->min());
        range.max = byteArrayToString(typed->max());
        range.hasNulls = hasNulls(stats);
        return true;
    }
    //=============================================================================
    bool
    rangeMayMatch(const NumericRange& range, const std::string& op, double value)
    {
        if (std::isnan(value)) {
            return true;
        }
        if (op == "gt") {
            return range.max > value;
        }
        if (op == "ge") {
            return range.max >= value;
        }
        if (op == "lt") {
            return range.min < value;
        }
        if (op == "le") {
            return range.min <= value;
        }
        if (op == "eq") {
            return range.min <= value && value <= range.max;
        }
        if (op == "ne") {
            return range.hasNulls || !(range.min == value && range.max == value);
        }
        return true;
    }
    //=============================================================================
    bool
    rangeMayMatch(const TextRange& range, const std::string& op, const std::string& value)
    {
        if (op == "gt") {
            return range.max > value;
        }
        if (op == "ge") {
            return range.max >= value;
        }
        if (op == "lt") {
            return range.min < value;
        }
        if (op == "le") {
            return range.min <= value;
        }
        if (op == "eq") {
            return range.min <= value && value <= range.max;
        }
        if (op == "ne") {
            return range.hasNulls || !(range.min == value && range.max == value);
        }
        return true;
    }
    //=============================================================================
    MatchState
    matchComparison(const std::shared_ptr<parquet::FileMetaData>& metadata, int rowGroupIndex,
        const std::string& op, const std::string& name, const ConstantValue& constant)
    {
        int columnIndex = findPrimitiveColumn(metadata, name);
        if (columnIndex < 0) {
            return MatchState::Maybe;
        }
        auto chunk = metadata->RowGroup(rowGroupIndex)->ColumnChunk(columnIndex);
        if (!chunk || !chunk->is_stats_set()) {
            return MatchState::Maybe;
        }
        auto stats = chunk->statistics();
        if (constant.kind == ConstantKind::Number) {
            NumericRange range;
            if (!numericRangeForStats(stats, range)) {
                return MatchState::Maybe;
            }
            return rangeMayMatch(range, op, constant.number) ? MatchState::Maybe : MatchState::No;
        }
        if (constant.kind == ConstantKind::Text) {
            TextRange range;
            if (!textRangeForStats(stats, range)) {
                return MatchState::Maybe;
            }
            return rangeMayMatch(range, op, constant.text) ? MatchState::Maybe : MatchState::No;
        }
        return MatchState::Maybe;
    }
    //=============================================================================
    MatchState
    matchExpression(const std::shared_ptr<parquet::FileMetaData>& metadata, int rowGroupIndex,
        const ArrayOf& expression)
    {
        std::string kind = expressionKind(expression);
        if (kind == "and") {
            MatchState left = matchExpression(metadata, rowGroupIndex, expression.field("left"));
            if (left == MatchState::No) {
                return MatchState::No;
            }
            return matchExpression(metadata, rowGroupIndex, expression.field("right"));
        }
        if (kind == "or") {
            MatchState left = matchExpression(metadata, rowGroupIndex, expression.field("left"));
            MatchState right = matchExpression(metadata, rowGroupIndex, expression.field("right"));
            return (left == MatchState::No && right == MatchState::No) ? MatchState::No
                                                                       : MatchState::Maybe;
        }
        if (kind == "not") {
            return MatchState::Maybe;
        }
        if (kind == "gt" || kind == "ge" || kind == "lt" || kind == "le" || kind == "eq"
            || kind == "ne") {
            ArrayOf left = expression.field("left");
            ArrayOf right = expression.field("right");
            std::string name = variableName(left);
            ConstantValue constant = constantValue(right);
            std::string op = kind;
            if (name.empty() || constant.kind == ConstantKind::Unsupported) {
                name = variableName(right);
                constant = constantValue(left);
                op = reverseOperator(kind);
            }
            if (name.empty() || constant.kind == ConstantKind::Unsupported) {
                return MatchState::Maybe;
            }
            return matchComparison(metadata, rowGroupIndex, op, name, constant);
        }
        return MatchState::Maybe;
    }
    //=============================================================================
    std::vector<int>
    baseRowGroups(const std::shared_ptr<parquet::FileMetaData>& metadata,
        const std::vector<int>& requestedRowGroups)
    {
        if (!requestedRowGroups.empty()) {
            return requestedRowGroups;
        }
        std::vector<int> groups;
        groups.reserve(metadata->num_row_groups());
        for (int k = 0; k < metadata->num_row_groups(); ++k) {
            groups.push_back(k);
        }
        return groups;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::vector<int>
selectParquetRowGroupsByFilter(const std::shared_ptr<parquet::FileMetaData>& metadata,
    const ArrayOf& expression, const std::vector<int>& requestedRowGroups, bool& hasSelection)
{
    hasSelection = !requestedRowGroups.empty();
    std::vector<int> groups = baseRowGroups(metadata, requestedRowGroups);
    if (expression.isEmpty() || !hasField(expression, "kind")) {
        return groups;
    }
    hasSelection = true;
    std::vector<int> selected;
    selected.reserve(groups.size());
    for (int rowGroup : groups) {
        if (rowGroup >= 0 && rowGroup < metadata->num_row_groups()
            && matchExpression(metadata, rowGroup, expression) == MatchState::Maybe) {
            selected.push_back(rowGroup);
        }
    }
    return selected;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
