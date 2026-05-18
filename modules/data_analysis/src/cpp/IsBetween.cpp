//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsBetween.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
#include <algorithm>
#include <cmath>
#include <cwctype>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
enum class IntervalType
{
    CLOSED,
    OPEN,
    OPEN_LEFT,
    OPEN_RIGHT
};
//=============================================================================
struct Options
{
    IntervalType intervalType = IntervalType::CLOSED;
    bool outputTabular = false;
    bool hasDataVariables = false;
    ArrayOf dataVariables;
};
//=============================================================================
std::wstring
toLower(std::wstring value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    return value;
}
//=============================================================================
bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || (value.isStringArray() && value.isScalar());
}
//=============================================================================
std::wstring
getTextScalar(const ArrayOf& value, const std::wstring& message)
{
    if (!isTextScalar(value)) {
        Error(message);
    }
    return value.getContentAsWideString();
}
//=============================================================================
bool
parseIntervalType(const ArrayOf& value, IntervalType& intervalType)
{
    if (!isTextScalar(value)) {
        return false;
    }
    std::wstring text = toLower(value.getContentAsWideString());
    if (text == L"closed") {
        intervalType = IntervalType::CLOSED;
        return true;
    }
    if (text == L"open") {
        intervalType = IntervalType::OPEN;
        return true;
    }
    if (text == L"openleft" || text == L"closedright") {
        intervalType = IntervalType::OPEN_LEFT;
        return true;
    }
    if (text == L"openright" || text == L"closedleft") {
        intervalType = IntervalType::OPEN_RIGHT;
        return true;
    }
    return false;
}
//=============================================================================
bool
canCompareWithNumericBroadcast(
    const ArrayOf& value, const ArrayOf& lowerBound, const ArrayOf& upperBound)
{
    return value.isNumeric() && lowerBound.isNumeric() && upperBound.isNumeric()
        && !value.isSparse() && !lowerBound.isSparse() && !upperBound.isSparse();
}
//=============================================================================
bool
hasComplexOperand(const ArrayOf& value, const ArrayOf& lowerBound, const ArrayOf& upperBound)
{
    return value.isComplex() || lowerBound.isComplex() || upperBound.isComplex();
}
//=============================================================================
double
complexMagnitudeAt(const double* values, indexType index)
{
    return std::hypot(values[2 * index], values[2 * index + 1]);
}
//=============================================================================
Dimensions
broadcastDimensions(
    const Dimensions& valueDims, const Dimensions& lowerDims, const Dimensions& upperDims)
{
    indexType length
        = std::max(std::max(valueDims.getLength(), lowerDims.getLength()), upperDims.getLength());
    length = std::max<indexType>(length, 2);
    Dimensions result(length);
    for (indexType k = 0; k < length; k++) {
        indexType valueDim = valueDims.getDimensionLength(k);
        indexType lowerDim = lowerDims.getDimensionLength(k);
        indexType upperDim = upperDims.getDimensionLength(k);
        indexType resultDim;
        if (valueDim == 0 || lowerDim == 0 || upperDim == 0) {
            if (valueDim > 1 || lowerDim > 1 || upperDim > 1) {
                Error(_("Arrays have incompatible sizes."));
            }
            resultDim = 0;
        } else {
            resultDim = std::max(std::max(valueDim, lowerDim), upperDim);
            if ((valueDim != resultDim && valueDim != 1) || (lowerDim != resultDim && lowerDim != 1)
                || (upperDim != resultDim && upperDim != 1)) {
                Error(_("Arrays have incompatible sizes."));
            }
        }
        result[k] = resultDim;
    }
    result.simplify();
    return result;
}
//=============================================================================
indexType
broadcastIndex(indexType linearIndex, const Dimensions& outputDims, const Dimensions& inputDims)
{
    indexType sourceIndex = 0;
    indexType stride = 1;
    indexType remaining = linearIndex;
    indexType length = std::max(outputDims.getLength(), inputDims.getLength());
    length = std::max<indexType>(length, 2);
    for (indexType dim = 0; dim < length; dim++) {
        indexType outputLength = outputDims.getDimensionLength(dim);
        indexType inputLength = inputDims.getDimensionLength(dim);
        indexType coordinate = outputLength == 0 ? 0 : remaining % outputLength;
        remaining = outputLength == 0 ? 0 : remaining / outputLength;
        sourceIndex += (inputLength == 1 ? 0 : coordinate) * stride;
        stride *= inputLength;
    }
    return sourceIndex;
}
//=============================================================================
ArrayOf
evaluateNumericBetween(const ArrayOf& value, const ArrayOf& lowerBound, const ArrayOf& upperBound,
    IntervalType intervalType)
{
    ArrayOf valueAsDouble(value);
    ArrayOf lowerAsDouble(lowerBound);
    ArrayOf upperAsDouble(upperBound);
    bool compareMagnitude = hasComplexOperand(value, lowerBound, upperBound);
    NelsonType targetType = compareMagnitude ? NLS_DCOMPLEX : NLS_DOUBLE;
    valueAsDouble.promoteType(targetType);
    lowerAsDouble.promoteType(targetType);
    upperAsDouble.promoteType(targetType);

    Dimensions valueDims = valueAsDouble.getDimensions();
    Dimensions lowerDims = lowerAsDouble.getDimensions();
    Dimensions upperDims = upperAsDouble.getDimensions();
    Dimensions outputDims = broadcastDimensions(valueDims, lowerDims, upperDims);

    indexType elementCount = outputDims.getElementCount();
    auto* result = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, elementCount, stringVector(), false));
    auto* values = static_cast<const double*>(valueAsDouble.getDataPointer());
    auto* lowers = static_cast<const double*>(lowerAsDouble.getDataPointer());
    auto* uppers = static_cast<const double*>(upperAsDouble.getDataPointer());

    const bool openLower
        = intervalType == IntervalType::OPEN || intervalType == IntervalType::OPEN_LEFT;
    const bool openUpper
        = intervalType == IntervalType::OPEN || intervalType == IntervalType::OPEN_RIGHT;
    for (indexType k = 0; k < elementCount; k++) {
        indexType valueIndex = broadcastIndex(k, outputDims, valueDims);
        indexType lowerIndex = broadcastIndex(k, outputDims, lowerDims);
        indexType upperIndex = broadcastIndex(k, outputDims, upperDims);
        double v = compareMagnitude ? complexMagnitudeAt(values, valueIndex) : values[valueIndex];
        double lb = compareMagnitude ? complexMagnitudeAt(lowers, lowerIndex) : lowers[lowerIndex];
        double ub = compareMagnitude ? complexMagnitudeAt(uppers, upperIndex) : uppers[upperIndex];
        bool lowerOk = openLower ? (v > lb) : (v >= lb);
        bool upperOk = openUpper ? (v < ub) : (v <= ub);
        result[k] = static_cast<logical>(lowerOk && upperOk);
    }
    return ArrayOf(NLS_LOGICAL, outputDims, result);
}
//=============================================================================
ArrayOfVector
normalizeTrailingNamedArguments(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector normalized(argIn);
    if (normalized.size() > 3 && normalized.back().isStruct()) {
        FunctionDefPtr funcDef;
        if (eval->lookupFunction("namedargs2cell", funcDef)) {
            ArrayOfVector input;
            input << normalized.back();
            ArrayOfVector converted = funcDef->evaluateFunction(eval, input, 1);
            if (!converted.empty() && converted[0].isCell()) {
                normalized.pop_back();
                ArrayOfVector nv = scalarArrayOfToArrayOfVector(converted[0]);
                for (auto& value : nv) {
                    normalized.push_back(value);
                }
            }
        }
    }
    return normalized;
}
//=============================================================================
Options
parseOptions(Evaluator* eval, const ArrayOfVector& originalArgIn)
{
    ArrayOfVector argIn = normalizeTrailingNamedArguments(eval, originalArgIn);
    Options options;
    size_t index = 3;
    if (argIn.size() > index) {
        IntervalType parsed;
        if (parseIntervalType(argIn[index], parsed)) {
            options.intervalType = parsed;
            index++;
        } else if (isTextScalar(argIn[index]) && ((argIn.size() - index) % 2) != 0) {
            Error(_("Invalid interval type."));
        }
    }
    if (((argIn.size() - index) % 2) != 0) {
        Error(_("Wrong number of input arguments. Inputs must be name-value pairs."));
    }
    while (index < argIn.size()) {
        std::wstring name = toLower(getTextScalar(
            argIn[index], _W("Name must be a string scalar or character row vector.")));
        ArrayOf value = argIn[index + 1];
        if (name == L"datavariables") {
            options.hasDataVariables = true;
            options.dataVariables = value;
        } else if (name == L"outputformat") {
            std::wstring outputFormat = toLower(getTextScalar(
                value, _W("OutputFormat must be a string scalar or character row vector.")));
            if (outputFormat == L"logical") {
                options.outputTabular = false;
            } else if (outputFormat == L"tabular") {
                options.outputTabular = true;
            } else {
                Error(_("OutputFormat must be 'logical' or 'tabular'."));
            }
        } else {
            Error(_("Unknown name-value argument."));
        }
        index += 2;
    }
    return options;
}
//=============================================================================
ArrayOf
evaluateComparison(Evaluator* eval, const std::string& functionName, const ArrayOfVector& args)
{
    FunctionDefPtr funcDef;
    if (!eval->lookupFunction(functionName, funcDef)) {
        Error(_("Comparison function not found."));
    }
    ArrayOfVector result = funcDef->evaluateFunction(eval, args, 1);
    if (result.empty()) {
        Error(_("Comparison function did not return a value."));
    }
    return result[0];
}
//=============================================================================
ArrayOf
evaluateBetween(Evaluator* eval, const ArrayOf& value, const ArrayOf& lowerBound,
    const ArrayOf& upperBound, IntervalType intervalType)
{
    if (canCompareWithNumericBroadcast(value, lowerBound, upperBound)) {
        return evaluateNumericBetween(value, lowerBound, upperBound, intervalType);
    }

    ArrayOfVector lowerArgs;
    lowerArgs << value;
    lowerArgs << lowerBound;
    ArrayOf lowerMask
        = (intervalType == IntervalType::OPEN || intervalType == IntervalType::OPEN_LEFT)
        ? evaluateComparison(eval, "gt", lowerArgs)
        : evaluateComparison(eval, "ge", lowerArgs);

    ArrayOfVector upperArgs;
    upperArgs << value;
    upperArgs << upperBound;
    ArrayOf upperMask
        = (intervalType == IntervalType::OPEN || intervalType == IntervalType::OPEN_RIGHT)
        ? evaluateComparison(eval, "lt", upperArgs)
        : evaluateComparison(eval, "le", upperArgs);

    ArrayOfVector andArgs;
    andArgs << lowerMask;
    andArgs << upperMask;
    return evaluateComparison(eval, "and", andArgs);
}
//=============================================================================
std::vector<indexType>
allTableColumns(indexType width)
{
    std::vector<indexType> indices;
    indices.reserve(width);
    for (indexType k = 0; k < width; k++) {
        indices.push_back(k);
    }
    return indices;
}
//=============================================================================
std::vector<indexType>
selectedTableColumns(const ArrayOf& table, const Options& options)
{
    indexType width = table.getTableWidth();
    if (!options.hasDataVariables) {
        return allTableColumns(width);
    }

    ArrayOf selector = options.dataVariables;
    if (isTextScalar(selector) || selector.isCellArrayOfCharacterVectors()
        || selector.isStringArray()) {
        stringVector selectedNames;
        if (isTextScalar(selector)) {
            stringVector scalarName = selector.getContentAsCStringVector();
            selectedNames.push_back(scalarName[0]);
        } else {
            selectedNames = selector.getContentAsCStringVector();
        }
        stringVector variableNames = table.getTableVariableNames();
        std::vector<indexType> indices;
        for (const std::string& name : selectedNames) {
            auto it = std::find(variableNames.begin(), variableNames.end(), name);
            if (it == variableNames.end()) {
                Error(_("Unknown table variable name."));
            }
            indices.push_back(static_cast<indexType>(std::distance(variableNames.begin(), it)));
        }
        return indices;
    }

    if (selector.isLogical()) {
        if (selector.getElementCount() != width) {
            Error(_("DataVariables logical selector must match table width."));
        }
        auto* values = static_cast<const logical*>(selector.getDataPointer());
        std::vector<indexType> indices;
        for (indexType k = 0; k < width; k++) {
            if (values[k] != 0) {
                indices.push_back(k);
            }
        }
        return indices;
    }

    if (selector.isNumeric()) {
        std::vector<indexType> indices = selector.getContentAsIndexVector();
        for (indexType& index : indices) {
            if (index == 0 || index > width) {
                Error(_("DataVariables index out of bounds."));
            }
            index--;
        }
        return indices;
    }

    Error(_("Unsupported DataVariables selector."));
    return {};
}
//=============================================================================
ArrayOf
getBoundForTableColumn(const ArrayOf& bound, const std::string& variableName)
{
    if (bound.isTable()) {
        return bound.getTableColumn(variableName);
    }
    return bound;
}
//=============================================================================
ArrayOf
logicalEmpty(indexType rows, indexType columns)
{
    logical* data = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, rows * columns, stringVector(), false));
    return ArrayOf(NLS_LOGICAL, Dimensions(rows, columns), data);
}
//=============================================================================
ArrayOf
horzcatColumns(Evaluator* eval, const ArrayOfVector& columns, indexType rows)
{
    if (columns.empty()) {
        return logicalEmpty(rows, 0);
    }
    if (columns.size() == 1) {
        return columns[0];
    }
    return evaluateComparison(eval, "horzcat", columns);
}
//=============================================================================
ArrayOf
tableIsBetween(Evaluator* eval, const ArrayOf& table, const ArrayOf& lowerBound,
    const ArrayOf& upperBound, const Options& options)
{
    std::vector<indexType> selectedColumns = selectedTableColumns(table, options);
    stringVector variableNames = table.getTableVariableNames();
    stringVector selectedNames;
    ArrayOfVector resultColumns;
    selectedNames.reserve(selectedColumns.size());
    for (indexType columnIndex : selectedColumns) {
        const std::string& variableName = variableNames[columnIndex];
        ArrayOf value = table.getTableColumn(columnIndex);
        ArrayOf lb = getBoundForTableColumn(lowerBound, variableName);
        ArrayOf ub = getBoundForTableColumn(upperBound, variableName);
        resultColumns << evaluateBetween(eval, value, lb, ub, options.intervalType);
        selectedNames.push_back(variableName);
    }
    if (options.outputTabular) {
        return ArrayOf::tableConstructor(resultColumns, selectedNames, table.getTableRowNames());
    }
    return horzcatColumns(eval, resultColumns, table.getTableHeight());
}
//=============================================================================
ArrayOf
allLogical(ArrayOf value)
{
    value.promoteType(NLS_LOGICAL);
    const auto* values = static_cast<const logical*>(value.getDataPointer());
    indexType elementCount = value.getElementCount();
    bool result = true;
    for (indexType k = 0; k < elementCount; k++) {
        if (values[k] == 0) {
            result = false;
            break;
        }
    }
    return ArrayOf::logicalConstructor(result);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOf
Nelson::IsBetween(Evaluator* eval, const ArrayOfVector& argIn)
{
    Options options = parseOptions(eval, argIn);
    ArrayOf value = argIn[0];
    ArrayOf lowerBound = argIn[1];
    ArrayOf upperBound = argIn[2];
    if (value.isTable()) {
        return tableIsBetween(eval, value, lowerBound, upperBound, options);
    }
    if (options.hasDataVariables || options.outputTabular) {
        Error(_("DataVariables and OutputFormat are supported only for table inputs."));
    }
    return evaluateBetween(eval, value, lowerBound, upperBound, options.intervalType);
}
//=============================================================================
ArrayOf
Nelson::AllBetween(Evaluator* eval, const ArrayOfVector& argIn)
{
    return allLogical(IsBetween(eval, argIn));
}
//=============================================================================
