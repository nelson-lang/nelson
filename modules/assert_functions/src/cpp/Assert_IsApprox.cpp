//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "ErrorID.hpp"
#include <cmath>
#include <utility>
#include <vector>
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "Assert_IsApprox.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
formatArrayDescription(const std::wstring& label, const ArrayOf& value)
{
    std::wstring className;
    ClassName(value, className);
    return fmt::format(
        L"{0}: class {1}, size {2}", label, className, value.dimensions().toWideString());
}
//=============================================================================
static std::wstring
formatNelsonIndex(indexType linearIndex, const Dimensions& dims)
{
    if (dims.length() == 0) {
        return std::to_wstring(static_cast<unsigned long long>(linearIndex + 1));
    }
    std::wstring result = L"(";
    indexType remaining = linearIndex;
    for (indexType k = 0; k < dims.length(); ++k) {
        indexType dimLength = dims.dimensionLength(k);
        indexType subscript = dimLength == 0 ? 0 : (remaining % dimLength) + 1;
        remaining = dimLength == 0 ? 0 : remaining / dimLength;
        if (k > 0) {
            result += L", ";
        }
        result += std::to_wstring(static_cast<unsigned long long>(subscript));
    }
    result += L")";
    return result;
}
//=============================================================================
static doublecomplex
numericValueAt(const ArrayOf& value, indexType index)
{
    switch (value.dataClass()) {
    case NLS_LOGICAL:
        return static_cast<double>(static_cast<const logical*>(value.dataPointer())[index]);
    case NLS_INT8:
        return static_cast<double>(static_cast<const int8*>(value.dataPointer())[index]);
    case NLS_INT16:
        return static_cast<double>(static_cast<const int16*>(value.dataPointer())[index]);
    case NLS_INT32:
        return static_cast<double>(static_cast<const int32*>(value.dataPointer())[index]);
    case NLS_INT64:
        return static_cast<double>(static_cast<const int64*>(value.dataPointer())[index]);
    case NLS_UINT8:
        return static_cast<double>(static_cast<const uint8*>(value.dataPointer())[index]);
    case NLS_UINT16:
        return static_cast<double>(static_cast<const uint16*>(value.dataPointer())[index]);
    case NLS_UINT32:
        return static_cast<double>(static_cast<const uint32*>(value.dataPointer())[index]);
    case NLS_UINT64:
        return static_cast<double>(static_cast<const uint64*>(value.dataPointer())[index]);
    case NLS_SINGLE:
        return static_cast<double>(static_cast<const single*>(value.dataPointer())[index]);
    case NLS_DOUBLE:
        return static_cast<const double*>(value.dataPointer())[index];
    case NLS_SCOMPLEX: {
        const auto* data = static_cast<const single*>(value.dataPointer());
        return doublecomplex(
            static_cast<double>(data[2 * index]), static_cast<double>(data[2 * index + 1]));
    }
    case NLS_DCOMPLEX: {
        const auto* data = static_cast<const double*>(value.dataPointer());
        return doublecomplex(data[2 * index], data[2 * index + 1]);
    }
    default:
        return 0.;
    }
}
//=============================================================================
static bool
isApproxValue(double expected, double computed, double relativeTolerance, double absoluteTolerance)
{
    if (std::isnan(expected) || std::isnan(computed)) {
        return std::isnan(expected) && std::isnan(computed);
    }
    if (std::isinf(expected) || std::isinf(computed)) {
        return expected == computed;
    }
    double scale = std::max(std::abs(expected), std::abs(computed));
    double tolerance = std::max(absoluteTolerance, relativeTolerance * scale);
    return std::abs(expected - computed) <= tolerance;
}
//=============================================================================
static bool
isApproxValue(const doublecomplex& expected, const doublecomplex& computed,
    double relativeTolerance, double absoluteTolerance)
{
    return isApproxValue(expected.real(), computed.real(), relativeTolerance, absoluteTolerance)
        && isApproxValue(expected.imag(), computed.imag(), relativeTolerance, absoluteTolerance);
}
//=============================================================================
static std::wstring
formatNumericValue(const doublecomplex& value)
{
    if (value.imag() == 0.) {
        return fmt::format(L"{0}", value.real());
    }
    return fmt::format(L"{0}+{1}i", value.real(), value.imag());
}
//=============================================================================
static std::vector<std::pair<indexType, doublecomplex>>
sparseNumericEntries(Evaluator* eval, const ArrayOf& value)
{
    std::vector<std::pair<indexType, doublecomplex>> entries;
    if (!value.isSparse()) {
        return entries;
    }
    FunctionDef* findFunction = nullptr;
    if (!eval->getContext()->lookupFunction("find", findFunction)) {
        Error(L"Nelson:asserts:missingFunction:find", L"find function not found.");
    }
    ArrayOfVector arguments;
    arguments << value;
    ArrayOfVector outputs = findFunction->evaluateFunction(eval, arguments, 3);
    if (outputs.size() != 3 || outputs[0].dataClass() != NLS_DOUBLE
        || outputs[1].dataClass() != NLS_DOUBLE || outputs[0].isSparse() || outputs[1].isSparse()
        || outputs[2].isSparse() || !outputs[2].isNumeric()
        || outputs[0].elementCount() != outputs[2].elementCount()
        || outputs[1].elementCount() != outputs[2].elementCount()) {
        Error(L"Nelson:asserts:invalidComparisonFunction:find", L"Invalid sparse find result.");
    }
    const auto* rows = static_cast<const double*>(outputs[0].dataPointer());
    const auto* columns = static_cast<const double*>(outputs[1].dataPointer());
    entries.reserve(outputs[2].elementCount());
    for (indexType k = 0; k < outputs[2].elementCount(); ++k) {
        if (!(rows[k] >= 1 && rows[k] <= value.rows()) || rows[k] != std::floor(rows[k])
            || !(columns[k] >= 1 && columns[k] <= value.columns())
            || columns[k] != std::floor(columns[k])) {
            Error(L"Nelson:asserts:invalidComparisonFunction:find", L"Invalid sparse find result.");
        }
        indexType index = static_cast<indexType>(rows[k] - 1)
            + static_cast<indexType>(columns[k] - 1) * value.rows();
        if (!entries.empty() && index <= entries.back().first) {
            Error(L"Nelson:asserts:invalidComparisonFunction:find", L"Invalid sparse find result.");
        }
        entries.emplace_back(index, numericValueAt(outputs[2], k));
    }
    return entries;
}
//=============================================================================
static bool
findFirstDifferentValue(Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray,
    double relativeTolerance, double absoluteTolerance, indexType& firstIndex,
    std::wstring& computedValue, std::wstring& expectedValue)
{
    if (!computedArray.isNumeric() || !expectedArray.isNumeric()
        || !computedArray.dimensions().equals(expectedArray.dimensions())) {
        return false;
    }
    auto computedEntries = sparseNumericEntries(eval, computedArray);
    auto expectedEntries = sparseNumericEntries(eval, expectedArray);
    size_t c = 0;
    size_t e = 0;
    const indexType end = expectedArray.elementCount();
    for (indexType k = 0; k < end; ++k) {
        indexType nextC = c < computedEntries.size() ? computedEntries[c].first : end;
        indexType nextE = e < expectedEntries.size() ? expectedEntries[e].first : end;
        if (computedArray.isSparse() && expectedArray.isSparse()) {
            k = std::min(nextC, nextE);
            if (k == end) {
                break;
            }
        }
        doublecomplex computed = computedArray.isSparse()
            ? (nextC == k ? computedEntries[c++].second : doublecomplex(0.))
            : numericValueAt(computedArray, k);
        doublecomplex expected = expectedArray.isSparse()
            ? (nextE == k ? expectedEntries[e++].second : doublecomplex(0.))
            : numericValueAt(expectedArray, k);
        if (!isApproxValue(expected, computed, relativeTolerance, absoluteTolerance)) {
            firstIndex = k;
            expectedValue = formatNumericValue(expected);
            computedValue = formatNumericValue(computed);
            return true;
        }
    }
    return false;
}
//=============================================================================
static std::wstring
formatDetailedMessage(Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray,
    double relativeTolerance, double absoluteTolerance)
{
    std::wstring message
        = resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedComputedToo",
              L"Assertion failed: expected and computed values are too different.")
        + L" " + formatArrayDescription(L"Expected", expectedArray) + L". "
        + formatArrayDescription(L"Computed", computedArray) + L". "
        + fmt::format(L"Relative tolerance: {0}. Absolute tolerance: {1}.", relativeTolerance,
            absoluteTolerance);
    indexType firstIndex = 0;
    std::wstring computedValue;
    std::wstring expectedValue;
    if (findFirstDifferentValue(eval, computedArray, expectedArray, relativeTolerance,
            absoluteTolerance, firstIndex, computedValue, expectedValue)) {
        message += fmt::format(L" First different index: {0}, expected {1}, computed {2}.",
            formatNelsonIndex(firstIndex, expectedArray.dimensions()), expectedValue,
            computedValue);
    }
    return message;
}
//=============================================================================
bool
Assert_IsApprox(Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray,
    double precision, std::wstring& msg)
{
    return Assert_IsApprox(eval, computedArray, expectedArray, precision, 0., msg);
}
//=============================================================================
bool
Assert_IsApprox(Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray,
    double relativeTolerance, double absoluteTolerance, std::wstring& msg)
{
    bool bRes = false;
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    std::string IsApproxName = "isapprox";
    if (context->lookupFunction(IsApproxName, funcDef)) {
        if ((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION)) {
            ArrayOfVector argInCopy(3);
            argInCopy << computedArray;
            argInCopy << expectedArray;
            argInCopy << ArrayOf::doubleScalar(relativeTolerance);
            try {
                ArrayOfVector resVect = funcDef->evaluateFunction(eval, argInCopy, 1);
                if (resVect.size() != 1) {
                    Error(L"Nelson:asserts:invalidComparisonFunction:isapproxMultipleOutputs",
                        L"isapprox returns more than one output argument.");
                }
                ArrayOf r = resVect[0];
                if (r.isScalar() && r.isLogical()) {
                    bRes = r.asLogicalScalar() ? true : false;
                } else {
                    Error(L"Nelson:asserts:invalidComparisonFunction:isapproxNotLogical",
                        L"isapprox must return an logical.");
                }
            } catch (const NelsonException&) {
                throw;
            }
        }
    } else {
        Error(L"Nelson:asserts:missingFunction:isapprox", L"isapprox function not found.");
    }
    if (!bRes && absoluteTolerance > 0. && computedArray.isNumeric() && expectedArray.isNumeric()
        && computedArray.dimensions().equals(expectedArray.dimensions())) {
        indexType firstIndex = 0;
        std::wstring computedValue;
        std::wstring expectedValue;
        bRes = !findFirstDifferentValue(eval, computedArray, expectedArray, relativeTolerance,
            absoluteTolerance, firstIndex, computedValue, expectedValue);
    }
    if (!bRes) {
        msg = formatDetailedMessage(
            eval, computedArray, expectedArray, relativeTolerance, absoluteTolerance);
    } else {
        msg.clear();
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
