//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "ErrorID.hpp"
#include <type_traits>
#include <algorithm>
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "Assert_IsEqual.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
valuesAreEqual(Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray)
{
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    std::string isEqualToName = "isequalto";
    if (context->lookupFunction(isEqualToName, funcDef)) {
        if ((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION)) {
            ArrayOfVector argInCopy(2);
            argInCopy << computedArray;
            argInCopy << expectedArray;
            try {
                ArrayOfVector resVect = funcDef->evaluateFunction(eval, argInCopy, 1);
                if (resVect.size() != 1) {
                    Error(L"Nelson:asserts:invalidComparisonFunction:isequaltoMultipleOutputs",
                        L"isequalto returns more than one output argument.");
                }
                ArrayOf r = resVect[0];
                if (r.isScalar() && r.isLogical()) {
                    return r.asLogicalScalar() ? true : false;
                }
                Error(L"Nelson:asserts:invalidComparisonFunction:isequaltoNotLogical",
                    L"isequalto must return an logical.");
            } catch (const NelsonException&) {
                Error(L"Nelson:asserts:unexpectedComparisonError",
                    L"isequalto returns an unexpected error.");
            }
        }
    } else {
        Error(L"Nelson:asserts:missingFunction:isequalto", L"isequalto function not found.");
    }
    return false;
}
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
static bool
sameFloatingValue(double expected, double computed)
{
    return (std::isnan(expected) && std::isnan(computed)) || expected == computed;
}
//=============================================================================
template <class T>
static std::wstring
integerToWideString(T value)
{
    if (std::is_signed<T>::value) {
        return std::to_wstring(static_cast<long long>(value));
    }
    return std::to_wstring(static_cast<unsigned long long>(value));
}
//=============================================================================
template <class T>
static bool
findFirstDifferentInteger(const ArrayOf& computedArray, const ArrayOf& expectedArray,
    indexType& firstIndex, std::wstring& computedValue, std::wstring& expectedValue)
{
    const auto* computed = static_cast<const T*>(computedArray.dataPointer());
    const auto* expected = static_cast<const T*>(expectedArray.dataPointer());
    for (indexType k = 0; k < expectedArray.elementCount(); ++k) {
        if (expected[k] != computed[k]) {
            firstIndex = k;
            expectedValue = integerToWideString(expected[k]);
            computedValue = integerToWideString(computed[k]);
            return true;
        }
    }
    return false;
}
//=============================================================================
template <class T>
static bool
findFirstDifferentFloating(const ArrayOf& computedArray, const ArrayOf& expectedArray,
    indexType& firstIndex, std::wstring& computedValue, std::wstring& expectedValue)
{
    const auto* computed = static_cast<const T*>(computedArray.dataPointer());
    const auto* expected = static_cast<const T*>(expectedArray.dataPointer());
    for (indexType k = 0; k < expectedArray.elementCount(); ++k) {
        if (!sameFloatingValue(
                static_cast<double>(expected[k]), static_cast<double>(computed[k]))) {
            firstIndex = k;
            expectedValue = fmt::format(L"{0}", expected[k]);
            computedValue = fmt::format(L"{0}", computed[k]);
            return true;
        }
    }
    return false;
}
//=============================================================================
template <class T>
static bool
findFirstDifferentComplex(const ArrayOf& computedArray, const ArrayOf& expectedArray,
    indexType& firstIndex, std::wstring& computedValue, std::wstring& expectedValue)
{
    const auto* computed = static_cast<const T*>(computedArray.dataPointer());
    const auto* expected = static_cast<const T*>(expectedArray.dataPointer());
    for (indexType k = 0; k < expectedArray.elementCount(); ++k) {
        double expectedReal = static_cast<double>(expected[2 * k]);
        double expectedImag = static_cast<double>(expected[2 * k + 1]);
        double computedReal = static_cast<double>(computed[2 * k]);
        double computedImag = static_cast<double>(computed[2 * k + 1]);
        if (!sameFloatingValue(expectedReal, computedReal)
            || !sameFloatingValue(expectedImag, computedImag)) {
            firstIndex = k;
            expectedValue = fmt::format(L"{0}+{1}i", expectedReal, expectedImag);
            computedValue = fmt::format(L"{0}+{1}i", computedReal, computedImag);
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
findFirstDifferentCell(Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray,
    std::wstring& detail)
{
    if (!computedArray.isCell() || !expectedArray.isCell()
        || !computedArray.dimensions().equals(expectedArray.dimensions())) {
        return false;
    }
    const auto* computed = static_cast<const ArrayOf*>(computedArray.dataPointer());
    const auto* expected = static_cast<const ArrayOf*>(expectedArray.dataPointer());
    for (indexType k = 0; k < expectedArray.elementCount(); ++k) {
        if (!valuesAreEqual(eval, computed[k], expected[k])) {
            detail = fmt::format(L" First different cell index: {0}.",
                formatNelsonIndex(k, expectedArray.dimensions()));
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
sameFieldNames(stringVector computedFields, stringVector expectedFields)
{
    std::sort(computedFields.begin(), computedFields.end());
    std::sort(expectedFields.begin(), expectedFields.end());
    return computedFields == expectedFields;
}
//=============================================================================
static bool
findFirstDifferentStruct(Evaluator* eval, const ArrayOf& computedArray,
    const ArrayOf& expectedArray, std::wstring& detail)
{
    if (!computedArray.isStruct() || !expectedArray.isStruct()
        || !computedArray.dimensions().equals(expectedArray.dimensions())) {
        return false;
    }
    stringVector computedFields = computedArray.fieldNames();
    stringVector expectedFields = expectedArray.fieldNames();
    if (!sameFieldNames(computedFields, expectedFields)) {
        detail = L" Struct fields differ.";
        return true;
    }
    std::sort(expectedFields.begin(), expectedFields.end());
    for (const std::string& fieldName : expectedFields) {
        ArrayOfVector computedFieldsValues = computedArray.fieldAsList(fieldName);
        ArrayOfVector expectedFieldsValues = expectedArray.fieldAsList(fieldName);
        for (indexType k = 0; k < static_cast<indexType>(expectedFieldsValues.size()); ++k) {
            if (!valuesAreEqual(eval, computedFieldsValues[k], expectedFieldsValues[k])) {
                std::wstring wideFieldName(fieldName.begin(), fieldName.end());
                detail = fmt::format(L" First different struct field: '{0}' at index {1}.",
                    wideFieldName, formatNelsonIndex(k, expectedArray.dimensions()));
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
static bool
findFirstDifferentValue(const ArrayOf& computedArray, const ArrayOf& expectedArray,
    indexType& firstIndex, std::wstring& computedValue, std::wstring& expectedValue)
{
    if (computedArray.isSparse() || expectedArray.isSparse()
        || computedArray.dataClass() != expectedArray.dataClass()
        || !computedArray.dimensions().equals(expectedArray.dimensions())) {
        return false;
    }
    switch (expectedArray.dataClass()) {
    case NLS_LOGICAL:
        return findFirstDifferentInteger<logical>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_INT8:
        return findFirstDifferentInteger<int8>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_INT16:
        return findFirstDifferentInteger<int16>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_INT32:
        return findFirstDifferentInteger<int32>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_INT64:
        return findFirstDifferentInteger<int64>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_UINT8:
        return findFirstDifferentInteger<uint8>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_UINT16:
        return findFirstDifferentInteger<uint16>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_UINT32:
        return findFirstDifferentInteger<uint32>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_UINT64:
        return findFirstDifferentInteger<uint64>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_SINGLE:
        return findFirstDifferentFloating<single>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_DOUBLE:
        return findFirstDifferentFloating<double>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_SCOMPLEX:
        return findFirstDifferentComplex<single>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    case NLS_DCOMPLEX:
        return findFirstDifferentComplex<double>(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue);
    default:
        return false;
    }
}
//=============================================================================
static std::wstring
formatDetailedMessage(Evaluator* eval, const std::wstring& base, const ArrayOf& computedArray,
    const ArrayOf& expectedArray)
{
    std::wstring message = base + L" " + formatArrayDescription(L"Expected", expectedArray) + L". "
        + formatArrayDescription(L"Computed", computedArray) + L".";
    indexType firstIndex = 0;
    std::wstring computedValue;
    std::wstring expectedValue;
    if (findFirstDifferentValue(
            computedArray, expectedArray, firstIndex, computedValue, expectedValue)) {
        message += fmt::format(L" First different index: {0}, expected {1}, computed {2}.",
            formatNelsonIndex(firstIndex, expectedArray.dimensions()), expectedValue,
            computedValue);
    } else {
        std::wstring detail;
        if (findFirstDifferentCell(eval, computedArray, expectedArray, detail)
            || findFirstDifferentStruct(eval, computedArray, expectedArray, detail)) {
            message += detail;
        }
    }
    return message;
}
//=============================================================================
static std::wstring
formatMessage(Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray)
{
    std::wstring message;
    bool doDefaultMessage = false;
    if ((computedArray.isCharacterArray()
            && (computedArray.isRowVector() || computedArray.isScalar() || computedArray.isEmpty()))
        && (expectedArray.isCharacterArray()
            && (expectedArray.isRowVector() || expectedArray.isScalar()
                || expectedArray.isEmpty()))) {
        std::wstring computed = computedArray.asWideString();
        std::wstring expected = expectedArray.asWideString();
        message = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedComputedDifferent2",
                L"Assertion failed: expected '%s' and computed '%s' values are different."),
            expected, computed);
    } else {
        if (computedArray.isScalar() && expectedArray.isScalar()) {
            if ((computedArray.dataClass() == expectedArray.dataClass())
                && (!computedArray.isSparse() && !expectedArray.isSparse())) {
                switch (expectedArray.dataClass()) {
                case NLS_LOGICAL: {
                    logical computed = computedArray.asLogicalScalar();
                    logical expected = expectedArray.asLogicalScalar();
                    message = formatI18nMessage(
                        resolveMessageTemplate(
                            L"Nelson:assert_functions:assertionFailedComputedDifferent3",
                            L"Assertion failed: expected (%s) and computed (%s) values are "
                            L"different."),
                        expected ? L"true" : L"false", computed ? L"true" : L"false");
                } break;
                case NLS_STRING_ARRAY: {
                    std::wstring computed = computedArray.asWideString();
                    std::wstring expected = expectedArray.asWideString();
                    message = formatI18nMessage(
                        resolveMessageTemplate(
                            L"Nelson:assert_functions:assertionFailedComputedDifferent",
                            L"Assertion failed: expected \"%s\" and computed \"%s\" values are "
                            L"different."),
                        expected, computed);

                } break;
                case NLS_DOUBLE: {
                    double computed = ArrayOf(computedArray).asDoubleScalar();
                    double expected = ArrayOf(expectedArray).asDoubleScalar();
                    message = formatI18nMessage(
                        resolveMessageTemplate(
                            L"Nelson:assert_functions:assertionFailedComputedDifferent3",
                            L"Assertion failed: expected (%s) and computed (%s) values are "
                            L"different."),
                        fmt::format(L"{0}", expected), fmt::format(L"{0}", computed));
                } break;
                case NLS_INT32: {
                    int32 computed = ArrayOf(computedArray).asInt32Scalar();
                    int32 expected = ArrayOf(expectedArray).asInt32Scalar();
                    message = formatI18nMessage(
                        resolveMessageTemplate(
                            L"Nelson:assert_functions:assertionFailedComputedDifferent3",
                            L"Assertion failed: expected (%s) and computed (%s) values are "
                            L"different."),
                        fmt::to_wstring(expected), fmt::to_wstring(computed));
                } break;
                case NLS_UINT64: {
                    uint64 computed = ArrayOf(computedArray).asUint64Scalar();
                    uint64 expected = ArrayOf(expectedArray).asUint64Scalar();
                    message = formatI18nMessage(
                        resolveMessageTemplate(
                            L"Nelson:assert_functions:assertionFailedComputedDifferent3",
                            L"Assertion failed: expected (%s) and computed (%s) values are "
                            L"different."),
                        fmt::to_wstring(expected), fmt::to_wstring(computed));
                } break;
                default: {
                    doDefaultMessage = true;
                } break;
                }
            } else {
                doDefaultMessage = true;
            }
        } else {
            doDefaultMessage = true;
        }
    }

    if (doDefaultMessage) {
        message = formatDetailedMessage(eval,
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedComputedDifferent4",
                L"Assertion failed: expected and computed values are different."),
            computedArray, expectedArray);
    }
    return message;
}
//=============================================================================
bool
Assert_IsEqual(
    Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray, std::wstring& msg)
{
    bool bRes = valuesAreEqual(eval, computedArray, expectedArray);
    if (!bRes) {
        msg = formatMessage(eval, computedArray, expectedArray);
    } else {
        msg.clear();
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
