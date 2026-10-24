//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <cmath>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "ValidatorsInternal.hpp"
#include "ValidatorsHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
raiseComparisonError(const ArrayOf& c, const std::wstring& id, const std::wstring& messageTemplate,
    const std::wstring& comparedMessage, int argPosition, bool asCaller);
static std::wstring
printableScalar(const ArrayOf& c);
template <typename T>
static std::wstring
printableFloatingPoint(T value);
//=============================================================================
bool
allCompare(const ArrayOf& arg, const ArrayOf& c, const std::string& operatorName)
{
    if (arg.isEmpty()) {
        return true;
    }
    ArrayOfVector params;
    params << arg;
    params << c;
    ArrayOfVector argOut = evaluateFunction(params, 1, operatorName);
    ArrayOf asVector = argOut[0];
    Dimensions dimsV(1, asVector.dimensions().elementCount());
    asVector.reshape(dimsV);
    argOut = evaluateFunction(asVector, 1, "all");
    return argOut[0].asLogicalScalar();
}
//=============================================================================
void
mustBeGreaterThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    if (allCompare(arg, c, GT_OPERATOR_STR)) {
        return;
    }
    raiseComparisonError(c, L"Nelson:validators:mustBeGreaterThan",
        resolveMessageTemplate(L"Nelson:validators:mustBeGreaterThan", L"Value must exceed %s."),
        resolveMessageTemplate(L"Nelson:validators:mustBeGreaterThanCompared",
            L"Value must exceed the compared value."),
        argPosition, asCaller);
}
//=============================================================================
void
mustBeLessThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    if (allCompare(arg, c, LT_OPERATOR_STR)) {
        return;
    }
    raiseComparisonError(c, L"Nelson:validators:mustBeLessThan",
        resolveMessageTemplate(L"Nelson:validators:mustBeLessThan", L"Value must be below %s."),
        resolveMessageTemplate(L"Nelson:validators:mustBeLessThanCompared",
            L"Value must be below the compared value."),
        argPosition, asCaller);
}
//=============================================================================
void
mustBeGreaterThanOrEqual(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    if (allCompare(arg, c, GE_OPERATOR_STR)) {
        return;
    }
    raiseComparisonError(c, L"Nelson:validators:mustBeGreaterThanOrEqual",
        resolveMessageTemplate(
            L"Nelson:validators:mustBeGreaterThanOrEqual", L"Value must be %s or greater."),
        resolveMessageTemplate(L"Nelson:validators:mustBeGreaterThanOrEqualCompared",
            L"Value must be equal to or greater than the compared value."),
        argPosition, asCaller);
}
//=============================================================================
void
mustBeLessThanOrEqual(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    if (allCompare(arg, c, LE_OPERATOR_STR)) {
        return;
    }
    raiseComparisonError(c, L"Nelson:validators:mustBeLessThanOrEqual",
        resolveMessageTemplate(
            L"Nelson:validators:mustBeLessThanOrEqual", L"Value must be %s or less."),
        resolveMessageTemplate(L"Nelson:validators:mustBeLessThanOrEqualCompared",
            L"Value must be equal to or less than the compared value."),
        argPosition, asCaller);
}
//=============================================================================
void
raiseComparisonError(const ArrayOf& c, const std::wstring& id, const std::wstring& messageTemplate,
    const std::wstring& comparedMessage, int argPosition, bool asCaller)
{
    std::wstring valueAsString = printableScalar(c);
    std::wstring msg = valueAsString.empty() ? comparedMessage
                                             : formatI18nMessage(messageTemplate, valueAsString);
    RaiseError(invalidPositionMessage(argPosition) + msg, id, asCaller);
}
//=============================================================================
std::wstring
printableScalar(const ArrayOf& c)
{
    if (!c.isScalar() || c.isComplex() || c.isSparse()) {
        return {};
    }
    switch (c.dataClass()) {
    case NLS_LOGICAL:
        return c.asLogicalScalar() ? L"true" : L"false";
    case NLS_UINT8:
        return fmt::to_wstring(ArrayOf(c).asUint8Scalar());
    case NLS_INT8:
        return fmt::to_wstring(ArrayOf(c).asInt8Scalar());
    case NLS_UINT16:
        return fmt::to_wstring(ArrayOf(c).asUint16Scalar());
    case NLS_INT16:
        return fmt::to_wstring(ArrayOf(c).asInt16Scalar());
    case NLS_UINT32:
        return fmt::to_wstring(ArrayOf(c).asUint32Scalar());
    case NLS_INT32:
        return fmt::to_wstring(ArrayOf(c).asInt32Scalar());
    case NLS_UINT64:
        return fmt::to_wstring(ArrayOf(c).asUint64Scalar());
    case NLS_INT64:
        return fmt::to_wstring(ArrayOf(c).asInt64Scalar());
    case NLS_SINGLE:
        return printableFloatingPoint(ArrayOf(c).asSingleScalar());
    case NLS_DOUBLE:
        return printableFloatingPoint(ArrayOf(c).asDoubleScalar());
    default:
        return {};
    }
}
//=============================================================================
template <typename T>
std::wstring
printableFloatingPoint(T value)
{
    if (std::isnan(value)) {
        return L"NaN";
    }
    if (std::isinf(value)) {
        return std::signbit(value) ? L"-Inf" : L"Inf";
    }
    return fmt::to_wstring(value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
