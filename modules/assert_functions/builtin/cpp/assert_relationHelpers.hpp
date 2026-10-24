//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "assert_extraBuiltins.hpp"
#include "ErrorID.hpp"
#include "assert_modernHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::AssertFunctionsGateway;
//=============================================================================
static bool
prepareRealNumeric(ArrayOf& value)
{
    if ((!value.isNumeric() && !value.isLogical()) || value.isComplex()) {
        return false;
    }
    if (value.isSparse()) {
        value.makeDense();
    }
    return true;
}
//=============================================================================
static double
realValueAt(const ArrayOf& value, indexType index)
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
    default:
        return 0.;
    }
}
//=============================================================================
static bool
compatibleBinarySize(const ArrayOf& left, const ArrayOf& right)
{
    return left.dimensions().equals(right.dimensions()) || left.isScalar() || right.isScalar();
}
//=============================================================================
static bool
relationCheck(ArrayOf left, ArrayOf right, const std::wstring& name,
    const std::wstring& relationText, bool (*predicate)(double, double), std::wstring& msg)
{
    if (!prepareRealNumeric(left) || !prepareRealNumeric(right)) {
        Error(L"Nelson:asserts:invalidArgument:realNumericOrLogical",
            L"Real numeric or logical values expected.");
    }
    if (!compatibleBinarySize(left, right)) {
        msg = formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:assert_functions:assertionFailedRequiresCompatible",
                L"Assertion failed: %s requires compatible sizes. Computed %s. Expected %s."),
            name, formatClassAndSize(left), formatClassAndSize(right));
        return false;
    }
    indexType count = left.isScalar() ? right.elementCount() : left.elementCount();
    for (indexType k = 0; k < count; ++k) {
        double l = realValueAt(left, left.isScalar() ? 0 : k);
        double r = realValueAt(right, right.isScalar() ? 0 : k);
        if (!predicate(l, r)) {
            msg = formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:assert_functions:assertionFailedFirstMismatch",
                                        L"Assertion failed: %s expected %s. First mismatch: "
                                        L"computed %s, expected %s. Computed %s. Expected %s."),
                name, relationText, fmt::format(L"{0}", l), fmt::format(L"{0}", r),
                formatClassAndSize(left), formatClassAndSize(right));
            return false;
        }
    }
    msg.clear();
    return true;
}
//=============================================================================
static bool
lt(double left, double right)
{
    return left < right;
}
//=============================================================================
static bool
le(double left, double right)
{
    return left <= right;
}
//=============================================================================
static bool
gt(double left, double right)
{
    return left > right;
}
//=============================================================================
static bool
ge(double left, double right)
{
    return left >= right;
}
//=============================================================================
