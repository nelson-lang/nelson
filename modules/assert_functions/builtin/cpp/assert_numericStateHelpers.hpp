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
#include <cmath>
#include "assert_extraBuiltins.hpp"
#include "assert_modernHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::AssertFunctionsGateway;
//=============================================================================
static bool
allLogicalValuesEqual(ArrayOf value, logical expected)
{
    if (!value.isLogical()) {
        Error(L"Nelson:asserts:invalidArgument", L"Wrong type for argument #%d: %s expected.", (1),
            L"logical");
    }
    value.makeDense();
    const auto* data = static_cast<const logical*>(value.dataPointer());
    for (indexType k = 0; k < value.elementCount(); ++k) {
        if (data[k] != expected) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
allRealValuesSatisfy(const ArrayOf& value, bool checkFinite, bool checkNan)
{
    const auto* data = static_cast<const T*>(value.dataPointer());
    for (indexType k = 0; k < value.elementCount(); ++k) {
        double current = static_cast<double>(data[k]);
        if (checkFinite && !std::isfinite(current)) {
            return false;
        }
        if (checkNan && std::isnan(current)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
allComplexValuesSatisfy(const ArrayOf& value, bool checkFinite, bool checkNan)
{
    const auto* data = static_cast<const T*>(value.dataPointer());
    for (indexType k = 0; k < value.elementCount(); ++k) {
        double realPart = static_cast<double>(data[2 * k]);
        double imagPart = static_cast<double>(data[2 * k + 1]);
        if (checkFinite && (!std::isfinite(realPart) || !std::isfinite(imagPart))) {
            return false;
        }
        if (checkNan && (std::isnan(realPart) || std::isnan(imagPart))) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
allNumericValuesSatisfy(ArrayOf value, bool checkFinite, bool checkNan)
{
    if (!value.isNumeric() && !value.isLogical()) {
        Error(L"Nelson:asserts:invalidArgument:numericOrLogical",
            L"Numeric or logical value expected.");
    }
    if (value.isSparse()) {
        value.makeDense();
    }
    switch (value.dataClass()) {
    case NLS_DOUBLE:
        return allRealValuesSatisfy<double>(value, checkFinite, checkNan);
    case NLS_SINGLE:
        return allRealValuesSatisfy<single>(value, checkFinite, checkNan);
    case NLS_DCOMPLEX:
        return allComplexValuesSatisfy<double>(value, checkFinite, checkNan);
    case NLS_SCOMPLEX:
        return allComplexValuesSatisfy<single>(value, checkFinite, checkNan);
    default:
        return true;
    }
}
//=============================================================================
