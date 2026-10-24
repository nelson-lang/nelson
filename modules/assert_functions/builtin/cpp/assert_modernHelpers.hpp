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
#ifndef FMT_HEADER_ONLY
#define FMT_HEADER_ONLY
#endif
//=============================================================================
#include <cmath>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::AssertFunctionsGateway {
//=============================================================================
inline ArrayOfVector
assertionResult(int nLhs, bool passed, const std::wstring& message)
{
    ArrayOfVector retval(nLhs);
    if (nLhs == 0) {
        if (!passed) {
            ErrorAsCaller(L"Nelson:asserts:assertionFailed", message);
        }
    } else {
        retval << ArrayOf::logicalScalar(passed);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(message);
        }
    }
    return retval;
}
//=============================================================================
inline std::wstring
formatClassAndSize(const ArrayOf& value)
{
    std::wstring className;
    ClassName(value, className);
    return fmt::format(L"class {0}, size {1}", className, value.dimensions().toWideString());
}
//=============================================================================
inline bool
indexScalar(const ArrayOf& value, indexType& result)
{
    if (!value.isNumeric() || value.isComplex() || value.isSparse() || !value.isScalar()) {
        return false;
    }
    double scalar = value.asDoubleScalar();
    if (!std::isfinite(scalar) || scalar < 0. || std::floor(scalar) != scalar) {
        return false;
    }
    result = static_cast<indexType>(scalar);
    return true;
}
//=============================================================================
inline wstringVector
stringList(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return { value.asWideString() };
    }
    if (value.isCellArrayOfCharacterVectors() || value.isStringArray()) {
        return value.asWideStringVector(false);
    }
    Error(L"Nelson:asserts:invalidArgument:stringExpected",
        L"String, character vector, string array, or cell of character vectors expected.");
    return {};
}
//=============================================================================
} // namespace Nelson::AssertFunctionsGateway
//=============================================================================
