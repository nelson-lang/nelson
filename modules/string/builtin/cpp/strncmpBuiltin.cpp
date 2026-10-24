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
#include "strncmpBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Error.hpp"
#include "StringCompare.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
strncmpBuiltin(int nLhs, const ArrayOfVector& argIn, bool bCaseSensitive)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 3, 3);
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    ArrayOf C = argIn[2];
    if (!C.isNumeric() || !C.isScalar()) {
        Error(L"Nelson:string:strncmp:invalidCount", L"Third argument must be a numeric scalar.");
    }
    double count = C.asDoubleScalar();
    if (std::isnan(count)) {
        Error(L"Nelson:string:strncmp:invalidCount", L"Third argument must be a numeric scalar.");
    }
    // a count below one compares nothing (always equal), an infinite count
    // compares the whole strings, a fractional count is rounded down
    indexType len = STRNCMP_ZERO_LENGTH;
    if (std::isinf(count) && count > 0) {
        len = 0;
    } else if (count >= 1) {
        len = static_cast<indexType>(std::floor(count));
    }
    retval << StringCompare(A, B, bCaseSensitive, len);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strncmpBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return ::strncmpBuiltin(nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strncmpiBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return ::strncmpBuiltin(nLhs, argIn, false);
}
//=============================================================================
