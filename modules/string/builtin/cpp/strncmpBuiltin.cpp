//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "strncmpBuiltin.hpp"
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
    indexType len = C.getContentAsScalarIndex(false);
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
