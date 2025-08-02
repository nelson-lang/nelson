//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "strcmpBuiltin.hpp"
#include "StringCompare.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
strcmpBuiltin(int nLhs, const ArrayOfVector& argIn, bool bCaseSensitive)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    retval << StringCompare(A, B, bCaseSensitive);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strcmpBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return ::strcmpBuiltin(nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strcmpiBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return ::strcmpBuiltin(nLhs, argIn, false);
}
//=============================================================================
