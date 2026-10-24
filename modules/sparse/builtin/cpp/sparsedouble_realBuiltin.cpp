//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sparsedouble_realBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseRealPart.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isSupportedSparseRealType(const ArrayOf& value)
{
    if (!value.isSparse()) {
        return false;
    }
    NelsonType dataClass = value.dataClass();
    return dataClass == NLS_DOUBLE || dataClass == NLS_DCOMPLEX || dataClass == NLS_LOGICAL
        || dataClass == NLS_SINGLE || dataClass == NLS_SCOMPLEX;
}
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparsedouble_realBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!isSupportedSparseRealType(argIn[0])) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse double");
    }
    retval << SparseRealPart(argIn[0]);
    return retval;
}
//=============================================================================
