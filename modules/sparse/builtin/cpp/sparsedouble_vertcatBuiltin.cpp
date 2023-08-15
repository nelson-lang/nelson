//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sparsedouble_vertcatBuiltin.hpp"
#include "VertCatSparseDouble.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparsedouble_vertcatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (!A.isDoubleClass()) {
        A.promoteType(A.isComplex() ? NLS_DCOMPLEX : NLS_DOUBLE);
    }
    if (!A.isSparse()) {
        A.makeSparse();
    }
    if (!B.isDoubleClass()) {
        B.promoteType(B.isComplex() ? NLS_DCOMPLEX : NLS_DOUBLE);
    }
    if (!B.isSparse()) {
        B.makeSparse();
    }
    return VertCatSparseDouble(A, B);
}
//=============================================================================
