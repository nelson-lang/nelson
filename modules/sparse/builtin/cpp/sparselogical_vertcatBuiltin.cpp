//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sparselogical_vertcatBuiltin.hpp"
#include "VertCatSparseLogical.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparselogical_vertcatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (!A.isLogical()) {
        A.promoteType(NLS_LOGICAL);
    }
    if (!A.isSparse()) {
        A.makeSparse();
    }
    if (!B.isLogical()) {
        B.promoteType(NLS_LOGICAL);
    }
    if (!B.isSparse()) {
        B.makeSparse();
    }
    return VertCatSparseLogical(A, B);
}
//=============================================================================
