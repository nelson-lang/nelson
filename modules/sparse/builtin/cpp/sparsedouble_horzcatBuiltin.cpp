//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sparsedouble_horzcatBuiltin.hpp"
#include "HorzCatSparseDouble.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparsedouble_horzcatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);

    ArrayOfVector _argIn(argIn);
    for (size_t k = 0; k < _argIn.size(); ++k) {
        if (!_argIn[k].isDoubleClass()) {
            _argIn[k].promoteType(_argIn[k].isComplex() ? NLS_DCOMPLEX : NLS_DOUBLE);
        }
        if (!_argIn[k].isSparse()) {
            _argIn[k].makeSparse();
        }
    }

    ArrayOf res = _argIn[0];
    for (size_t k = 1; k < _argIn.size(); k++) {
        res = HorzCatSparseDouble(res, _argIn[k]);
    }
    return res;
}
//=============================================================================
