//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);

    ArrayOfVector _argIn(argIn);
    for (size_t k = 0; k < _argIn.size(); ++k) {
        if (!_argIn[k].isLogical()) {
            _argIn[k].promoteType(NLS_LOGICAL);
        }
        if (!_argIn[k].isSparse()) {
            _argIn[k].makeSparse();
        }
    }

    ArrayOf res = _argIn[0];
    for (size_t k = 1; k < _argIn.size(); k++) {
        res = VertCatSparseLogical(res, _argIn[k]);
    }
    return res;
}
//=============================================================================
