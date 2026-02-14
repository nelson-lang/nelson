//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sparselogical_realBuiltin.hpp"
#include "Error.hpp"
#include "SparseRealPart.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparselogical_realBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isSparse()) {
        raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 1, NLS_SPARSE_LOGICAL_STR);
    }
    if (!argIn[0].isLogical()) {
        raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 1, NLS_SPARSE_LOGICAL_STR);
    }
    retval << SparseRealPart(argIn[0]);
    return retval;
}
//=============================================================================
