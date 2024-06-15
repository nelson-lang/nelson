//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__num2cell__Builtin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "nlsBuildConfig.h"
#include "NewWithException.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::__num2cell__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(nLhs);
    indexType nbElements = argIn[0].getElementCount();
    Dimensions dims = argIn[0].getDimensions();
    auto* dp = new_with_exception<ArrayOf>(nbElements, false);
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, dp);
    ArrayOf param(argIn[0]);
    if (param.isSparse()) {
        param.makeDense();
    }
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
        dp[k] = param.getValueAtIndex(k);
    }
    retval << res;
    return retval;
}
//=============================================================================
