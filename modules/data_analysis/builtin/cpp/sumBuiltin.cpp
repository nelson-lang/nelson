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
#include <vector>
#include "sumBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Sum.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "sumProdOptionsHelpers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
reduceOverDimensions(
    const ArrayOf& X, const std::vector<indexType>& dims, const std::wstring& strtype, bool withnan)
{
    // sum over several dimensions == successive single-dimension reductions.
    ArrayOf acc(X);
    for (indexType dm : dims) {
        acc = Sum(acc, dm, strtype, withnan);
    }
    return acc;
}
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::sumBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct() || argIn[0].isClassType()) {
        OverloadRequired("sum");
    }
    ArrayOf X = argIn[0];
    SumProdOptions options = parseSumProdOptions(argIn);
    if (!options.vecDims.empty()) {
        retval << reduceOverDimensions(X, options.vecDims, options.outType, options.withNaN);
    } else {
        retval << Sum(X, options.dim, options.outType, options.withNaN);
    }
    return retval;
}
//=============================================================================
