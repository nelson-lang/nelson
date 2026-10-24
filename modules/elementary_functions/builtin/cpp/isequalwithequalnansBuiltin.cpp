//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "isequalwithequalnansBuiltin.hpp"
#include "isequalnBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
bool
isFastDoubleArray(const ArrayOf& value)
{
    return value.isDoubleType(true) && !value.isSparse() && !value.isComplex();
}
//=============================================================================
bool
sameDoubleWithEqualNaNs(const ArrayOf& reference, const ArrayOf& value)
{
    if (!reference.dimensions().equals(value.dimensions())) {
        return false;
    }
    const double* refPtr = static_cast<const double*>(reference.dataPointer());
    const double* valPtr = static_cast<const double*>(value.dataPointer());
    indexType len = reference.elementCount();
    for (indexType k = 0; k < len; ++k) {
        if (refPtr[k] == valPtr[k]) {
            continue;
        }
        if (!std::isnan(refPtr[k]) || !std::isnan(valPtr[k])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
canUseFastDoublePath(const ArrayOfVector& args)
{
    for (const ArrayOf& arg : args) {
        if (!isFastDoubleArray(arg)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequalwithequalnansBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2);
    if (canUseFastDoublePath(argIn)) {
        const ArrayOf& reference = argIn[0];
        for (size_t k = 1; k < argIn.size(); ++k) {
            if (!sameDoubleWithEqualNaNs(reference, argIn[k])) {
                return ArrayOf::logicalScalar(false);
            }
        }
        return ArrayOf::logicalScalar(true);
    }
    return isequalnBuiltin(eval, nLhs, argIn);
}
//=============================================================================
