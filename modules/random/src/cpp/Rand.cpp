//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Rand.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "RandomInterface.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Rand(NelsonType cl)
{
    Dimensions dims(1, 1);
    return Rand(dims, cl);
}
//=============================================================================
ArrayOf
Rand(Dimensions& dims, NelsonType cl)
{
    dims.simplify();
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(cl);
        return res;
    }
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    if (randEngine == nullptr) {
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
    }
    switch (cl) {
    case NLS_SINGLE: {
        indexType nbElements = dims.elementCount();
        single* mat = static_cast<single*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsSingle(mat, nbElements, dims.columns());
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_SCOMPLEX: {
        indexType nbElements = dims.elementCount();
        single* mat = static_cast<single*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsSingle(mat, nbElements * 2, dims.columns());
        return ArrayOf(cl, dims, mat, false);
    } break;

    case NLS_DCOMPLEX: {
        indexType nbElements = dims.elementCount();
        double* mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsDouble(mat, nbElements * 2, dims.columns());
        return ArrayOf(cl, dims, mat, false);

    } break;
    case NLS_DOUBLE: {
        indexType nbElements = dims.elementCount();
        double* mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsDouble(mat, nbElements, dims.columns());
        return ArrayOf(cl, dims, mat, false);
    } break;
    default:
        Error(L"Nelson:random:typeNotSupported", L"Type not supported.");
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
