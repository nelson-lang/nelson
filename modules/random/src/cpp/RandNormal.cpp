//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RandNormal.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "RandomInterface.hpp"
#include "NelsonConfiguration.hpp"
#include "Rng.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
RandNormal(NelsonType cl)
{
    Dimensions dims(1, 1);
    return RandNormal(dims, cl);
}
//=============================================================================
ArrayOf
RandNormal(Dimensions& dims, NelsonType cl)
{
    dims.simplify();
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(cl);
        return res;
    }
    if (NelsonConfiguration::getInstance()->getRandomEngine() == nullptr) {
        // Initialize the default engine on first use, so a caller that reaches
        // the random generator without the random module having been loaded
        // (e.g. statistics' normrnd) works like rand/randn rather than erroring.
        RngSetDefault();
    }
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    if (randEngine == nullptr) {
        Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
    }
    switch (cl) {
    case NLS_SCOMPLEX: {
        indexType nbElements = dims.elementCount();
        single* mat = static_cast<single*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsSingle(mat, nbElements * 2, dims.columns(), RNG_DISTRIBUTION_NORMAL);
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_SINGLE: {
        indexType nbElements = dims.elementCount();
        single* mat = static_cast<single*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsSingle(mat, nbElements, dims.columns(), RNG_DISTRIBUTION_NORMAL);
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_DCOMPLEX: {
        indexType nbElements = dims.elementCount();
        double* mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsDouble(mat, nbElements * 2, dims.columns(), RNG_DISTRIBUTION_NORMAL);
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_DOUBLE: {
        indexType nbElements = dims.elementCount();
        double* mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsDouble(mat, nbElements, dims.columns(), RNG_DISTRIBUTION_NORMAL);
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
