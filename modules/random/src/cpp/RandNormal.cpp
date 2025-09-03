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
#include "i18n.hpp"
#include "RandomInterface.hpp"
#include "NelsonConfiguration.hpp"
#include "PredefinedErrorMessages.hpp"
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
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    if (randEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    switch (cl) {
    case NLS_SCOMPLEX: {
        indexType nbElements = dims.getElementCount();
        single* mat = static_cast<single*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsSingle(
            mat, nbElements * 2, dims.getColumns(), RNG_DISTRIBUTION_NORMAL);
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_SINGLE: {
        indexType nbElements = dims.getElementCount();
        single* mat = static_cast<single*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsSingle(mat, nbElements, dims.getColumns(), RNG_DISTRIBUTION_NORMAL);
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_DCOMPLEX: {
        indexType nbElements = dims.getElementCount();
        double* mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsDouble(
            mat, nbElements * 2, dims.getColumns(), RNG_DISTRIBUTION_NORMAL);
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_DOUBLE: {
        indexType nbElements = dims.getElementCount();
        double* mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsDouble(mat, nbElements, dims.getColumns(), RNG_DISTRIBUTION_NORMAL);
        return ArrayOf(cl, dims, mat, false);
    } break;
    default:
        Error(ERROR_TYPE_NOT_SUPPORTED);
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
