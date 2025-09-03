//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RandInteger.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "RandomInterface.hpp"
#include "NelsonConfiguration.hpp"
#include "PredefinedErrorMessages.hpp"
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
RandInteger(indexType imin, indexType imax, Dimensions& dims, NelsonType cl)
{
    dims.simplify();
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::emptyConstructor(dims);
        res.promoteType(cl);
        return res;
    }
    auto* randEngine
        = static_cast<RandomInterface*>(NelsonConfiguration::getInstance()->getRandomEngine());
    if (randEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }

    indexType nbElements = dims.getElementCount();
    randEngine->setMinMaxUniformIntDistribution((int)imin, (int)imax);

    switch (cl) {
    case NLS_SINGLE: {
        single* mat = static_cast<single*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsSingle(mat, nbElements, 1, RNG_DISTRIBUTION_UNIFORM_INT);
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_DOUBLE:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_LOGICAL: {
        double* mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsDouble(mat, nbElements, 1, RNG_DISTRIBUTION_UNIFORM_INT);
        ArrayOf res = ArrayOf(NLS_DOUBLE, dims, mat, false);
        res.promoteType(cl);
        return res;
    } break;
    default:
        Error(ERROR_TYPE_NOT_SUPPORTED);
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
