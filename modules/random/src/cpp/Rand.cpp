//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Rand.hpp"
#include "Error.hpp"
#include "RandomInterface.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Rand(Evaluator* eval, Class cl)
{
    Dimensions dims(1, 1);
    return Rand(eval, dims, cl);
}
//=============================================================================
ArrayOf
Rand(Evaluator* eval, Dimensions& dims, Class cl)
{
    dims.simplify();
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::emptyConstructor(dims);
        res.promoteType(cl);
        return res;
    }
    if (eval->RandomEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    RandomInterface* randEngine = (RandomInterface*)eval->RandomEngine;
    switch (cl) {
    case NLS_SINGLE: {
        indexType nbElements = dims.getElementCount();
        single* mat
            = (single*)ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false);
        randEngine->getValuesAsSingle(mat, nbElements, dims.getColumns());
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_DOUBLE: {
        indexType nbElements = dims.getElementCount();
        double* mat
            = (double*)ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false);
        randEngine->getValuesAsDouble(mat, nbElements, dims.getColumns());
        return ArrayOf(cl, dims, mat, false);
    } break;
    default:
        Error(ERROR_TYPE_NOT_SUPPORTED);
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
