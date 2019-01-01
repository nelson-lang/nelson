//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "RandNormal.hpp"
#include "Error.hpp"
#include "RandomInterface.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
RandNormal(Evaluator* eval, Class cl)
{
    Dimensions dims(1, 1);
    return RandNormal(eval, dims, cl);
}
//=============================================================================
ArrayOf
RandNormal(Evaluator* eval, Dimensions& dims, Class cl)
{
    dims.simplify();
    if (eval->RandomEngine == nullptr) {
        Error(_W("random engine not initialized."));
    }
    auto* randEngine = static_cast<RandomInterface*>(eval->RandomEngine);
    switch (cl) {
    case NLS_SINGLE: {
        indexType nbElements = dims.getElementCount();
        single* mat = static_cast<single*>(
            ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
        randEngine->getValuesAsSingle(mat, nbElements, dims.getColumns(), RNG_DISTRIBUTION_NORMAL);
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
    return ArrayOf();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
