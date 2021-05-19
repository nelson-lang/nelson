//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
binaryOperatorEmptyMatrixEmptryMatrix(
    const ArrayOf& A, const ArrayOf& B, Class commonClass, const std::string& operatorName)
{
    std::vector<indexType> dimsA = A.getDimensions().getAsVector();
    std::vector<indexType> dimsB = B.getDimensions().getAsVector();
    indexType nDim = std::max(dimsA.size(), dimsB.size());
    while (dimsA.size() != nDim) {
        dimsA.push_back(1);
    }
    while (dimsB.size() != nDim) {
        dimsB.push_back(1);
    }
    std::vector<indexType> dimsOut(nDim, 0);
    for (int i = 0; i < nDim; i++) {
        indexType idx = dimsA[i];
        indexType idy = dimsB[i];
        if (idx == 1) {
            dimsOut[i] = idy;
        } else if (idy == 1 || idx == idy) {
            dimsOut[i] = idx;
        } else {
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + operatorName);
        }
    }
    Dimensions dims(dimsOut);
    ArrayOf res = ArrayOf::emptyConstructor(dims);
    res.promoteType(commonClass);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
