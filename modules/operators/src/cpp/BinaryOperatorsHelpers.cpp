//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BinaryOperatorsHelpers.hpp"
#include "i18n.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
binaryOperatorEmptyMatrixEmptryMatrix(
    const ArrayOf& A, const ArrayOf& B, NelsonType commonClass, const std::string& operatorName)
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
