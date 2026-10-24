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
#include <algorithm>
#include <vector>
#include "i18n.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static indexType
sourceStride(const Dimensions& dimensions, indexType dim)
{
    indexType stride = 1;
    for (indexType k = 0; k < dim; ++k) {
        stride *= dimensions.dimensionLength(k);
    }
    return stride;
}
//=============================================================================
BinaryBroadcast
binaryOperatorBroadcast(const ArrayOf& A, const ArrayOf& B, const std::string& operatorName)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    indexType nDims = std::max(dimsA.length(), dimsB.length());
    nDims = std::max<indexType>(nDims, 2);
    if (nDims > MAXDIMS) {
        Error(resolveMessageTemplate("Nelson:constructors_functions:tooManyDimensionsCurrent",
                  "Too many dimensions! Current limit is")
            + " " + std::to_string(Nelson::maxDims) + ".");
    }

    std::vector<indexType> outputDimensions(static_cast<size_t>(nDims), 1);
    BinaryBroadcast info;
    info.nDims = nDims;
    for (indexType k = 0; k < nDims; ++k) {
        indexType dimA = dimsA.dimensionLength(k);
        indexType dimB = dimsB.dimensionLength(k);
        if (dimA != dimB && dimA != 1 && dimB != 1) {
            if (operatorName == "&" || operatorName == "|") {
                Error("Nelson:dimagree:sizeMismatchArgumentsValue",
                    "Size mismatch on arguments to %s", operatorName);
            }
            Error("Nelson:dimagree:sizeMismatchArgumentsArithmeticOperatorValue",
                "Size mismatch on arguments to arithmetic operator %s", operatorName);
        }
        indexType dimC = dimA == 1 ? dimB : dimA;
        outputDimensions[static_cast<size_t>(k)] = dimC;
        info.outputLengths[k] = dimC;
        info.aStrides[k] = dimA == 1 ? 0 : sourceStride(dimsA, k);
        info.bStrides[k] = dimB == 1 ? 0 : sourceStride(dimsB, k);
    }
    info.dimensions = Dimensions(outputDimensions);
    info.dimensions.simplify();
    info.nDims = info.dimensions.length();
    return info;
}
//=============================================================================
ArrayOf
binaryOperatorEmptyMatrixEmptyMatrix(
    const ArrayOf& A, const ArrayOf& B, NelsonType commonClass, const std::string& operatorName)
{
    std::vector<indexType> dimsA = A.dimensions().asVector();
    std::vector<indexType> dimsB = B.dimensions().asVector();
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
            Error("Nelson:dimagree:sizeMismatchArgumentsArithmeticOperatorValue",
                "Size mismatch on arguments to arithmetic operator %s", operatorName);
        }
    }
    Dimensions dims(dimsOut);
    ArrayOf res = ArrayOf::empty(dims);
    res.promoteType(commonClass);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
