//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <vector>
#include "IsApprox.hpp"
#include "RealPart.hpp"
#include "ImagPart.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
std::vector<indexType>
nanIndices(T* values, indexType nbElements)
{
    std::vector<indexType> res;
    for (indexType k = 0; k < nbElements; k++) {
        if (std::isnan(values[k])) {
            res.push_back(k);
        }
    }
    return res;
}
//=============================================================================
template <class T>
std::vector<indexType>
infIndices(T* values, indexType nbElements)
{
    std::vector<indexType> res;
    for (indexType k = 0; k < nbElements; k++) {
        if (std::isinf(values[k]) && (values[k] > 0)) {
            res.push_back(k);
        }
    }
    return res;
}
//=============================================================================
template <class T>
std::vector<indexType>
ninfIndices(T* values, indexType nbElements)
{
    std::vector<indexType> res;
    for (indexType k = 0; k < nbElements; k++) {
        if (std::isinf(values[k]) && (values[k] < 0)) {
            res.push_back(k);
        }
    }
    return res;
}
//=============================================================================
template <class T>
bool
IsApprox(T* ptrA, T* ptrB, const Dimensions& dims, double precision)
{
    bool bRes = false;
    std::vector<indexType> infA = infIndices<T>(ptrA, dims.getElementCount());
    std::vector<indexType> infB = infIndices<T>(ptrB, dims.getElementCount());
    if (infA.size() != infB.size()) {
        return false;
    }
    for (indexType k = 0; k < (indexType)infA.size(); k++) {
        if (infA[k] != infB[k]) {
            return false;
        }
        ptrA[infA[k]] = 0;
        ptrB[infB[k]] = 0;
    }
    infA.clear();
    infB.clear();
    std::vector<indexType> ninfA = ninfIndices<T>(ptrA, dims.getElementCount());
    std::vector<indexType> ninfB = ninfIndices<T>(ptrB, dims.getElementCount());
    if (ninfA.size() != ninfB.size()) {
        return false;
    }
    for (size_t k = 0; k < ninfA.size(); k++) {
        if (ninfA[k] != ninfB[k]) {
            return false;
        }
        ptrA[ninfA[k]] = 0;
        ptrB[ninfB[k]] = 0;
    }
    ninfA.clear();
    ninfB.clear();
    std::vector<indexType> nanA = nanIndices<T>(ptrA, dims.getElementCount());
    std::vector<indexType> nanB = nanIndices<T>(ptrB, dims.getElementCount());
    if (nanA.size() != nanB.size()) {
        return false;
    }
    for (size_t k = 0; k < nanA.size(); k++) {
        if (nanA[k] != nanB[k]) {
            return false;
        }
        ptrA[nanA[k]] = 0;
        ptrB[nanB[k]] = 0;
    }
    nanA.clear();
    nanB.clear();
    if (!dims.is2D()) {
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
            ptrA, 1, dims.getElementCount());
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
            ptrB, 1, dims.getElementCount());
        bRes = matA.isApprox(matB, (T)precision);
    } else {
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
            ptrA, dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
            ptrB, dims.getRows(), dims.getColumns());
        bRes = matA.isApprox(matB, (T)precision);
    }
    return bRes;
}
//=============================================================================
bool
IsApprox(ArrayOf A, ArrayOf B, double precision)
{
    bool bRes = false;
    if (A.getDataClass() == B.getDataClass()) {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (A.isEmpty() || B.isEmpty()) {
            return dimsA.equals(dimsB);
        }
        if (!dimsA.equals(dimsB)) {
            return false;
        }
        if (A.getDataClass() == NLS_SINGLE) {
            auto* ptrA = static_cast<single*>(A.getReadWriteDataPointer());
            auto* ptrB = static_cast<single*>(B.getReadWriteDataPointer());
            bRes = IsApprox<single>(ptrA, ptrB, dimsA, precision);
        } else if (A.getDataClass() == NLS_DOUBLE) {
            auto* ptrA = static_cast<double*>(A.getReadWriteDataPointer());
            auto* ptrB = static_cast<double*>(B.getReadWriteDataPointer());
            bRes = IsApprox<double>(ptrA, ptrB, dimsA, precision);
        } else if ((A.getDataClass() == NLS_DCOMPLEX) || (A.getDataClass() == NLS_SCOMPLEX)) {
            ArrayOf realpartA = RealPart(A);
            ArrayOf realpartB = RealPart(B);
            if (IsApprox(realpartA, realpartB, precision)) {
                ArrayOf imagpartA = ImagPart(A);
                ArrayOf imagpartB = ImagPart(B);
                bRes = IsApprox(imagpartA, imagpartB, precision);
            }
        }
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
