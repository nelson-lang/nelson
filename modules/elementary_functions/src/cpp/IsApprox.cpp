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
#include <Eigen/Sparse>
#include <vector>
#include "IsApprox.hpp"
#include "RealPart.hpp"
#include "ImagPart.hpp"
#include "SparseType.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isApproxValue(double a, double b, double precision)
{
    if (std::isnan(a) || std::isnan(b)) {
        return std::isnan(a) && std::isnan(b);
    }
    if (std::isinf(a) || std::isinf(b)) {
        return a == b;
    }
    Eigen::Matrix<double, 1, 1> valueA;
    Eigen::Matrix<double, 1, 1> valueB;
    valueA(0) = a;
    valueB(0) = b;
    return valueA.isApprox(valueB, precision);
}
//=============================================================================
static bool
isApproxValue(const doublecomplex& a, const doublecomplex& b, double precision)
{
    return isApproxValue(a.real(), b.real(), precision)
        && isApproxValue(a.imag(), b.imag(), precision);
}
//=============================================================================
template <class T>
bool
IsApprox(T* ptrA, T* ptrB, const Dimensions& dims, double precision);
//=============================================================================
static bool
SparseValuesIsApprox(std::vector<double>& valuesA, std::vector<double>& valuesB, double precision)
{
    Dimensions dims(valuesA.size(), 1);
    return IsApprox<double>(valuesA.data(), valuesB.data(), dims, precision);
}
//=============================================================================
static bool
SparseValuesIsApprox(
    std::vector<doublecomplex>& valuesA, std::vector<doublecomplex>& valuesB, double precision)
{
    std::vector<double> realA;
    std::vector<double> realB;
    std::vector<double> imagA;
    std::vector<double> imagB;
    realA.reserve(valuesA.size());
    realB.reserve(valuesB.size());
    imagA.reserve(valuesA.size());
    imagB.reserve(valuesB.size());
    for (indexType k = 0; k < (indexType)valuesA.size(); ++k) {
        realA.push_back(valuesA[k].real());
        realB.push_back(valuesB[k].real());
        imagA.push_back(valuesA[k].imag());
        imagB.push_back(valuesB[k].imag());
    }
    return SparseValuesIsApprox(realA, realB, precision)
        && SparseValuesIsApprox(imagA, imagB, precision);
}
//=============================================================================
template <class T>
bool
SparseIsApprox(const ArrayOf& A, const ArrayOf& B, double precision)
{
    const auto* spmatA
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    const auto* spmatB
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(B.sparseDataPointer());
    if (spmatA->nonZeros() != spmatB->nonZeros()) {
        return false;
    }
    std::vector<T> valuesA;
    std::vector<T> valuesB;
    valuesA.reserve(spmatA->nonZeros());
    valuesB.reserve(spmatB->nonZeros());
    for (indexType k = 0; k < (indexType)spmatA->outerSize(); ++k) {
        typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator itA(*spmatA, k);
        typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator itB(*spmatB, k);
        for (; itA && itB; ++itA, ++itB) {
            if (itA.row() != itB.row() || itA.col() != itB.col()) {
                return false;
            }
            valuesA.push_back(itA.value());
            valuesB.push_back(itB.value());
        }
        if (itA || itB) {
            return false;
        }
    }
    return SparseValuesIsApprox(valuesA, valuesB, precision);
}
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
    std::vector<indexType> infA = infIndices<T>(ptrA, dims.elementCount());
    std::vector<indexType> infB = infIndices<T>(ptrB, dims.elementCount());
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
    std::vector<indexType> ninfA = ninfIndices<T>(ptrA, dims.elementCount());
    std::vector<indexType> ninfB = ninfIndices<T>(ptrB, dims.elementCount());
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
    std::vector<indexType> nanA = nanIndices<T>(ptrA, dims.elementCount());
    std::vector<indexType> nanB = nanIndices<T>(ptrB, dims.elementCount());
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
            ptrA, 1, dims.elementCount());
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
            ptrB, 1, dims.elementCount());
        bRes = matA.isApprox(matB, (T)precision);
    } else {
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
            ptrA, dims.rows(), dims.columns());
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
            ptrB, dims.rows(), dims.columns());
        bRes = matA.isApprox(matB, (T)precision);
    }
    return bRes;
}
//=============================================================================
bool
IsApprox(ArrayOf A, ArrayOf B, double precision)
{
    bool bRes = false;
    if (A.dataClass() == B.dataClass()) {
        Dimensions dimsA = A.dimensions();
        Dimensions dimsB = B.dimensions();
        if (A.isEmpty() || B.isEmpty()) {
            return dimsA.equals(dimsB);
        }
        if (!dimsA.equals(dimsB)) {
            return false;
        }
        if (A.isSparse() || B.isSparse()) {
            if (!A.isSparse() || !B.isSparse()) {
                return false;
            }
            if (A.dataClass() == NLS_DOUBLE) {
                return SparseIsApprox<double>(A, B, precision);
            }
            if (A.dataClass() == NLS_DCOMPLEX) {
                return SparseIsApprox<doublecomplex>(A, B, precision);
            }
            return false;
        }
        if (A.dataClass() == NLS_SINGLE) {
            auto* ptrA = static_cast<single*>(A.readWriteDataPointer());
            auto* ptrB = static_cast<single*>(B.readWriteDataPointer());
            bRes = IsApprox<single>(ptrA, ptrB, dimsA, precision);
        } else if (A.dataClass() == NLS_DOUBLE) {
            auto* ptrA = static_cast<double*>(A.readWriteDataPointer());
            auto* ptrB = static_cast<double*>(B.readWriteDataPointer());
            bRes = IsApprox<double>(ptrA, ptrB, dimsA, precision);
        } else if ((A.dataClass() == NLS_DCOMPLEX) || (A.dataClass() == NLS_SCOMPLEX)) {
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
