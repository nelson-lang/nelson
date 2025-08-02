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
#include "LeastSquareSolver.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];
//=============================================================================
template <class T>
ArrayOf
solveLeastSquareReal(NelsonType destinationClass, const ArrayOf& matA, const ArrayOf& matB,
    std::wstring& warningId, std::string& warningMessage)
{
    ArrayOf res;

    indexType m = matA.getDimensionLength(0);
    indexType n = matA.getDimensionLength(1);
    indexType k = matB.getDimensionLength(1);

    Dimensions outDim(n, k);
    warningId.clear();
    warningMessage.clear();

    Dimensions dimsA = matA.getDimensions();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matAz(
        (T*)matA.getDataPointer(), dimsA.getRows(), dimsA.getColumns());

    Dimensions dimsB = matB.getDimensions();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matBz(
        (T*)matB.getDataPointer(), dimsB.getRows(), dimsB.getColumns());

    T* ptrC = (T*)ArrayOf::allocateArrayOf(destinationClass, n * k);
    res = ArrayOf(destinationClass, outDim, ptrC);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matCz(
        ptrC, outDim.getRows(), outDim.getColumns());

    matCz = matAz.colPivHouseholderQr().solve(matBz);
    double rank = (double)matAz.colPivHouseholderQr().rank();

    bool doRankWarning = false;
    if (m > n) {
        if (rank < n) {
            doRankWarning = true;
        }
    } else if (rank < m) {
        doRankWarning = true;
    }
    if (doRankWarning) {
        snprintf(msgBuffer, MSGBUFLEN, "%s RANK = %d",
            _("Matrix is rank deficient to machine precision:").c_str(), (int)rank);
        warningMessage = msgBuffer;
        warningId = WARNING_RANK_DEFICIENT_MATRIX;
    }
    return res;
}
//=============================================================================
ArrayOf
solveLeastSquareDouble(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    return solveLeastSquareReal<double>(NLS_DOUBLE, matA, matB, warningId, warningMessage);
}
//=============================================================================
ArrayOf
solveLeastSquareSingle(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    return solveLeastSquareReal<single>(NLS_SINGLE, matA, matB, warningId, warningMessage);
}
//=============================================================================
template <class T>
ArrayOf
solveLeastSquareComplex(NelsonType destinationClass, const ArrayOf& matA, const ArrayOf& matB,
    std::wstring& warningId, std::string& warningMessage)
{
    ArrayOf res;

    indexType m = matA.getDimensionLength(0);
    indexType n = matA.getDimensionLength(1);
    indexType k = matB.getDimensionLength(1);

    Dimensions outDim(n, k);
    warningId.clear();
    warningMessage.clear();

    Dimensions dimsA = matA.getDimensions();
    std::complex<T>* ptrAz = reinterpret_cast<std::complex<T>*>((T*)matA.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matAz(
        ptrAz, dimsA.getRows(), dimsA.getColumns());

    Dimensions dimsB = matB.getDimensions();

    std::complex<T>* ptrBz = reinterpret_cast<std::complex<T>*>((T*)matB.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matBz(
        ptrBz, dimsB.getRows(), dimsB.getColumns());

    T* ptrC = (T*)ArrayOf::allocateArrayOf(destinationClass, n * k);
    res = ArrayOf(destinationClass, outDim, ptrC);
    std::complex<T>* ptrCz = reinterpret_cast<std::complex<T>*>((T*)ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matCz(
        ptrCz, outDim.getRows(), outDim.getColumns());

    matCz = matAz.colPivHouseholderQr().solve(matBz);
    double rank = (double)matAz.colPivHouseholderQr().rank();

    bool doRankWarning = false;
    if (m > n) {
        if (rank < n) {
            doRankWarning = true;
        }
    } else if (rank < m) {
        doRankWarning = true;
    }
    if (doRankWarning) {
        snprintf(msgBuffer, MSGBUFLEN, "%s RANK = %d",
            _("Matrix is rank deficient to machine precision:").c_str(), (int)rank);
        warningMessage = msgBuffer;
        warningId = WARNING_RANK_DEFICIENT_MATRIX;
    }
    return res;
}
//=============================================================================
ArrayOf
solveLeastSquareDoubleComplex(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    return solveLeastSquareComplex<double>(NLS_DCOMPLEX, matA, matB, warningId, warningMessage);
}
//=============================================================================
ArrayOf
solveLeastSquareSingleComplex(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    return solveLeastSquareComplex<single>(NLS_SCOMPLEX, matA, matB, warningId, warningMessage);
}
//=============================================================================
}
//=============================================================================
