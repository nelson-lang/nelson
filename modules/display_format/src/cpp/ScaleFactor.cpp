//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "ScaleFactor.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline double
getScaleFactorWithAmplitude(double max_amplitude, double maxval)
{
    int commonLogarithm = (int)log10(std::abs(max_amplitude));
    if (commonLogarithm == 1) {
        return 1;
    }

    if (::pow(double(10.0), std::abs(commonLogarithm)) >= maxval) {
        return commonLogarithm;
    }
    return (double)(1.0);
}
//=============================================================================
template <class T>
static bool
ComputeScaleFactorReal(const T* ptrData, indexType count, bool isSparse, NelsonType classReal,
    bool allInteger, FormatDisplayInformation& formatInfo)
{
    double max_amplitude = 0;
    if (count == 0) {
        return false;
    }
    bool finiteElementFound = false;
    for (indexType i = 0; i < count; i++) {
        if (std::isfinite(static_cast<double>(ptrData[i])) && !finiteElementFound) {
            max_amplitude = static_cast<double>(ptrData[i]);
            finiteElementFound = true;
        }
        if ((std::isfinite(static_cast<double>(ptrData[i])))
            && (fabs((double)ptrData[i]) > fabs((double)max_amplitude))) {
            max_amplitude = static_cast<double>(ptrData[i]);
        }
    }
    if (!finiteElementFound) {
        return false;
    }
    double maxval = ::pow(double(10.0), 3);
    formatInfo.scaleFactor = getScaleFactorWithAmplitude(max_amplitude, maxval);
    return true;
}
//=============================================================================
template <class T>
static bool
ComputeScaleFactorComplex(const std::complex<T>* ptrData, indexType count, bool isSparse,
    NelsonType classReal, FormatDisplayInformation& formatInfo)
{
    double max_amplitude = 0;
    if (count == 0) {
        return false;
    }
    bool finiteElementFound = false;
    for (indexType i = 0; i < count; i++) {
        if (std::isfinite(static_cast<double>(ptrData[i].real())) && !finiteElementFound) {
            max_amplitude = static_cast<double>(ptrData[i].real());
            finiteElementFound = true;
        }
        if (std::isfinite(static_cast<double>(ptrData[i].imag())) && !finiteElementFound) {
            max_amplitude = static_cast<double>(ptrData[i].imag());
            finiteElementFound = true;
        }
        if (std::isfinite(static_cast<double>(ptrData[i].real()))
            && fabs((double)ptrData[i].real()) > fabs((double)max_amplitude)) {
            max_amplitude = static_cast<double>(ptrData[i].real());
        }
        if (std::isfinite(static_cast<double>(ptrData[i].imag()))
            && fabs((double)ptrData[i].imag()) > fabs((double)max_amplitude)) {
            max_amplitude = static_cast<double>(ptrData[i].imag());
        }
    }
    if (!finiteElementFound) {
        return false;
    }
    double maxval = ::pow(double(10.0), 2);
    formatInfo.scaleFactor = getScaleFactorWithAmplitude(max_amplitude, maxval);
    return true;
}
//=============================================================================
bool
ComputeScaleFactor(const ArrayOf& A, bool allInteger, FormatDisplayInformation& formatInfo)
{
    bool computed = false;
    switch (A.getDataClass()) {
    case NLS_DCOMPLEX: {
        std::complex<double>* ptrZ = nullptr;
        indexType nbElements;
        if (A.isSparse()) {
            Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)
                      A.getSparseDataPointer();
            ptrZ = spMat->valuePtr();
            nbElements = (indexType)spMat->nonZeros();
        } else {
            ptrZ = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
            nbElements = (indexType)A.getElementCount();
        }
        return ComputeScaleFactorComplex<double>(
            ptrZ, nbElements, A.isSparse(), NLS_DOUBLE, formatInfo);
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single>* ptrZ = nullptr;
        indexType nbElements;
        if (A.isSparse()) {
            Eigen::SparseMatrix<std::complex<single>, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<std::complex<single>, 0, signedIndexType>*)
                      A.getSparseDataPointer();
            ptrZ = spMat->valuePtr();
            nbElements = (indexType)spMat->nonZeros();
        } else {
            ptrZ = reinterpret_cast<std::complex<single>*>((single*)A.getDataPointer());
            nbElements = (indexType)A.getElementCount();
        }
        return ComputeScaleFactorComplex<single>(
            ptrZ, nbElements, A.isSparse(), NLS_SINGLE, formatInfo);
    } break;
    case NLS_DOUBLE: {
        indexType nbElements;
        double* ptr = nullptr;
        if (A.isSparse()) {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();
            ptr = spMat->valuePtr();
            nbElements = (indexType)spMat->nonZeros();
        } else {
            ptr = reinterpret_cast<double*>((double*)A.getDataPointer());
            nbElements = (indexType)A.getElementCount();
        }
        return ComputeScaleFactorReal<double>(
            ptr, nbElements, A.isSparse(), NLS_DOUBLE, allInteger, formatInfo);
    } break;
    case NLS_SINGLE: {
        indexType nbElements;
        single* ptr = nullptr;
        if (A.isSparse()) {
            Eigen::SparseMatrix<single, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<single, 0, signedIndexType>*)A.getSparseDataPointer();
            ptr = spMat->valuePtr();
            nbElements = (indexType)spMat->nonZeros();
        } else {
            ptr = reinterpret_cast<single*>((single*)A.getDataPointer());
            nbElements = (indexType)A.getElementCount();
        }
        return ComputeScaleFactorReal<single>(
            ptr, nbElements, A.isSparse(), NLS_SINGLE, allInteger, formatInfo);
    } break;
    default: {
        computed = false;
    } break;
    }
    return computed;
}
//=============================================================================
}
//=============================================================================
