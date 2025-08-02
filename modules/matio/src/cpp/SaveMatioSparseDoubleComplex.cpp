//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Sparse>
#include "SaveMatioSparseDoubleComplex.hpp"
#include "matioHelpers.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioSparseDoubleComplex(const std::string& variableName, const ArrayOf& variableValue)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spmat
        = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)
              variableValue.getSparseDataPointer();
    indexType nnz = 0;
    auto nzmax = static_cast<int32>(variableValue.nzmax());
    int njc = 0;
    if (spmat) {
        nnz = spmat->nonZeros();
        njc = (int)spmat->outerSize();
    } else {
        nnz = 0;
    }

    mat_uint32_t nir = static_cast<mat_uint32_t>(nnz);
#if MATIO_VERSION > 1517
    mat_uint32_t* pI = nullptr;
#else
    mat_int32_t* pI = nullptr;
#endif
    try {
#if MATIO_VERSION > 1517
        pI = new mat_uint32_t[static_cast<mat_uint32_t>(nir)];
#else
        pI = new mat_int32_t[static_cast<mat_int32_t>(nir)];
#endif
    } catch (const std::bad_alloc&) {
        return nullptr;
    }
#if MATIO_VERSION > 1517
    mat_uint32_t* pJ = nullptr;
#else
    mat_int32_t* pJ = nullptr;
#endif
    try {
#if MATIO_VERSION > 1517
        pJ = new mat_uint32_t[static_cast<mat_uint32_t>(njc + 1)];
#else
        pJ = new mat_int32_t[static_cast<mat_int32_t>(njc + 1)];
#endif
    } catch (const std::bad_alloc&) {
        delete[] pI;
        return nullptr;
    }
    signedIndexType* pInner = nullptr;
    if (spmat) {
        pInner = spmat->innerIndexPtr();
        for (signedIndexType k = 0; k < nir; ++k) {
#if MATIO_VERSION > 1517
            pI[k] = static_cast<mat_uint32_t>(pInner[k]);
#else
            pI[k] = static_cast<mat_int32_t>(pInner[k]);
#endif
        }
    }
    signedIndexType* pOuter = nullptr;
    if (spmat) {
        pOuter = spmat->outerIndexPtr();
        for (signedIndexType k = 0; k < njc; ++k) {
#if MATIO_VERSION > 1517
            pJ[k] = static_cast<mat_uint32_t>(pOuter[k]);
#else
            pJ[k] = static_cast<mat_int32_t>(pOuter[k]);
#endif
        }
        pJ[njc] = static_cast<mat_uint32_t>(nnz);
    }

    double* realptr = nullptr;
    try {
        realptr = new double[nnz];
    } catch (const std::bad_alloc&) {
        delete[] pI;
        delete[] pJ;
        return nullptr;
    }
    double* imagptr = nullptr;
    try {
        imagptr = new double[nnz];
    } catch (const std::bad_alloc&) {
        delete[] pI;
        delete[] pJ;
        delete[] realptr;
        return nullptr;
    }
    mat_complex_split_t z = { nullptr, nullptr };
    if (spmat) {
        doublecomplex* cplx = spmat->valuePtr();
        for (indexType k = 0; k < nnz; ++k) {
            imagptr[k] = cplx[k].imag();
            realptr[k] = cplx[k].real();
        }
    }
    z.Im = imagptr;
    z.Re = realptr;
    mat_sparse_t* sparse = nullptr;
    try {
        sparse = new mat_sparse_t[1];
    } catch (const std::bad_alloc&) {
        delete[] pI;
        delete[] pJ;
        delete[] realptr;
        delete[] imagptr;
        return nullptr;
    }

    sparse->nzmax = nzmax;
    sparse->nir = nir;
#if MATIO_VERSION > 1517
    sparse->ir = pI;
#else
    sparse->ir = (mat_int32_t*)pI;
#endif
    sparse->njc = njc + 1;
    sparse->jc = pJ;
    sparse->ndata = (int)nnz;
    sparse->data = &z;

    matvar_t* matVariableNoCopy = Mat_VarCreate(variableName.c_str(), MAT_C_SPARSE, MAT_T_DOUBLE,
        (int)rank, dims, sparse, MAT_F_COMPLEX | MAT_F_DONT_COPY_DATA);
    if (matVariableNoCopy == nullptr) {
        delete[] dims;
        delete[] pI;
        delete[] pJ;
        delete[] realptr;
        delete[] imagptr;
        delete[] sparse;
        return matVariableNoCopy;
    }
    matvar_t* matVariable = Mat_VarCreate(variableName.c_str(), MAT_C_SPARSE, MAT_T_DOUBLE,
        (int)rank, dims, matVariableNoCopy->data, 0 | MAT_F_COMPLEX);
    Mat_VarFree(matVariableNoCopy);
    delete[] dims;
    delete[] pI;
    delete[] pJ;
    delete[] realptr;
    delete[] imagptr;
    delete[] sparse;
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
