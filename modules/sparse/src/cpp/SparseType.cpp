//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <cmath>
#include <type_traits>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "SparseType.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NewWithException.hpp"
//=============================================================================
template <class T>
void*
Eigen_EyeSparseMatrixConstructor(indexType rows, indexType cols)
{
    if (cols * rows) {
        Eigen::SparseMatrix<T, 0, signedIndexType>* spMat;
        try {
            spMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(rows, cols);
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        spMat->setIdentity();
        spMat->finalize();
        spMat->makeCompressed();
        spMat->data().squeeze();
        return (void*)spMat;
    }
    return nullptr;
}
//=============================================================================
void*
Eigen_EyeSparseMatrixConstructor(NelsonType dclass, indexType rows, indexType cols)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_EyeSparseMatrixConstructor<logical>(rows, cols);
    } break;
    case NLS_DOUBLE: {
        return Eigen_EyeSparseMatrixConstructor<double>(rows, cols);
    } break;
    case NLS_SINGLE: {
        return Eigen_EyeSparseMatrixConstructor<single>(rows, cols);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_EyeSparseMatrixConstructor<doublecomplex>(rows, cols);
    } break;
    case NLS_SCOMPLEX: {
        return Eigen_EyeSparseMatrixConstructor<singlecomplex>(rows, cols);
    } break;
    default:
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
            L"EyeSparseMatrixConstructor");
    }
    return nullptr;
}
//=============================================================================
void*
Eigen_LogicalSparseMatrixConstructor(indexType rows, indexType cols, bool bMotif)
{
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat;
    try {
        spMat = new Eigen::SparseMatrix<logical, 0, signedIndexType>(rows, cols);
    } catch (const std::bad_alloc&) {
        spMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    if (bMotif) {
        for (indexType i = 0; i < rows; i++) {
            for (indexType j = 0; j < cols; j++) {
                spMat->coeffRef(i, j) = bMotif;
            }
        }
    }
    spMat->finalize();
    spMat->makeCompressed();
    spMat->data().squeeze();
    return (void*)spMat;
}
//=============================================================================
template <class T>
void
Eigen_DeleteSparseMatrix(void** cp)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)*cp;
    if (spMat) {
        delete spMat;
    }
    spMat = nullptr;
    *cp = nullptr;
}
//=============================================================================
void
Eigen_DeleteSparseMatrix(NelsonType dclass, indexType rows, indexType cols, void** cp)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        Eigen_DeleteSparseMatrix<logical>(cp);
    } break;
    case NLS_DOUBLE: {
        Eigen_DeleteSparseMatrix<double>(cp);
    } break;
    case NLS_SINGLE: {
        Eigen_DeleteSparseMatrix<single>(cp);
    } break;
    case NLS_DCOMPLEX: {
        Eigen_DeleteSparseMatrix<doublecomplex>(cp);
    } break;
    case NLS_SCOMPLEX: {
        Eigen_DeleteSparseMatrix<singlecomplex>(cp);
    } break;
    default: {
    } break;
    }
}
//=============================================================================
template <class T>
void*
Eigen_MakeDenseArrayOf(indexType rows, indexType cols, const void* cp)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)cp;
    T* pMat = nullptr;
    if (rows * cols) {
        try {
            pMat = new T[rows * cols];
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA((T*)pMat, rows, cols);
        matA = spMat->toDense();
    }
    return pMat;
}
//=============================================================================
void*
Eigen_MakeDenseArrayOf(NelsonType dclass, indexType rows, indexType cols, const void* cp)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_MakeDenseArrayOf<logical>(rows, cols, cp);
    } break;
    case NLS_DOUBLE: {
        return Eigen_MakeDenseArrayOf<double>(rows, cols, cp);
    } break;
    case NLS_SINGLE: {
        return Eigen_MakeDenseArrayOf<single>(rows, cols, cp);
    } break;
    case NLS_DCOMPLEX: {
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)cp;
        double* pMat = nullptr;
        if (rows * cols) {
            doublecomplex* pzMat = nullptr;
            try {
                pMat = new double[spMat->rows() * spMat->cols() * 2];
                pzMat = reinterpret_cast<doublecomplex*>(pMat);
            } catch (const std::bad_alloc&) {
                spMat = nullptr;
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            Eigen::Map<Eigen::Matrix<doublecomplex, Eigen::Dynamic, Eigen::Dynamic>> matA(
                (doublecomplex*)pzMat, spMat->rows(), spMat->cols());
            matA = spMat->toDense();
        }
        return pMat;
    } break;
    case NLS_SCOMPLEX: {
        Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>*)cp;
        single* pMat = nullptr;
        if (rows * cols) {
            singlecomplex* pzMat = nullptr;
            try {
                pMat = new single[spMat->rows() * spMat->cols() * 2];
                pzMat = reinterpret_cast<singlecomplex*>(pMat);
            } catch (const std::bad_alloc&) {
                spMat = nullptr;
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            Eigen::Map<Eigen::Matrix<singlecomplex, Eigen::Dynamic, Eigen::Dynamic>> matA(
                pzMat, spMat->rows(), spMat->cols());
            matA = spMat->toDense();
        }
        return pMat;
    } break;
    default:
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"MakeDenseArrayOf");
    }
    return nullptr;
}
//=============================================================================
template <class T>
void*
Eigen_MakeSparseArrayOf(indexType rows, indexType cols, const void* cp)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA((T*)cp, rows, cols);
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat;
    try {
        spMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(matA.sparseView());
    } catch (const std::bad_alloc&) {
        spMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    spMat->makeCompressed();
    spMat->data().squeeze();
    return (void*)spMat;
}
//=============================================================================
void*
Eigen_MakeSparseArrayOf(NelsonType dclass, indexType rows, indexType cols, const void* cp)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_MakeSparseArrayOf<logical>(rows, cols, cp);
    } break;
    case NLS_DOUBLE: {
        return Eigen_MakeSparseArrayOf<double>(rows, cols, cp);
    } break;
    case NLS_SINGLE: {
        return Eigen_MakeSparseArrayOf<single>(rows, cols, cp);
    } break;
    case NLS_DCOMPLEX: {
        auto* Az = reinterpret_cast<doublecomplex*>((double*)cp);
        Eigen::Map<Eigen::Matrix<doublecomplex, Eigen::Dynamic, Eigen::Dynamic>> matA(
            Az, rows, cols);
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat;
        try {
            spMat = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(matA.sparseView());
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        spMat->makeCompressed();
        spMat->data().squeeze();
        return (void*)spMat;
    } break;
    case NLS_SCOMPLEX: {
        auto* Az = reinterpret_cast<singlecomplex*>((single*)cp);
        Eigen::Map<Eigen::Matrix<singlecomplex, Eigen::Dynamic, Eigen::Dynamic>> matA(
            Az, rows, cols);
        Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>* spMat;
        try {
            spMat = new Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>(matA.sparseView());
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        spMat->makeCompressed();
        spMat->data().squeeze();
        return (void*)spMat;
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"MakeSparseArrayOf");
    }
    }
    return nullptr;
}
//=============================================================================
template <class T>
void*
Eigen_CopySparseMatrix(indexType rows, indexType cols, const void* cp)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* copiedpMat = nullptr;
    if (cp != nullptr) {
        Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<T, 0, signedIndexType>*)cp;
        try {
            copiedpMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(*spMat);
        } catch (const std::bad_alloc&) {
            copiedpMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        copiedpMat->makeCompressed();
        copiedpMat->data().squeeze();
    }
    return (void*)copiedpMat;
}
//=============================================================================
void*
Eigen_CopySparseMatrix(NelsonType dclass, indexType rows, indexType cols, const void* cp)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_CopySparseMatrix<logical>(rows, cols, cp);
    } break;
    case NLS_DOUBLE: {
        return Eigen_CopySparseMatrix<double>(rows, cols, cp);
    } break;
    case NLS_SINGLE: {
        return Eigen_CopySparseMatrix<single>(rows, cols, cp);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_CopySparseMatrix<doublecomplex>(rows, cols, cp);
    } break;
    case NLS_SCOMPLEX: {
        return Eigen_CopySparseMatrix<singlecomplex>(rows, cols, cp);
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"CopySparseMatrix");
    }
    }
    return nullptr;
}
//=============================================================================
template <class T>
indexType
Eigen_CountNonzeros(const void* cp)
{
    if (cp == nullptr) {
        Error(L"Nelson:sparse:invalidSparse", L"Invalid sparse.");
    }
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)cp;
    if (spMat) {
        return spMat->nonZeros();
    }
    return 0;
}
//=============================================================================
template <class T>
indexType
Eigen_CountNonzerosMax(const void* cp)
{
    if (cp) {
        Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<T, 0, signedIndexType>*)cp;
        return spMat->data().allocatedSize();
    }
    return 1;
}
//=============================================================================
indexType
Eigen_CountNonzeros(NelsonType dclass, indexType rows, indexType cols, const void* cp)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_CountNonzeros<logical>(cp);
    } break;
    case NLS_DOUBLE: {
        return Eigen_CountNonzeros<double>(cp);
    } break;
    case NLS_SINGLE: {
        return Eigen_CountNonzeros<single>(cp);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_CountNonzeros<doublecomplex>(cp);
    } break;
    case NLS_SCOMPLEX: {
        return Eigen_CountNonzeros<singlecomplex>(cp);
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"CountNonzeros");
    } break;
    }
    return 0;
}
//=============================================================================
indexType
Eigen_CountNonzerosMax(NelsonType dclass, indexType rows, indexType cols, const void* cp)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_CountNonzerosMax<logical>(cp);
    } break;
    case NLS_DOUBLE: {
        return Eigen_CountNonzerosMax<double>(cp);
    } break;
    case NLS_SINGLE: {
        return Eigen_CountNonzerosMax<single>(cp);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_CountNonzerosMax<doublecomplex>(cp);
    } break;
    case NLS_SCOMPLEX: {
        return Eigen_CountNonzerosMax<singlecomplex>(cp);
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"CountNonzerosMax");
    } break;
    }
    return 0;
}
//=============================================================================
void*
Eigen_SparseMatrixConstructor(NelsonType dclass, indexType rows, indexType cols, ArrayOfMatrix m)
{
    for (auto& i : m) {
        for (auto& j : i) {
            j.promoteType(dclass);
            j.makeSparse();
        }
    }
    switch (dclass) {
    case NLS_LOGICAL: {
        Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat = nullptr;
        try {
            spMat = new Eigen::SparseMatrix<logical, 0, signedIndexType>(rows, cols);
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        return spMat;
    } break;
    case NLS_DOUBLE: {
        Eigen::SparseMatrix<double, 0, signedIndexType>* spMat = nullptr;
        try {
            spMat = new Eigen::SparseMatrix<double, 0, signedIndexType>(rows, cols);
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        return spMat;
    } break;
    case NLS_SINGLE: {
        Eigen::SparseMatrix<single, 0, signedIndexType>* spMat = nullptr;
        try {
            spMat = new Eigen::SparseMatrix<single, 0, signedIndexType>(rows, cols);
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        return spMat;
    } break;
    case NLS_DCOMPLEX: {
        Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat = nullptr;
        try {
            spMat = new Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>(rows, cols);
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        return spMat;
    } break;
    case NLS_SCOMPLEX: {
        Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>* spMat = nullptr;
        try {
            spMat = new Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>(rows, cols);
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        return spMat;
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
            L"SparseMatrixConstructor");
    }
    }
    return nullptr;
}
//=============================================================================
template <class T>
void*
Eigen_CopyResizeSparseMatrix(
    const void* src, indexType rows, indexType cols, indexType maxrow, indexType maxcol)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)src;
    Eigen::SparseMatrix<T, 0, signedIndexType>* copiedpMat = nullptr;
    try {
        if (spMat == nullptr) {
            copiedpMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(maxrow, maxcol);
        } else {
            copiedpMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(*spMat);
        }
    } catch (const std::bad_alloc&) {
        copiedpMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    copiedpMat->conservativeResize(maxrow, maxcol);
    copiedpMat->finalize();
    copiedpMat->makeCompressed();
    return (void*)copiedpMat;
}
//=============================================================================
void*
Eigen_CopyResizeSparseMatrix(NelsonType dclass, const void* src, indexType rows, indexType cols,
    indexType maxrow, indexType maxcol)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_CopyResizeSparseMatrix<logical>(src, rows, cols, maxrow, maxcol);
    } break;
    case NLS_DOUBLE: {
        return Eigen_CopyResizeSparseMatrix<double>(src, rows, cols, maxrow, maxcol);
    } break;
    case NLS_SINGLE: {
        return Eigen_CopyResizeSparseMatrix<single>(src, rows, cols, maxrow, maxcol);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_CopyResizeSparseMatrix<doublecomplex>(src, rows, cols, maxrow, maxcol);
    } break;
    case NLS_SCOMPLEX: {
        return Eigen_CopyResizeSparseMatrix<singlecomplex>(src, rows, cols, maxrow, maxcol);
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
            L"CopyResizeSparseMatrix");
    } break;
    }
    return nullptr;
}
//=============================================================================
void*
Eigen_CreateSparseScalarElement(double v)
{
    Eigen::SparseMatrix<double, 0, signedIndexType>* pMat = nullptr;
    try {
        pMat = new Eigen::SparseMatrix<double, 0, signedIndexType>(1, 1);
    } catch (const std::bad_alloc&) {
        pMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    if (v != 0) {
        pMat->coeffRef(0, 0) = v;
    }
    pMat->finalize();
    pMat->makeCompressed();
    return pMat;
}
//=============================================================================
void*
Eigen_CreateSparseScalarElement(single v)
{
    Eigen::SparseMatrix<single, 0, signedIndexType>* pMat = nullptr;
    try {
        pMat = new Eigen::SparseMatrix<single, 0, signedIndexType>(1, 1);
    } catch (const std::bad_alloc&) {
        pMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    if (v != 0) {
        pMat->coeffRef(0, 0) = v;
    }
    pMat->finalize();
    pMat->makeCompressed();
    return pMat;
}
//=============================================================================
void*
Eigen_CreateSparseScalarElement(doublecomplex v)
{
    Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* pMat = nullptr;
    try {
        pMat = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(1, 1);
    } catch (const std::bad_alloc&) {
        pMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    if ((v.real() != 0.) || (v.imag() != 0.)) {
        pMat->coeffRef(0, 0) = v;
    }
    pMat->finalize();
    pMat->makeCompressed();
    return pMat;
}
//=============================================================================
void*
Eigen_CreateSparseScalarElement(singlecomplex v)
{
    Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>* pMat = nullptr;
    try {
        pMat = new Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>(1, 1);
    } catch (const std::bad_alloc&) {
        pMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    if ((v.real() != 0.) || (v.imag() != 0.)) {
        pMat->coeffRef(0, 0) = v;
    }
    pMat->finalize();
    pMat->makeCompressed();
    return pMat;
}
//=============================================================================
void*
Eigen_CreateSparseScalarElement(logical v)
{
    Eigen::SparseMatrix<logical, 0, signedIndexType>* pMat = nullptr;
    try {
        pMat = new Eigen::SparseMatrix<logical, 0, signedIndexType>(1, 1);
    } catch (const std::bad_alloc&) {
        pMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    if (v != 0) {
        pMat->coeffRef(0, 0) = v;
    }
    pMat->finalize();
    pMat->makeCompressed();
    return pMat;
}
//=============================================================================
template <class T>
void*
Eigen_GetSparseScalarElement(
    indexType rows, indexType cols, const void* src, indexType rindx, indexType cindx)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)src;
    T Value = spMat->coeff(rindx - 1, cindx - 1);
    spMat->makeCompressed();
    return (void*)Eigen_CreateSparseScalarElement(Value);
}
//=============================================================================
void*
Eigen_GetSparseScalarElement(NelsonType dclass, indexType rows, indexType cols, const void* src,
    indexType rindx, indexType cindx)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_GetSparseScalarElement<logical>(rows, cols, src, rindx, cindx);
    } break;
    case NLS_DOUBLE: {
        return Eigen_GetSparseScalarElement<double>(rows, cols, src, rindx, cindx);
    } break;
    case NLS_SINGLE: {
        return Eigen_GetSparseScalarElement<single>(rows, cols, src, rindx, cindx);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_GetSparseScalarElement<doublecomplex>(rows, cols, src, rindx, cindx);
    } break;
    case NLS_SCOMPLEX: {
        return Eigen_GetSparseScalarElement<singlecomplex>(rows, cols, src, rindx, cindx);
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"CountNonzeros");
    }
    }
    return nullptr;
}
//=============================================================================
template <class T>
void*
Eigen_ReshapeSparseMatrix(
    indexType rows, indexType cols, indexType newrows, indexType newcols, const void* cp)
{
    if ((rows == newrows) && (cols == newcols)) {
        return Eigen_CopySparseMatrix<T>(rows, cols, cp);
    }
    if (cp) {
        Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<T, 0, signedIndexType>*)cp;
        indexType nnz = spMat->nonZeros();
        indexType q = 0;
        T* pV = new_with_exception<T>(spMat->nonZeros(), false);
        indexType* pI = new_with_exception<indexType>(spMat->nonZeros(), false);
        indexType* pJ = new_with_exception<indexType>(spMat->nonZeros(), false);
        for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spMat, k);
                it; ++it) {
                pI[q] = it.row() + 1;
                pJ[q] = it.col() + 1;
                pV[q] = it.value();
                q++;
            }
        }
        using Triplet = Eigen::Triplet<T, signedIndexType>;
        std::vector<Triplet> tripletList;
        tripletList.reserve(nnz);
        for (indexType k = 0; k < nnz; k++) {
            indexType idx = pI[k] + (pJ[k] - 1) * rows;
            indexType vi = ((idx - 1) % newrows) + 1;
            indexType Y = (idx - vi) / newrows + 1;
            indexType X = vi;
            tripletList.push_back(Triplet(X - 1, Y - 1, pV[k]));
        }
        delete[] pV;
        delete[] pI;
        delete[] pJ;
        Eigen::SparseMatrix<T, 0, signedIndexType>* reshapedMat
            = new Eigen::SparseMatrix<T, 0, signedIndexType>(newrows, newcols);
        if (reshapedMat) {
            reshapedMat->setFromTriplets(tripletList.begin(), tripletList.end());
            reshapedMat->makeCompressed();
        }
        return (void*)reshapedMat;
    }

    return nullptr;
}
//=============================================================================
void*
Eigen_ReshapeSparseMatrix(NelsonType dclass, indexType rows, indexType cols, indexType newrows,
    indexType newcols, const void* cp)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_ReshapeSparseMatrix<logical>(rows, cols, newrows, newcols, cp);
    } break;
    case NLS_DOUBLE: {
        return Eigen_ReshapeSparseMatrix<double>(rows, cols, newrows, newcols, cp);
    } break;
    case NLS_SINGLE: {
        return Eigen_ReshapeSparseMatrix<single>(rows, cols, newrows, newcols, cp);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_ReshapeSparseMatrix<doublecomplex>(rows, cols, newrows, newcols, cp);
    } break;
    case NLS_SCOMPLEX: {
        return Eigen_ReshapeSparseMatrix<singlecomplex>(rows, cols, newrows, newcols, cp);
    } break;
    default: {
        Error(
            L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"ReshapeSparseMatrix");
    }
    }
    return nullptr;
}
//=============================================================================
