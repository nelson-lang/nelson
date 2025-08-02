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
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "SparseType.hpp"
#include "Error.hpp"
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
            Error(ERROR_MEMORY_ALLOCATION);
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
    case NLS_DCOMPLEX: {
        return Eigen_EyeSparseMatrixConstructor<doublecomplex>(rows, cols);
    } break;
    default:
        Error(_W("Unsupported type in EyeSparseMatrixConstructor."));
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
        Error(ERROR_MEMORY_ALLOCATION);
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
    case NLS_DCOMPLEX: {
        Eigen_DeleteSparseMatrix<doublecomplex>(cp);
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
            Error(ERROR_MEMORY_ALLOCATION);
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
                Error(ERROR_MEMORY_ALLOCATION);
            }
            Eigen::Map<Eigen::Matrix<doublecomplex, Eigen::Dynamic, Eigen::Dynamic>> matA(
                (doublecomplex*)pzMat, spMat->rows(), spMat->cols());
            matA = spMat->toDense();
        }
        return pMat;
    } break;
    default:
        Error(_W("Unsupported type in MakeDenseArrayOf."));
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
        Error(ERROR_MEMORY_ALLOCATION);
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
    case NLS_DCOMPLEX: {
        auto* Az = reinterpret_cast<doublecomplex*>((double*)cp);
        Eigen::Map<Eigen::Matrix<doublecomplex, Eigen::Dynamic, Eigen::Dynamic>> matA(
            Az, rows, cols);
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat;
        try {
            spMat = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(matA.sparseView());
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        spMat->makeCompressed();
        spMat->data().squeeze();
        return (void*)spMat;
    } break;
    default: {
        Error(_W("Unsupported type in MakeSparseArrayOf"));
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
            Error(ERROR_MEMORY_ALLOCATION);
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
    case NLS_DCOMPLEX: {
        return Eigen_CopySparseMatrix<doublecomplex>(rows, cols, cp);
    } break;
    default: {
        Error(_W("Unsupported type in CopySparseMatrix."));
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
        Error(_W("Invalid sparse."));
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
    case NLS_DCOMPLEX: {
        return Eigen_CountNonzeros<doublecomplex>(cp);
    } break;
    default: {
        Error(_W("Unsupported type in CountNonzeros."));
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
    case NLS_DCOMPLEX: {
        return Eigen_CountNonzerosMax<doublecomplex>(cp);
    } break;
    default: {
        Error(_W("Unsupported type in CountNonzerosMax."));
    } break;
    }
    return 0;
}
//=============================================================================
void*
Eigen_SparseMatrixConstructor(NelsonType dclass, indexType rows, indexType cols, ArrayOfMatrix m)
{
    // Precondition the arrays by converting to sparse and to
    // the output type
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
            Error(ERROR_MEMORY_ALLOCATION);
        }
        return spMat;
    } break;
    case NLS_DOUBLE: {
        Eigen::SparseMatrix<double, 0, signedIndexType>* spMat = nullptr;
        try {
            spMat = new Eigen::SparseMatrix<double, 0, signedIndexType>(rows, cols);
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        return spMat;
    } break;
    case NLS_DCOMPLEX: {
        Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat = nullptr;
        try {
            spMat = new Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>(rows, cols);
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        return spMat;
    } break;
    default: {
        Error(_W("Unsupported type in SparseMatrixConstructor."));
    }
    }
    return nullptr;
}
//=============================================================================
template <class T>
void*
Eigen_GetSparseVectorSubsetsInternal(indexType rows, indexType cols, const void* src,
    const indexType* indx, indexType irows, indexType icols)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMatsrc
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)src;
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    std::vector<Triplet> tripletList;
    tripletList.reserve(irows * icols);
    indexType X = 0;
    indexType Y = 0;
    for (indexType i = 0; i < irows * icols; i++) {
        indexType I = (indx[i] - 1) % rows;
        indexType J = (indx[i] - 1) / rows;
        T dValue = spMatsrc->coeff(I, J);
        if (dValue != 0.) {
            tripletList.push_back(Triplet(X, Y, dValue));
        }
        X++;
        if (X >= irows) {
            Y++;
            X = 0;
        }
    }
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat = nullptr;
    try {
        spMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(irows, icols);
    } catch (const std::bad_alloc&) {
        spMat = nullptr;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    spMat->setFromTriplets(tripletList.begin(), tripletList.end());
    spMat->finalize();
    return (void*)spMat;
}
//=============================================================================
void*
Eigen_GetSparseVectorSubsets(NelsonType dclass, indexType rows, indexType cols, const void* src,
    const indexType* indx, indexType irows, indexType icols)
{
    void* spMat = nullptr;
    indexType bound = rows * cols;
    for (indexType i = 0; i < irows * icols; i++) {
        double ndx = static_cast<double>(indx[i]) - 1;
        if ((ndx < 0) || ndx >= bound) {
            Error(_W("Index exceeds variable dimensions."));
        }
    }
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_GetSparseVectorSubsetsInternal<logical>(rows, cols, src, indx, irows, icols);
    } break;
    case NLS_DOUBLE: {
        return Eigen_GetSparseVectorSubsetsInternal<double>(rows, cols, src, indx, irows, icols);
    } break;
    case NLS_DCOMPLEX: {
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatsrc
            = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)src;
        using T = Eigen::Triplet<std::complex<double>, signedIndexType>;
        std::vector<T> tripletList;
        tripletList.reserve(irows * icols);
        indexType X = 0;
        indexType Y = 0;
        for (indexType i = 0; i < irows * icols; i++) {
            indexType I = (indx[i] - 1) % rows;
            indexType J = (indx[i] - 1) / rows;
            doublecomplex cValue = spMatsrc->coeff(I, J);
            if ((cValue.real() != 0.) && (cValue.imag() != 0.)) {
                tripletList.emplace_back(X, Y, cValue);
            }
            X++;
            if (X >= irows) {
                Y++;
                X = 0;
            }
        }
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat = nullptr;
        try {
            spMat = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(irows, icols);
        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        spMat->setFromTriplets(tripletList.begin(), tripletList.end());
        spMat->finalize();
        spMat->makeCompressed();
        return spMat;
    } break;
    default: {
        Error(_W("Unsupported type in CountNonzeros."));
    }
    }
    return spMat;
}
//=============================================================================
void*
Eigen_GetSparseNDimSubsets(NelsonType dclass, indexType rows, indexType cols, const void* src,
    const indexType* rindx, indexType irows, const indexType* cindx, indexType icols)
{
    void* spMat = nullptr;
    return spMat;
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
        Error(ERROR_MEMORY_ALLOCATION);
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
    case NLS_DCOMPLEX: {
        return Eigen_CopyResizeSparseMatrix<doublecomplex>(src, rows, cols, maxrow, maxcol);
    } break;
    default: {
        Error(_W("Unsupported type in SetSparseNDimSubsets."));
    } break;
    }
    return nullptr;
}
//=============================================================================
void*
Eigen_SetSparseVectorSubsets(NelsonType dclass, indexType& rows, indexType& cols, const void* src,
    const indexType* indx, indexType irows, indexType icols, const void* data, int advance)
{
    if (advance) {
        Error(_W("Eigen_DeleteSparseMatrixVectorSubset advanced not yet implemented."));
    }
    indexType* rowvect = new_with_exception<indexType>(irows * icols, false);
    indexType* colvect = new_with_exception<indexType>(irows * icols, false);
    for (indexType k = 0; k < irows * icols; k++) {
        auto idx = static_cast<indexType>(indx[k] - 1);
        rowvect[k] = static_cast<indexType>(idx % rows) + 1;
        colvect[k] = static_cast<indexType>(idx / rows) + 1;
    }
    void* spMat = Eigen_SetSparseNDimSubsets(
        dclass, rows, cols, src, rowvect, irows * icols, colvect, irows * icols, data, advance);
    delete[] rowvect;
    delete[] colvect;
    return spMat;
}
//=============================================================================
template <class T>
void*
Eigen_SetSparseNDimSubsetsInternal(indexType& rows, indexType& cols, const void* res,
    const indexType* rindx, indexType irows, const indexType* cindx, indexType icols,
    const void* data)
{
    T* dData = (T*)data;
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)res;
    spMat->uncompress();
    if (irows == icols) {
        for (indexType j = 0; j < irows; j++) {
            spMat->coeffRef(rindx[j] - 1, cindx[j] - 1) = dData[0];
        }
    } else {
        for (indexType i = 0; i < irows; i++) {
            for (indexType j = 0; j < icols; j++) {
                spMat->coeffRef(rindx[i] - 1, cindx[j] - 1) = dData[0];
            }
        }
    }
    spMat->finalize();
    spMat->makeCompressed();
    return (void*)spMat;
}
//=============================================================================
void*
Eigen_SetSparseNDimSubsets(NelsonType dclass, indexType& rows, indexType& cols, const void* src,
    const indexType* rindx, indexType irows, const indexType* cindx, indexType icols,
    const void* data, int advance)
{
    if (advance) {
        Error(_W("Eigen_SetSparseNDimSubsets advanced not yet implemented."));
    }
    void* res = nullptr;
    indexType i = 0;
    indexType maxrow = rindx[0];
    for (indexType i = 0; i < irows; i++) {
        maxrow = (maxrow > rindx[i]) ? maxrow : rindx[i];
    }
    indexType maxcol = cindx[0];
    for (indexType i = 0; i < icols; i++) {
        maxcol = (maxcol > cindx[i]) ? maxcol : cindx[i];
    }
    if ((maxrow > rows) || (maxcol > cols)) {
        maxrow = maxrow > rows ? maxrow : rows;
        maxcol = maxcol > cols ? maxcol : cols;
        res = Eigen_CopyResizeSparseMatrix(dclass, src, rows, cols, maxrow, maxcol);
        rows = maxrow;
        cols = maxcol;
    } else {
        res = Eigen_CopySparseMatrix(dclass, rows, cols, src);
    }
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_SetSparseNDimSubsetsInternal<logical>(
            rows, cols, res, rindx, irows, cindx, icols, data);
    } break;
    case NLS_DOUBLE: {
        return Eigen_SetSparseNDimSubsetsInternal<double>(
            rows, cols, res, rindx, irows, cindx, icols, data);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_SetSparseNDimSubsetsInternal<doublecomplex>(
            rows, cols, res, rindx, irows, cindx, icols, data);
    } break;
    default:
        Error(_W("Unsupported type in SetSparseNDimSubsets."));
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
        Error(ERROR_MEMORY_ALLOCATION);
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
        Error(ERROR_MEMORY_ALLOCATION);
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
        Error(ERROR_MEMORY_ALLOCATION);
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
    case NLS_DCOMPLEX: {
        return Eigen_GetSparseScalarElement<doublecomplex>(rows, cols, src, rindx, cindx);
    } break;
    default: {
        Error(_W("Unsupported type in CountNonzeros."));
    }
    }
    return nullptr;
}
//=============================================================================
template <class T>
void*
Eigen_SparseToIJV(const void* cp, indexType*& I, indexType*& J, int& nnz)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)cp;
    T* pV = nullptr;
    if (spMat) {
        indexType q = 0;
        pV = new_with_exception<T>(spMat->nonZeros(), false);
        for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spMat, k);
                 it; ++it) {
                I[q] = it.row() + 1;
                J[q] = it.col() + 1;
                pV[q] = it.value();
                q++;
            }
        }
        nnz = (int)spMat->nonZeros();
    } else {
        nnz = 0;
    }
    return (void*)pV;
}
//=============================================================================
void*
Eigen_SparseToIJV(NelsonType dclass, indexType rows, indexType cols, const void* cp, indexType*& I,
    indexType*& J, int& nnz)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_SparseToIJV<logical>(cp, I, J, nnz);
    } break;
    case NLS_DOUBLE: {
        return Eigen_SparseToIJV<double>(cp, I, J, nnz);
    } break;
    case NLS_DCOMPLEX: {
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)cp;
        double* pV = nullptr;
        if (spMat) {
            indexType q = 0;
            indexType k = 0;
            indexType l = 0;
            pV = new_with_exception<double>(spMat->nonZeros() * 2, false);
            for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
                for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                         *spMat, k);
                     it; ++it) {
                    I[q] = it.row() + 1;
                    J[q] = it.col() + 1;
                    pV[l] = it.value().real();
                    pV[l + 1] = it.value().imag();
                    l = l + 2;
                    q++;
                }
            }
            nnz = (int)spMat->nonZeros();
        } else {
            nnz = 0;
        }
        return (void*)pV;
    } break;
    default: {
        Error(_W("Unsupported type in SparseToIJV."));
    }
    }
    return nullptr;
}
//=============================================================================
template <class InputIterator, class T>
InputIterator
tripletfind(InputIterator first, InputIterator last, const T& val)
{
    while (first != last) {
        if ((*first).col() == val.col() && (*first).row() == val.row()) {
            return first;
        }
        ++first;
    }
    return last;
}
//=============================================================================
template <class T>
void*
Eigen_makeSparseFromIJVInternal(indexType rows, indexType cols, indexType nnz, indexType* I,
    indexType* J, const void* cp, bool bScalarV)
{
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    std::vector<Triplet> tripletList;
    tripletList.reserve(nnz);
    T* pV = (T*)cp;
    for (indexType k = 0; k < nnz; k++) {
        bool isZeroValue = false;
        Triplet tr;
        if (bScalarV) {
            if (pV[0]) {
                tr = Triplet(I[k] - 1, J[k] - 1, pV[0]);
            } else {
                isZeroValue = true;
            }
        } else {
            if (pV[0]) {
                tr = Triplet(I[k] - 1, J[k] - 1, pV[k]);
            } else {
                isZeroValue = true;
            }
        }

        if (!isZeroValue) {
            typename std::vector<Triplet>::iterator it
                = tripletfind(tripletList.begin(), tripletList.end(), tr);
            if (it != tripletList.end()) {
                T val = it->value();
                tripletList.erase(it);
                if (bScalarV) {
                    tr = Triplet(I[k] - 1, J[k] - 1, pV[0] + val);
                } else {
                    tr = Triplet(I[k] - 1, J[k] - 1, pV[k] + val);
                }
            }
            tripletList.push_back(tr);
        }
    }
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat = nullptr;
    try {
        spMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(rows, cols);
    } catch (const std::bad_alloc&) {
        spMat = nullptr;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    spMat->setFromTriplets(tripletList.begin(), tripletList.end());
    spMat->finalize();
    spMat->makeCompressed();
    return (void*)spMat;
}
//=============================================================================
void*
Eigen_makeSparseFromIJVLogical(indexType rows, indexType cols, indexType nnz, indexType* I,
    indexType* J, const void* cp, bool bScalarV)
{
    using Triplet = Eigen::Triplet<logical, signedIndexType>;
    std::vector<Triplet> tripletList;
    tripletList.reserve(nnz);
    auto* pV = (logical*)cp;
    for (indexType k = 0; k < nnz; k++) {
        bool isFalseValue = false;
        Triplet tr;
        if (bScalarV) {
            if (pV[0]) {
                tr = Triplet(I[k] - 1, J[k] - 1, pV[0]);
            } else {
                isFalseValue = true;
            }
        } else {
            if (pV[k]) {
                tr = Triplet(I[k] - 1, J[k] - 1, pV[k]);
            } else {
                isFalseValue = true;
            }
        }
        if (!isFalseValue) {
            std::vector<Triplet>::iterator it
                = tripletfind(tripletList.begin(), tripletList.end(), tr);
            if (it != tripletList.end()) {
                Error(_W("Repeated indices are not supported for sparse logical matrices."));
            }
            tripletList.push_back(tr);
        }
    }
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat = nullptr;
    try {
        spMat = new Eigen::SparseMatrix<logical, 0, signedIndexType>(rows, cols);
    } catch (const std::bad_alloc&) {
        spMat = nullptr;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    spMat->setFromTriplets(tripletList.begin(), tripletList.end());
    spMat->finalize();
    spMat->makeCompressed();
    return (void*)spMat;
}
//=============================================================================
void*
Eigen_makeSparseFromIJVComplex(indexType rows, indexType cols, indexType nnz, indexType* I,
    indexType* J, const void* cp, bool bScalarV)
{
    using T = Eigen::Triplet<std::complex<double>, signedIndexType>;
    std::vector<T> tripletList;
    tripletList.reserve(nnz);
    auto* pV = (double*)cp;
    indexType q = 0;
    for (indexType k = 0; k < nnz; k++) {
        /* Currently , for compatibility, complex values are not cumulative */
        if (bScalarV) {
            if (pV[0] || pV[1]) {
                doublecomplex v(pV[0], pV[1]);
                tripletList.emplace_back(I[k] - 1, J[k] - 1, v);
            }
            q = q + 2;
        } else {
            if (pV[q] || pV[q + 1]) {
                doublecomplex v(pV[q], pV[q + 1]);
                tripletList.emplace_back(I[k] - 1, J[k] - 1, v);
            }
        }
        q = q + 2;
    }
    Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat = nullptr;
    try {
        spMat = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(rows, cols);
    } catch (const std::bad_alloc&) {
        spMat = nullptr;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    spMat->setFromTriplets(tripletList.begin(), tripletList.end());
    spMat->finalize();
    spMat->makeCompressed();
    return (void*)spMat;
}
//=============================================================================
void*
Eigen_makeSparseFromIJV(NelsonType dclass, indexType rows, indexType cols, indexType nnz,
    indexType* I, int istride, indexType* J, int jstride, const void* cp, int cpstride,
    bool bScalarV)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_makeSparseFromIJVLogical(rows, cols, nnz, I, J, cp, bScalarV);
    } break;
    case NLS_DOUBLE: {
        return Eigen_makeSparseFromIJVInternal<double>(rows, cols, nnz, I, J, cp, bScalarV);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_makeSparseFromIJVComplex(rows, cols, nnz, I, J, cp, bScalarV);
    } break;
    default: {
        Error(_W("Unsupported type in SparseToIJV."));
    }
    }
    return nullptr;
}
//=============================================================================
void*
Eigen_DeleteSparseMatrixCols(
    NelsonType dclass, indexType rows, indexType cols, const void* cp, bool* dmap)
{
    void* spMat = nullptr;
    Error(_W("Eigen_DeleteSparseMatrixCols not yet implemented."));
    return spMat;
}
//=============================================================================
void*
Eigen_DeleteSparseMatrixRows(
    NelsonType dclass, indexType rows, indexType cols, const void* cp, bool* dmap)
{
    void* spMat = nullptr;
    Error(_W("Eigen_DeleteSparseMatrixRows not yet implemented."));
    return spMat;
}
//=============================================================================
void*
Eigen_DeleteSparseMatrixVectorSubset(NelsonType dclass, indexType& rows, indexType& cols,
    const void* cp, const indexType* todel, indexType delete_len)
{
    void* spMat = nullptr;
    Error(_W("Eigen_DeleteSparseMatrixVectorSubset not yet implemented."));
    return spMat;
}
//=============================================================================
void*
Eigen_TypeConvertSparse(
    NelsonType dclass, indexType rows, indexType cols, const void* cp, NelsonType oclass)
{
    switch (oclass) {
    case NLS_LOGICAL: {
        Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)cp;
        switch (dclass) {
        case NLS_LOGICAL: {
            return Eigen_CopySparseMatrix<logical>(rows, cols, cp);
        } break;
        case NLS_DOUBLE: {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMatdest = nullptr;
            try {
                spMatdest
                    = new Eigen::SparseMatrix<double, 0, signedIndexType>(spMat->cast<double>());
            } catch (const std::bad_alloc&) {
                Error(ERROR_MEMORY_ALLOCATION);
            }
            spMatdest->finalize();
            spMatdest->makeCompressed();
            return (void*)spMatdest;
        } break;
        case NLS_DCOMPLEX: {
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatdest = nullptr;
            try {
                spMatdest = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(
                    spMat->cast<doublecomplex>());
            } catch (const std::bad_alloc&) {
                Error(ERROR_MEMORY_ALLOCATION);
            }
            spMatdest->finalize();
            spMatdest->makeCompressed();
            return (void*)spMatdest;
        } break;
        default: {
            Error(_W("Unsupported type in TypeConvertSparse."));
        } break;
        }
    } break;
    case NLS_DOUBLE: {
        Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<double, 0, signedIndexType>*)cp;
        switch (dclass) {
        case NLS_LOGICAL: {
            Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatdest = nullptr;
            try {
                spMatdest = new Eigen::SparseMatrix<logical, 0, signedIndexType>(rows, cols);
            } catch (const std::bad_alloc&) {
                Error(ERROR_MEMORY_ALLOCATION);
            }
            if (rows * cols) {
                for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
                    for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(
                             *spMat, k);
                         it; ++it) {
                        logical bVal = (it.value() == 0) ? (logical)0 : (logical)1;
                        spMatdest->coeffRef(it.row(), it.col()) = bVal;
                    }
                }
            }
            spMatdest->finalize();
            spMatdest->makeCompressed();
            return (void*)spMatdest;
        } break;
        case NLS_DOUBLE: {
            return Eigen_CopySparseMatrix<double>(rows, cols, cp);
        } break;
        case NLS_DCOMPLEX: {
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatdest = nullptr;
            try {
                spMatdest = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(rows, cols);
            } catch (const std::bad_alloc&) {
                Error(ERROR_MEMORY_ALLOCATION);
            }
            if (rows * cols) {
                for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
                    for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(
                             *spMat, k);
                         it; ++it) {
                        doublecomplex dVal(it.value(), 0.0);
                        spMatdest->coeffRef(it.row(), it.col()) = dVal;
                    }
                }
            }
            spMatdest->finalize();
            spMatdest->makeCompressed();
            return (void*)spMatdest;
        } break;
        default: {
            Error(_W("Unsupported type in TypeConvertSparse."));
        } break;
        }
    } break;
    case NLS_DCOMPLEX: {
        switch (dclass) {
        case NLS_LOGICAL: {
            Error(_W("Unsupported type in TypeConvertSparse (complex to logical)."));
        } break;
        case NLS_DOUBLE: {
            Error(_W("Unsupported type in TypeConvertSparse (complex to double)."));
        } break;
        case NLS_DCOMPLEX: {
            return Eigen_CopySparseMatrix<doublecomplex>(rows, cols, cp);
        } break;
        default: {
            Error(_W("Unsupported type in TypeConvertSparse."));
        } break;
        }
    } break;
    default: {
        Error(_W("Unsupported type in TypeConvertSparse."));
    } break;
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
            reshapedMat->finalize();
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
    case NLS_DCOMPLEX: {
        return Eigen_ReshapeSparseMatrix<doublecomplex>(rows, cols, newrows, newcols, cp);
    } break;
    default: {
        Error(_W("Unsupported type in ReshapeSparseMatrix."));
    }
    }
    return nullptr;
}
//=============================================================================
