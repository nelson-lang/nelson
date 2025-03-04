//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define EIGEN_NO_DEBUG
//=============================================================================
#include <Eigen/Sparse>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "MxArrayOf.hpp"
#include "MxNumericTypes.h"
#include "MxHelpers.hpp"
#include "Exception.hpp"
#include "SparseConstructors.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static mwSize*
GetDimensions(const ArrayOf& array, mwSize& numdims)
{
    numdims = (mwSize)array.nDims();
    auto* dim_vec = (mwSize*)malloc(numdims * sizeof(mwSize));
    if (dim_vec != nullptr) {
        for (mwSize i = 0; i < numdims; i++) {
            dim_vec[i] = array.getDimensions()[i];
        }
    }
    return dim_vec;
}
//=============================================================================
template <class T, class S>
void
ArrayOfRealToMexReal(T* src, S* dst, size_t count)
{
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        dst[i] = (S)src[i];
    }
}
//=============================================================================
template <class T, class S>
void
MexRealToArrayOfReal(T* src, S* dst, size_t count)
{
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        dst[i] = (S)src[i];
    }
}
//=============================================================================
template <class T, class S>
void
MexComplexToArrayOfInterleavedComplex(T* src, S* dst, size_t count)
{
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count * 2; i++) {
        dst[i] = (S)src[i];
    }
}
//=============================================================================
template <class T, class S>
void
MexComplexToArrayOfSeparatedComplex(T* src_real, T* src_imag, S* dst, size_t count)
{
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        dst[2 * i] = (S)src_real[i];
        dst[2 * i + 1] = (S)src_imag[i];
    }
}
//=============================================================================
template <class mxType, class nlsType>
mxArray*
ArrayOfComplexToMexArray(const ArrayOf& array, mxClassID classID, bool interleavedComplex)
{
    mwSize num_dim;
    mwSize* dim_vec = GetDimensions(array, num_dim);
    mxArray* ret = mxCreateNumericArray(num_dim, dim_vec, classID, mxCOMPLEX);
    free(dim_vec);
    dim_vec = nullptr;
    if (ret) {
        auto* sp = (nlsType*)array.getDataPointer();
        auto* dp_r = (mxType*)ret->realdata;
        auto* dp_i = (mxType*)ret->imagdata;
        size_t N = mxGetNumberOfElements(ret);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            dp_r[i] = (mxType)sp[2 * i];
            dp_i[i] = (mxType)sp[2 * i + 1];
        }
    }
    return ret;
}
//=============================================================================
template <class mxType, class nlsType>
mxArray*
ArrayOfRealToMexArray(const ArrayOf& array, mxClassID classID)
{
    mwSize num_dim;
    mwSize* dim_vec = GetDimensions(array, num_dim);
    mxArray* ret = mxCreateNumericArray(num_dim, dim_vec, classID, mxREAL);
    free(dim_vec);
    dim_vec = nullptr;
    if (ret) {
        auto* sp = (nlsType*)array.getDataPointer();
        auto* dp = (mxType*)ret->realdata;
        size_t N = mxGetNumberOfElements(ret);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            dp[i] = (mxType)sp[i];
        }
    }
    return ret;
}
//=============================================================================
static mxArray*
ArrayOfSparseToMxArray(const ArrayOf& nlsArrayOf, bool interleavedComplex)
{
    mxArray* res;
    switch (nlsArrayOf.getDataClass()) {
    case NLS_LOGICAL: {
        res = mxNewArray();
        if (res != nullptr) {
            res->classID = mxLOGICAL_CLASS;
            res->interleavedcomplex = interleavedComplex;
            res->iscomplex = false;
            res->issparse = true;
            res->ptr = nullptr;
            res->imagdata = nullptr;
            Eigen::SparseMatrix<Nelson::logical, 0, Nelson::signedIndexType>* spMat
                = (Eigen::SparseMatrix<Nelson::logical, 0, Nelson::signedIndexType>*)
                      nlsArrayOf.getSparseDataPointer();
            Dimensions dims = nlsArrayOf.getDimensions();
            res->nzmax = nlsArrayOf.nzmax();
            res->number_of_dims = dims.getLength();
            res->dims = (mwSize*)mxCalloc(res->number_of_dims, sizeof(mwSize));
            for (mwIndex k = 0; k < res->number_of_dims; ++k) {
                res->dims[k] = dims.getAt(k);
            }
            if (spMat) {
                res->nIr = (mwIndex)spMat->nonZeros();
                res->nJc = (mwIndex)spMat->outerSize();
                Nelson::signedIndexType* pInner = spMat->innerIndexPtr();
                Nelson::signedIndexType* pOuter = spMat->outerIndexPtr();
                Nelson::logical* data = spMat->valuePtr();

                res->Ir = (mwIndex*)mxCalloc(res->nIr, sizeof(mwIndex));
                res->Jc = (mwIndex*)mxCalloc(res->nJc, sizeof(mwIndex));
                res->realdata = (mxLogical*)mxCalloc(res->nIr, sizeof(mxLogical));

                for (mwIndex k = 0; k < res->nIr; ++k) {
                    res->Ir[k] = (mwIndex)pInner[k];
                }
                for (mwIndex k = 0; k < res->nJc; ++k) {
                    res->Jc[k] = (mwIndex)pOuter[k];
                }
                memcpy(res->realdata, data, sizeof(mxLogical) * res->nIr);
            }
        }
    } break;
    case NLS_DCOMPLEX: {
        res = mxNewArray();
        if (res != nullptr) {
            res->classID = mxDOUBLE_CLASS;
            res->interleavedcomplex = interleavedComplex;
            res->iscomplex = true;
            res->issparse = true;
            res->ptr = nullptr;
            res->nzmax = nlsArrayOf.nzmax();
            Dimensions dims = nlsArrayOf.getDimensions();
            res->number_of_dims = dims.getLength();
            res->dims = (mwSize*)mxCalloc(res->number_of_dims, sizeof(mwSize));
            for (mwIndex k = 0; k < res->number_of_dims; ++k) {
                res->dims[k] = dims.getAt(k);
            }
            Eigen::SparseMatrix<doublecomplex, 0, Nelson::signedIndexType>* spMat
                = (Eigen::SparseMatrix<doublecomplex, 0, Nelson::signedIndexType>*)
                      nlsArrayOf.getSparseDataPointer();
            if (spMat) {
                res->nIr = (mwIndex)spMat->nonZeros();
                res->nJc = (mwIndex)spMat->outerSize();
                Nelson::signedIndexType* pInner = spMat->innerIndexPtr();
                Nelson::signedIndexType* pOuter = spMat->outerIndexPtr();

                res->Ir = (mwIndex*)mxCalloc(res->nIr, sizeof(mwIndex));
                res->Jc = (mwIndex*)mxCalloc(res->nJc, sizeof(mwIndex));
                doublecomplex* data = (doublecomplex*)spMat->valuePtr();

                if (interleavedComplex) {
                    res->realdata = (mxComplexDouble*)mxCalloc(res->nIr, sizeof(mxComplexDouble));
                    memcpy(res->realdata, data, sizeof(mxComplexDouble) * res->nIr);
                    res->imagdata = nullptr;
                } else {
                    res->realdata = (mxDouble*)mxCalloc(res->nIr, sizeof(mxDouble));
                    res->imagdata = (mxDouble*)mxCalloc(res->nIr, sizeof(mxDouble));
                    auto* realpart = (mxDouble*)res->realdata;
                    auto* imagpart = (mxDouble*)res->imagdata;
                    for (mwSize k = 0; k < res->nIr; ++k) {
                        realpart[k] = (mxDouble)data[k].real();
                        imagpart[k] = (mxDouble)data[k].imag();
                    }
                }
                OMP_PARALLEL_FOR_LOOP(res->nIr)
                for (ompIndexType k = 0; k < (ompIndexType)res->nIr; ++k) {
                    res->Ir[k] = (mwIndex)pInner[k];
                }
                OMP_PARALLEL_FOR_LOOP(res->nJc)
                for (ompIndexType k = 0; k < (ompIndexType)res->nJc; ++k) {
                    res->Jc[k] = (mwIndex)pOuter[k];
                }
            }
        }
    } break;
    case NLS_DOUBLE: {
        res = mxNewArray();
        if (res != nullptr) {
            res->classID = mxDOUBLE_CLASS;
            res->interleavedcomplex = interleavedComplex;
            res->iscomplex = false;
            res->issparse = true;
            res->ptr = nullptr;
            res->imagdata = nullptr;
            Eigen::SparseMatrix<mxDouble, 0, Nelson::signedIndexType>* spMat
                = (Eigen::SparseMatrix<mxDouble, 0, Nelson::signedIndexType>*)
                      nlsArrayOf.getSparseDataPointer();
            Dimensions dims = nlsArrayOf.getDimensions();
            res->nzmax = nlsArrayOf.nzmax();
            res->number_of_dims = dims.getLength();
            res->dims = (mwSize*)mxCalloc(res->number_of_dims, sizeof(mwSize));
            for (mwIndex k = 0; k < res->number_of_dims; ++k) {
                res->dims[k] = dims.getAt(k);
            }
            if (spMat) {
                res->nIr = (mwIndex)spMat->nonZeros();
                res->nJc = (mwIndex)spMat->outerSize();
                mwSize nnz = res->nIr == 0 ? res->nJc : res->nIr;
                Nelson::signedIndexType* pInner = spMat->innerIndexPtr();
                Nelson::signedIndexType* pOuter = spMat->outerIndexPtr();
                mxDouble* data = spMat->valuePtr();

                res->Ir = (mwIndex*)mxCalloc(res->nIr, sizeof(mwIndex));
                res->Jc = (mwIndex*)mxCalloc(res->nJc, sizeof(mwIndex));
                OMP_PARALLEL_FOR_LOOP(res->nIr)
                for (ompIndexType k = 0; k < (ompIndexType)res->nIr; ++k) {
                    res->Ir[k] = (mwIndex)pInner[k];
                }
                OMP_PARALLEL_FOR_LOOP(res->nJc)
                for (ompIndexType k = 0; k < (ompIndexType)res->nJc; ++k) {
                    res->Jc[k] = (mwIndex)pOuter[k];
                }

                res->realdata = (mxDouble*)mxCalloc(nnz, sizeof(mxDouble));
                memcpy(res->realdata, data, sizeof(mxDouble) * nnz);
            }
        }
    } break;
    default: {
        res = nullptr;
    } break;
    }
    return res;
}
//=============================================================================
#if (defined(_LP64) || defined(_WIN64))
#define SPARSE_INDEX_TYPE uint64
#define SPARSE_INDEX_CLASS NLS_UINT64
#else
#define SPARSE_INDEX_TYPE uint32
#define SPARSE_INDEX_CLASS NLS_UINT32
#endif
//=============================================================================
static ArrayOf
MxArraySparseToSparseDoubleArrayOf(const mxArray* pm)
{
    mwSize nIr = pm->nIr;
    mwSize nJc = pm->nJc;
    Eigen::Index nnz = nIr == 0 ? nJc : nIr;
    Eigen::Index rows = (Eigen::Index)pm->dims[0];
    Eigen::Index cols = (Eigen::Index)pm->dims[1];
    Dimensions dims(rows, cols);
    auto* outerIndexPtr = (Nelson::signedIndexType*)pm->Jc;
    auto* innerIndexPtr = (Nelson::signedIndexType*)pm->Ir;

    Eigen::Map<Eigen::SparseMatrix<double, 0, Nelson::signedIndexType>> sm(
        rows, cols, nnz, outerIndexPtr, innerIndexPtr, (double*)pm->realdata);
    std::vector<SPARSE_INDEX_TYPE> i;
    std::vector<SPARSE_INDEX_TYPE> j;
    std::vector<double> v;

    Eigen::Index n = 0;
    for (Eigen::Index k = 0; k < sm.outerSize() && n < nnz; ++k) {
        for (Eigen::Map<Eigen::SparseMatrix<double, 0, signedIndexType>>::InnerIterator it(sm, k);
             it; ++it) {
            if (n < nnz) {
                i.push_back((SPARSE_INDEX_TYPE)it.row() + 1);
                j.push_back((SPARSE_INDEX_TYPE)it.col() + 1);
                v.push_back((double)it.value());
                n++;
            } else {
                break;
            }
        }
    }
    Dimensions dimsI(1, i.size());
    Dimensions dimsJ(1, j.size());
    Dimensions dimsV(1, v.size());
    SPARSE_INDEX_TYPE* ptrI
        = (SPARSE_INDEX_TYPE*)ArrayOf::allocateArrayOf(SPARSE_INDEX_CLASS, i.size());
    memcpy(ptrI, i.data(), sizeof(SPARSE_INDEX_TYPE) * i.size());
    SPARSE_INDEX_TYPE* ptrJ
        = (SPARSE_INDEX_TYPE*)ArrayOf::allocateArrayOf(SPARSE_INDEX_CLASS, j.size());
    memcpy(ptrJ, j.data(), sizeof(SPARSE_INDEX_TYPE) * j.size());
    double* ptrV = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, v.size());
    memcpy(ptrV, v.data(), sizeof(double) * v.size());
    ArrayOf I = ArrayOf(SPARSE_INDEX_CLASS, dimsI, ptrI);
    ArrayOf J = ArrayOf(SPARSE_INDEX_CLASS, dimsJ, ptrJ);
    ArrayOf V = ArrayOf(NLS_DOUBLE, dimsJ, ptrV);
    return SparseConstructor(I, J, V, rows, cols, pm->nzmax);
}
//=============================================================================
static ArrayOf
MxArraySparseToSparseDoubleComplexArrayOf(const mxArray* pm)
{
    ArrayOf res;
    mwSize nIr = pm->nIr;
    mwSize nJc = pm->nJc;
    mwSize nzmax = pm->nzmax;
    mwIndex* Ir = pm->Ir;
    mwIndex* Jc = pm->Jc;
    Eigen::Index nnz = nIr == 0 ? nJc : nIr;
    Eigen::Index rows = (Eigen::Index)pm->dims[0];
    Eigen::Index cols = (Eigen::Index)pm->dims[1];
    Dimensions dims(rows, cols);
    auto* outerIndexPtr = (Nelson::signedIndexType*)pm->Jc;
    auto* innerIndexPtr = (Nelson::signedIndexType*)pm->Ir;

    if (pm->interleavedcomplex) {
        Eigen::Map<Eigen::SparseMatrix<doublecomplex, 0, Nelson::signedIndexType>> sm(
            rows, cols, nnz, outerIndexPtr, innerIndexPtr, (doublecomplex*)pm->realdata);
        std::vector<SPARSE_INDEX_TYPE> i;
        std::vector<SPARSE_INDEX_TYPE> j;
        std::vector<doublecomplex> v;

        Eigen::Index n = 0;
        for (Eigen::Index k = 0; k < sm.outerSize() && n < nnz; ++k) {
            for (Eigen::Map<Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>>::InnerIterator
                     it(sm, k);
                 it; ++it) {
                if (n < nnz) {
                    i.push_back((SPARSE_INDEX_TYPE)it.row() + 1);
                    j.push_back((SPARSE_INDEX_TYPE)it.col() + 1);
                    v.push_back((doublecomplex)it.value());
                    n++;
                } else {
                    break;
                }
            }
        }
        Dimensions dimsI(1, i.size());
        Dimensions dimsJ(1, j.size());
        Dimensions dimsV(1, v.size());
        SPARSE_INDEX_TYPE* ptrI
            = (SPARSE_INDEX_TYPE*)ArrayOf::allocateArrayOf(SPARSE_INDEX_CLASS, i.size());
        memcpy(ptrI, i.data(), sizeof(SPARSE_INDEX_TYPE) * i.size());
        SPARSE_INDEX_TYPE* ptrJ
            = (SPARSE_INDEX_TYPE*)ArrayOf::allocateArrayOf(SPARSE_INDEX_CLASS, j.size());
        memcpy(ptrJ, j.data(), sizeof(SPARSE_INDEX_TYPE) * j.size());
        double* ptrV = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, v.size());
        memcpy(ptrV, v.data(), sizeof(doublecomplex) * v.size());
        ArrayOf I = ArrayOf(SPARSE_INDEX_CLASS, dimsI, ptrI);
        ArrayOf J = ArrayOf(SPARSE_INDEX_CLASS, dimsJ, ptrJ);
        ArrayOf V = ArrayOf(NLS_DCOMPLEX, dimsJ, ptrV);
        return SparseConstructor(I, J, V, rows, cols, pm->nzmax);
    } else {
        auto* realpart = (double*)pm->realdata;
        auto* imagpart = (double*)pm->imagdata;
        double* interleaved = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nnz);
        Eigen::Index q = 0;
        for (ompIndexType k = 0; k < (ompIndexType)nnz; ++k) {
            interleaved[q] = realpart[k];
            interleaved[q + 1] = imagpart[k];
            q = q + 2;
        }
        auto* ri = reinterpret_cast<doublecomplex*>(interleaved);
        Eigen::Map<Eigen::SparseMatrix<doublecomplex, 0, Nelson::signedIndexType>> sm(
            rows, cols, nnz, outerIndexPtr, innerIndexPtr, ri);
        std::vector<SPARSE_INDEX_TYPE> i;
        std::vector<SPARSE_INDEX_TYPE> j;

        Eigen::Index n = 0;
        for (Eigen::Index k = 0; k < sm.outerSize() && n < nnz; ++k) {
            for (Eigen::Map<Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>>::InnerIterator
                     it(sm, k);
                 it; ++it) {
                if (n < nnz) {
                    i.push_back((SPARSE_INDEX_TYPE)it.row() + 1);
                    j.push_back((SPARSE_INDEX_TYPE)it.col() + 1);
                    n++;
                } else {
                    break;
                }
            }
        }
        Dimensions dimsI(1, i.size());
        Dimensions dimsJ(1, j.size());
        Dimensions dimsV(1, nnz);
        SPARSE_INDEX_TYPE* ptrI
            = (SPARSE_INDEX_TYPE*)ArrayOf::allocateArrayOf(SPARSE_INDEX_CLASS, i.size());
        memcpy(ptrI, i.data(), sizeof(SPARSE_INDEX_TYPE) * i.size());
        SPARSE_INDEX_TYPE* ptrJ
            = (SPARSE_INDEX_TYPE*)ArrayOf::allocateArrayOf(SPARSE_INDEX_CLASS, j.size());
        memcpy(ptrJ, j.data(), sizeof(SPARSE_INDEX_TYPE) * j.size());
        ArrayOf I = ArrayOf(SPARSE_INDEX_CLASS, dimsI, ptrI);
        ArrayOf J = ArrayOf(SPARSE_INDEX_CLASS, dimsJ, ptrJ);
        ArrayOf V = ArrayOf(NLS_DCOMPLEX, dimsV, interleaved);
        return SparseConstructor(I, J, V, rows, cols, pm->nzmax);
    }
    return res;
}
//=============================================================================
static ArrayOf
MxArraySparseToSparseLogicalArrayOf(const mxArray* pm)
{
    mwSize nIr = pm->nIr;
    mwSize nJc = pm->nJc;
    mwSize nzmax = pm->nzmax;
    mwIndex* Ir = pm->Ir;
    mwIndex* Jc = pm->Jc;
    Eigen::Index nnz = nIr == 0 ? nJc : nIr;
    Eigen::Index rows = (Eigen::Index)pm->dims[0];
    Eigen::Index cols = (Eigen::Index)pm->dims[1];
    Dimensions dims(rows, cols);
    auto* outerIndexPtr = (Nelson::signedIndexType*)pm->Jc;
    auto* innerIndexPtr = (Nelson::signedIndexType*)pm->Ir;

    Eigen::Map<Eigen::SparseMatrix<logical, 0, Nelson::signedIndexType>> sm(
        rows, cols, nnz, outerIndexPtr, innerIndexPtr, (logical*)pm->realdata);
    std::vector<SPARSE_INDEX_TYPE> i;
    std::vector<SPARSE_INDEX_TYPE> j;
    std::vector<logical> v;

    Eigen::Index n = 0;
    for (Eigen::Index k = 0; k < sm.outerSize() && n < nnz; ++k) {
        for (Eigen::Map<Eigen::SparseMatrix<logical, 0, signedIndexType>>::InnerIterator it(sm, k);
             it; ++it) {
            if (n < nnz) {
                i.push_back((SPARSE_INDEX_TYPE)it.row() + 1);
                j.push_back((SPARSE_INDEX_TYPE)it.col() + 1);
                v.push_back((logical)it.value());
                n++;
            } else {
                break;
            }
        }
    }
    Dimensions dimsI(1, i.size());
    Dimensions dimsJ(1, j.size());
    Dimensions dimsV(1, v.size());
    SPARSE_INDEX_TYPE* ptrI
        = (SPARSE_INDEX_TYPE*)ArrayOf::allocateArrayOf(SPARSE_INDEX_CLASS, i.size());
    memcpy(ptrI, i.data(), sizeof(SPARSE_INDEX_TYPE) * i.size());
    SPARSE_INDEX_TYPE* ptrJ
        = (SPARSE_INDEX_TYPE*)ArrayOf::allocateArrayOf(SPARSE_INDEX_CLASS, j.size());
    memcpy(ptrJ, j.data(), sizeof(SPARSE_INDEX_TYPE) * j.size());
    logical* ptrV = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, v.size());
    memcpy(ptrV, v.data(), sizeof(logical) * v.size());
    ArrayOf I = ArrayOf(SPARSE_INDEX_CLASS, dimsI, ptrI);
    ArrayOf J = ArrayOf(SPARSE_INDEX_CLASS, dimsJ, ptrJ);
    ArrayOf V = ArrayOf(NLS_LOGICAL, dimsJ, ptrV);
    return SparseConstructor(I, J, V, rows, cols, pm->nzmax);
}
//=============================================================================
static ArrayOf
MxArraySparseToArrayOf(const mxArray* pm)
{
    ArrayOf res;
    switch (pm->classID) {
    case mxLOGICAL_CLASS: {
        res = MxArraySparseToSparseLogicalArrayOf(pm);
    } break;
    case mxDOUBLE_CLASS: {
        if (pm->iscomplex) {
            res = MxArraySparseToSparseDoubleComplexArrayOf(pm);
        } else {
            res = MxArraySparseToSparseDoubleArrayOf(pm);
        }
    } break;
    default: {
        // never
    } break;
    }
    return res;
}
//=============================================================================
mxArray*
ArrayOfToMxArray(const ArrayOf& nlsArrayOf, bool interleavedComplex)
{
    mxArray* res = nullptr;
    switch (nlsArrayOf.getDataClass()) {
    case NLS_STRING_ARRAY: {
        res = mxNewArray();
        if (res != nullptr) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(nlsArrayOf, num_dim);
            res->number_of_dims = num_dim;
            res->dims = copyDims(num_dim, dim_vec);
            free(dim_vec);
            res->classID = mxOBJECT_CLASS;
            res->issparse = false;
            res->interleavedcomplex = interleavedComplex;
            res->iscomplex = false;
            res->imagdata = nullptr;
            res->realdata = nullptr;
            res->nzmax = 0;
            res->nIr = 0;
            res->nJc = 0;
            res->Jc = nullptr;
            res->Ir = nullptr;
            auto* ptr = new ArrayOf(nlsArrayOf);
            ptr->ensureSingleOwner();
            res->ptr = (uint64_t*)ptr;
        }
    } break;
    case NLS_HANDLE: {
        res = mxNewArray();
        if (res != nullptr) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(nlsArrayOf, num_dim);
            res->number_of_dims = num_dim;
            res->dims = copyDims(num_dim, dim_vec);
            free(dim_vec);
            res->classID = mxOBJECT_CLASS;
            res->issparse = false;
            res->interleavedcomplex = interleavedComplex;
            res->iscomplex = false;
            res->imagdata = nullptr;
            res->realdata = nullptr;
            res->nzmax = 0;
            res->nIr = 0;
            res->nJc = 0;
            res->Jc = nullptr;
            res->Ir = nullptr;
            auto* ptr = new ArrayOf(nlsArrayOf);
            ptr->ensureSingleOwner();
            res->ptr = (uint64_t*)ptr;
        }
    } break;
    case NLS_CELL_ARRAY: {
        mwSize num_dim;
        mwSize* dim_vec = GetDimensions(nlsArrayOf, num_dim);
        res = mxCreateCellArray(num_dim, dim_vec);
        free(dim_vec);
        if (res != nullptr) {
            auto* sp = (ArrayOf*)nlsArrayOf.getDataPointer();
            auto** dp = (mxArray**)res->realdata;
            size_t N = mxGetNumberOfElements(res);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                dp[i] = ArrayOfToMxArray(sp[i], interleavedComplex);
            }
        }
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY: {
        res = mxNewArray();
        if (res != nullptr) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(nlsArrayOf, num_dim);
            res->number_of_dims = num_dim;
            res->dims = copyDims(num_dim, dim_vec);
            free(dim_vec);
            res->classID = mxSTRUCT_CLASS;
            res->issparse = false;
            res->interleavedcomplex = interleavedComplex;
            res->iscomplex = false;
            res->imagdata = nullptr;
            res->realdata = nullptr;
            res->nzmax = 0;
            res->nIr = 0;
            res->nJc = 0;
            res->Jc = nullptr;
            res->Ir = nullptr;
            auto* ptr = new ArrayOf(nlsArrayOf);
            ptr->ensureSingleOwner();
            res->ptr = (uint64_t*)ptr;
        }
        res = mxNewArray();
        if (res != nullptr) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(nlsArrayOf, num_dim);
            res->number_of_dims = num_dim;
            res->dims = copyDims(num_dim, dim_vec);
            free(dim_vec);
            res->classID = mxSTRUCT_CLASS;
            res->issparse = false;
            res->interleavedcomplex = interleavedComplex;
            res->iscomplex = false;
            res->imagdata = nullptr;
            res->realdata = nullptr;
            res->nzmax = 0;
            res->nIr = 0;
            res->nJc = 0;
            res->Jc = nullptr;
            res->Ir = nullptr;
            auto* ptr = new ArrayOf(nlsArrayOf);
            ptr->ensureSingleOwner();
            res->ptr = (uint64_t*)ptr;
        }
    } break;
    case NLS_LOGICAL: {
        if (nlsArrayOf.isSparse()) {
            res = ArrayOfSparseToMxArray(nlsArrayOf, interleavedComplex);
        } else {
            res = ArrayOfRealToMexArray<mxLogical, logical>(nlsArrayOf, mxLOGICAL_CLASS);
        }
    } break;
    case NLS_UINT8: {
        res = ArrayOfRealToMexArray<mxUint8, uint8>(nlsArrayOf, mxUINT8_CLASS);
    } break;
    case NLS_INT8: {
        res = ArrayOfRealToMexArray<mxInt8, int8>(nlsArrayOf, mxINT8_CLASS);
    } break;
    case NLS_UINT16: {
        res = ArrayOfRealToMexArray<mxUint16, uint16>(nlsArrayOf, mxUINT16_CLASS);
    } break;
    case NLS_INT16: {
        res = ArrayOfRealToMexArray<mxInt16, int16>(nlsArrayOf, mxINT16_CLASS);
    } break;
    case NLS_UINT32: {
        res = ArrayOfRealToMexArray<mxUint32, uint32>(nlsArrayOf, mxUINT32_CLASS);
    } break;
    case NLS_INT32: {
        res = ArrayOfRealToMexArray<mxInt32, int32>(nlsArrayOf, mxINT32_CLASS);
    } break;
    case NLS_UINT64: {
        res = ArrayOfRealToMexArray<mxUint64, uint64>(nlsArrayOf, mxUINT64_CLASS);
    } break;
    case NLS_INT64: {
        res = ArrayOfRealToMexArray<mxInt64, int64>(nlsArrayOf, mxINT64_CLASS);
    } break;
    case NLS_SINGLE: {
        res = ArrayOfRealToMexArray<mxSingle, single>(nlsArrayOf, mxSINGLE_CLASS);
    } break;
    case NLS_DOUBLE: {
        if (nlsArrayOf.isSparse()) {
            res = ArrayOfSparseToMxArray(nlsArrayOf, interleavedComplex);
        } else {
            res = ArrayOfRealToMexArray<mxDouble, double>(nlsArrayOf, mxDOUBLE_CLASS);
        }
    } break;
    case NLS_SCOMPLEX: {
        res = ArrayOfComplexToMexArray<mxSingle, single>(
            nlsArrayOf, mxSINGLE_CLASS, interleavedComplex);
    } break;
    case NLS_DCOMPLEX: {
        if (nlsArrayOf.isSparse()) {
            res = ArrayOfSparseToMxArray(nlsArrayOf, interleavedComplex);
        } else {
            res = ArrayOfComplexToMexArray<mxDouble, double>(
                nlsArrayOf, mxDOUBLE_CLASS, interleavedComplex);
        }
    } break;
    case NLS_CHAR: {
        res = ArrayOfRealToMexArray<mxChar, charType>(nlsArrayOf, mxCHAR_CLASS);
    } break;
    case NLS_GO_HANDLE: {
        res = mxNewArray();
        if (res != nullptr) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(nlsArrayOf, num_dim);
            res->number_of_dims = num_dim;
            res->dims = copyDims(num_dim, dim_vec);
            free(dim_vec);
            res->classID = mxOBJECT_CLASS;
            res->issparse = false;
            res->interleavedcomplex = interleavedComplex;
            res->iscomplex = false;
            res->imagdata = nullptr;
            res->realdata = nullptr;
            res->nzmax = 0;
            res->nIr = 0;
            res->nJc = 0;
            res->Jc = nullptr;
            res->Ir = nullptr;
            auto* ptr = new ArrayOf(nlsArrayOf);
            ptr->ensureSingleOwner();
            res->ptr = (uint64_t*)ptr;
        }
    } break;
    default: {
        Error(_("C MEX type not managed."));
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
MxArrayToArrayOf(const mxArray* pm)
{
    ArrayOf res;
    if (pm == nullptr || (pm->dims == nullptr && pm->number_of_dims == 0)) {
        Dimensions dims(1, 0);
        return ArrayOf::emptyConstructor(dims);
    }
    NelsonType destClass = NLS_UNKNOWN;
    Dimensions dim;
    void* cp = nullptr;
    for (mwSize i = 0; i < pm->number_of_dims; i++) {
        dim[i] = pm->dims[i];
    }
    size_t N = mxGetNumberOfElements(pm);
    switch (pm->classID) {
    case mxCELL_CLASS: {
        destClass = NLS_CELL_ARRAY;
        auto** dp = (mxArray**)pm->realdata;
        cp = ArrayOf::allocateArrayOf(destClass, N);
        auto* elements = (ArrayOf*)cp;
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            elements[i] = MxArrayToArrayOf(dp[i]);
        }
    } break;
    case mxSTRUCT_CLASS: {
        auto* ptr = (ArrayOf*)pm->ptr;
        res = ArrayOf(*ptr);
        res.ensureSingleOwner();
        return res;
    } break;
    case mxLOGICAL_CLASS: {
        destClass = NLS_LOGICAL;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        if (pm->issparse) {
            return MxArraySparseToArrayOf(pm);
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxLogical, logical>((mxLogical*)pm->realdata, (logical*)cp, N);

    } break;
    case mxCHAR_CLASS: {
        destClass = NLS_CHAR;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxChar, charType>((mxChar*)pm->realdata, (charType*)cp, N);
    } break;
    case mxDOUBLE_CLASS: {
        if (pm->issparse) {
            return MxArraySparseToArrayOf(pm);
        }
        if (pm->iscomplex) {
            destClass = NLS_DCOMPLEX;
            cp = ArrayOf::allocateArrayOf(destClass, N);
            if (pm->interleavedcomplex) {
                MexComplexToArrayOfInterleavedComplex<mxDouble, double>(
                    (mxDouble*)pm->realdata, (double*)cp, N);
            } else {
                MexComplexToArrayOfSeparatedComplex<mxDouble, double>(
                    (mxDouble*)pm->realdata, (mxDouble*)pm->imagdata, (double*)cp, N);
            }
        } else {
            destClass = NLS_DOUBLE;
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxDouble, double>((mxDouble*)pm->realdata, (double*)cp, N);
        }

    } break;
    case mxSINGLE_CLASS: {
        if (pm->iscomplex) {
            destClass = NLS_SCOMPLEX;
            cp = ArrayOf::allocateArrayOf(destClass, N);
            if (pm->interleavedcomplex) {
                MexComplexToArrayOfInterleavedComplex<mxSingle, single>(
                    (mxSingle*)pm->realdata, (single*)cp, N);
            } else {
                MexComplexToArrayOfSeparatedComplex<mxSingle, single>(
                    (mxSingle*)pm->realdata, (mxSingle*)pm->imagdata, (single*)cp, N);
            }
        } else {
            destClass = NLS_SINGLE;
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxSingle, single>((mxSingle*)pm->realdata, (single*)cp, N);
        }
    } break;
    case mxINT8_CLASS: {
        destClass = NLS_INT8;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxInt8, int8>((mxInt8*)pm->realdata, (int8*)cp, N);
    } break;
    case mxUINT8_CLASS: {
        destClass = NLS_UINT8;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxUint8, uint8>((mxUint8*)pm->realdata, (uint8*)cp, N);
    } break;
    case mxINT16_CLASS: {
        destClass = NLS_INT16;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxInt16, int16>((mxInt16*)pm->realdata, (int16*)cp, N);
    } break;
    case mxUINT16_CLASS: {
        destClass = NLS_UINT16;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxUint16, uint16>((mxUint16*)pm->realdata, (uint16*)cp, N);
    } break;
    case mxINT32_CLASS: {
        destClass = NLS_INT32;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxInt32, int32>((mxInt32*)pm->realdata, (int32*)cp, N);
    } break;
    case mxUINT32_CLASS: {
        destClass = NLS_UINT32;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxUint32, uint32>((mxUint32*)pm->realdata, (uint32*)cp, N);
    } break;
    case mxINT64_CLASS: {
        destClass = NLS_INT64;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxInt64, int64>((mxInt64*)pm->realdata, (int64*)cp, N);
    } break;
    case mxUINT64_CLASS: {
        destClass = NLS_UINT64;
        if (pm->iscomplex) {
            Error(_("C MEX type not managed."));
        }
        cp = ArrayOf::allocateArrayOf(destClass, N);
        MexRealToArrayOfReal<mxUint64, uint64>((mxUint64*)pm->realdata, (uint64*)cp, N);
    } break;
    case mxOBJECT_CLASS: {
        auto* ptr = (ArrayOf*)pm->ptr;
        res = ArrayOf(*ptr);
        res.ensureSingleOwner();
        return res;
    } break;
    default: {
        Error(_("C MEX type not managed."));
    } break;
    }
    return ArrayOf(destClass, dim, cp);
}
//=============================================================================
}
//=============================================================================
