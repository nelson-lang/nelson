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
#include <vector>
#include <Eigen/Sparse>
#include "SparseType.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
// Subset extraction, assignment and deletion of sparse matrices, written once
// for every sparse class (logical, double, single and their complex forms).
//=============================================================================
template <class T> using SparseMatrixT = Eigen::SparseMatrix<T, 0, signedIndexType>;
template <class T> using TripletT = Eigen::Triplet<T, signedIndexType>;
//=============================================================================
template <class Fn>
static void*
dispatchSparseClass(NelsonType dclass, const wchar_t* functionName, Fn&& fn);
template <class T>
static void*
sparseFromTriplets(indexType rows, indexType cols, const std::vector<TripletT<T>>& triplets);
template <class T>
static void*
getVectorSubsets(
    indexType rows, const void* src, const indexType* indx, indexType irows, indexType icols);
template <class T>
static void*
getNDimSubsets(const void* src, const indexType* rindx, indexType irows, const indexType* cindx,
    indexType icols);
template <class T>
static void*
setVectorSubsets(indexType rows, void* res, const indexType* indx, indexType nelem,
    const void* data, bool isScalarData);
template <class T>
static void*
setNDimSubsets(void* res, const indexType* rindx, indexType irows, const indexType* cindx,
    indexType icols, const void* data, bool isScalarData);
template <class T>
static void*
deleteCols(indexType rows, indexType cols, const void* cp, const bool* dmap, indexType keepCols);
template <class T>
static void*
deleteRows(
    indexType cols, const void* cp, const std::vector<signedIndexType>& rowMap, indexType keepRows);
template <class T>
static void*
deleteVectorSubset(indexType oldRows, const void* cp, const std::vector<char>& delmap,
    indexType newRows, indexType newCols);
//=============================================================================
void*
Eigen_GetSparseVectorSubsets(NelsonType dclass, indexType rows, indexType cols, const void* src,
    const indexType* indx, indexType irows, indexType icols)
{
    indexType bound = rows * cols;
    for (indexType i = 0; i < irows * icols; i++) {
        double ndx = static_cast<double>(indx[i]) - 1;
        if ((ndx < 0) || ndx >= bound) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
    }
    return dispatchSparseClass(dclass, L"GetSparseVectorSubsets",
        [&](auto tag) { return getVectorSubsets<decltype(tag)>(rows, src, indx, irows, icols); });
}
//=============================================================================
void*
Eigen_GetSparseNDimSubsets(NelsonType dclass, indexType rows, indexType cols, const void* src,
    const indexType* rindx, indexType irows, const indexType* cindx, indexType icols)
{
    for (indexType i = 0; i < irows; ++i) {
        double ndx = static_cast<double>(rindx[i]) - 1;
        if ((ndx < 0) || ndx >= rows) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
    }
    for (indexType j = 0; j < icols; ++j) {
        double ndx = static_cast<double>(cindx[j]) - 1;
        if ((ndx < 0) || ndx >= cols) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
    }
    return dispatchSparseClass(dclass, L"GetSparseNDimSubsets",
        [&](auto tag) { return getNDimSubsets<decltype(tag)>(src, rindx, irows, cindx, icols); });
}
//=============================================================================
void*
Eigen_SetSparseVectorSubsets(NelsonType dclass, indexType& rows, indexType& cols, const void* src,
    const indexType* indx, indexType irows, indexType icols, const void* data, int advance)
{
    indexType nelem = irows * icols;
    indexType maxIndex = 0;
    for (indexType k = 0; k < nelem; ++k) {
        if (indx[k] > maxIndex) {
            maxIndex = indx[k];
        }
    }
    void* res = nullptr;
    if (maxIndex > rows * cols) {
        // Linear-index growth is only defined for vectors (and empties).
        if (rows * cols == 0 || rows == 1) {
            res = Eigen_CopyResizeSparseMatrix(dclass, src, rows, cols, 1, maxIndex);
            rows = 1;
            cols = maxIndex;
        } else if (cols == 1) {
            res = Eigen_CopyResizeSparseMatrix(dclass, src, rows, cols, maxIndex, 1);
            rows = maxIndex;
        } else {
            Error(L"Nelson:sparse:cannotResize",
                L"In an assignment A(I) = B, a matrix A cannot be resized.");
        }
    } else {
        res = Eigen_CopySparseMatrix(dclass, rows, cols, src);
    }
    indexType resRows = rows;
    return dispatchSparseClass(dclass, L"SetSparseVectorSubsets", [&](auto tag) {
        return setVectorSubsets<decltype(tag)>(resRows, res, indx, nelem, data, advance == 0);
    });
}
//=============================================================================
void*
Eigen_SetSparseNDimSubsets(NelsonType dclass, indexType& rows, indexType& cols, const void* src,
    const indexType* rindx, indexType irows, const indexType* cindx, indexType icols,
    const void* data, int advance)
{
    // advance == 0 broadcasts a scalar right-hand side.
    void* res = nullptr;
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
    return dispatchSparseClass(dclass, L"SetSparseNDimSubsets", [&](auto tag) {
        return setNDimSubsets<decltype(tag)>(res, rindx, irows, cindx, icols, data, advance == 0);
    });
}
//=============================================================================
void*
Eigen_DeleteSparseMatrixCols(
    NelsonType dclass, indexType rows, indexType cols, const void* cp, bool* dmap)
{
    // dmap has 'cols' entries, true means delete that column.
    indexType keepCols = 0;
    for (indexType j = 0; j < cols; ++j) {
        if (!dmap[j]) {
            keepCols++;
        }
    }
    if (keepCols == 0) {
        // No columns left: the caller sets the dimensions.
        return nullptr;
    }
    return dispatchSparseClass(dclass, L"DeleteSparseMatrixCols",
        [&](auto tag) { return deleteCols<decltype(tag)>(rows, cols, cp, dmap, keepCols); });
}
//=============================================================================
void*
Eigen_DeleteSparseMatrixRows(
    NelsonType dclass, indexType rows, indexType cols, const void* cp, bool* dmap)
{
    // dmap has 'rows' entries, true means delete that row.
    indexType keepRows = 0;
    for (indexType i = 0; i < rows; ++i) {
        if (!dmap[i]) {
            keepRows++;
        }
    }
    if (keepRows == 0) {
        // No rows left: the caller sets the dimensions.
        return nullptr;
    }
    // Old row -> new row index, -1 for a deleted row (signed on purpose).
    std::vector<signedIndexType> rowMap(rows, -1);
    signedIndexType newRow = 0;
    for (indexType i = 0; i < rows; ++i) {
        if (!dmap[i]) {
            rowMap[i] = newRow++;
        }
    }
    return dispatchSparseClass(dclass, L"DeleteSparseMatrixRows",
        [&](auto tag) { return deleteRows<decltype(tag)>(cols, cp, rowMap, keepRows); });
}
//=============================================================================
void*
Eigen_DeleteSparseMatrixVectorSubset(NelsonType dclass, indexType& rows, indexType& cols,
    const void* cp, const indexType* todel, indexType delete_len)
{
    indexType oldRows = rows;
    indexType oldCols = cols;
    indexType N = oldRows * oldCols;
    std::vector<char> delmap(N, 0);
    for (indexType k = 0; k < delete_len; ++k) {
        double ndx = static_cast<double>(todel[k]) - 1;
        if ((ndx < 0) || ndx >= N) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        delmap[static_cast<size_t>(todel[k] - 1)] = 1;
    }
    indexType newSize = 0;
    for (indexType i = 0; i < N; ++i) {
        if (!delmap[i]) {
            newSize++;
        }
    }
    // New dimensions follow the dense semantics.
    bool isScalar = (oldRows == 1 && oldCols == 1);
    bool isVector = (oldRows == 1 || oldCols == 1);
    indexType newRows = 0;
    indexType newCols = 0;
    if (isScalar) {
        if (delete_len == 1) {
            newRows = 0;
            newCols = 0;
        } else {
            newRows = 1;
            newCols = newSize;
        }
    } else if (isVector) {
        if (oldRows != 1) {
            newRows = newSize;
            newCols = oldCols;
        } else {
            newRows = oldRows;
            newCols = newSize;
        }
    } else {
        // A matrix losing elements becomes a row; losing all of them, 0x0.
        newRows = (newSize == 0) ? 0 : 1;
        newCols = newSize;
    }
    void* res = dispatchSparseClass(dclass, L"DeleteSparseMatrixVectorSubset", [&](auto tag) {
        return deleteVectorSubset<decltype(tag)>(oldRows, cp, delmap, newRows, newCols);
    });
    rows = newRows;
    cols = newCols;
    return res;
}
//=============================================================================
template <class Fn>
void*
dispatchSparseClass(NelsonType dclass, const wchar_t* functionName, Fn&& fn)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return fn(logical());
    } break;
    case NLS_DOUBLE: {
        return fn(double());
    } break;
    case NLS_SINGLE: {
        return fn(single());
    } break;
    case NLS_DCOMPLEX: {
        return fn(doublecomplex());
    } break;
    case NLS_SCOMPLEX: {
        return fn(singlecomplex());
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", functionName);
    } break;
    }
    return nullptr;
}
//=============================================================================
template <class T>
void*
sparseFromTriplets(indexType rows, indexType cols, const std::vector<TripletT<T>>& triplets)
{
    SparseMatrixT<T>* dst = nullptr;
    try {
        dst = new SparseMatrixT<T>(rows, cols);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    dst->setFromTriplets(triplets.begin(), triplets.end());
    dst->makeCompressed();
    return (void*)dst;
}
//=============================================================================
template <class T>
void*
getVectorSubsets(
    indexType rows, const void* src, const indexType* indx, indexType irows, indexType icols)
{
    const auto* spMatsrc = static_cast<const SparseMatrixT<T>*>(src);
    std::vector<TripletT<T>> tripletList;
    tripletList.reserve(irows * icols);
    indexType X = 0;
    indexType Y = 0;
    for (indexType i = 0; i < irows * icols; i++) {
        indexType I = (indx[i] - 1) % rows;
        indexType J = (indx[i] - 1) / rows;
        T value = spMatsrc->coeff(I, J);
        if (value != T(0)) {
            tripletList.emplace_back(X, Y, value);
        }
        X++;
        if (X >= irows) {
            Y++;
            X = 0;
        }
    }
    return sparseFromTriplets<T>(irows, icols, tripletList);
}
//=============================================================================
template <class T>
void*
getNDimSubsets(const void* src, const indexType* rindx, indexType irows, const indexType* cindx,
    indexType icols)
{
    const auto* spMatsrc = static_cast<const SparseMatrixT<T>*>(src);
    std::vector<TripletT<T>> tripletList;
    tripletList.reserve(spMatsrc ? spMatsrc->nonZeros() : 0);
    // Map each source row to the output row position(s) that request it, then walk
    // only the stored nonzeros of the requested columns: O(nnz of those columns),
    // where probing every output cell with coeff() would be O(irows*icols).
    const indexType srcRows = static_cast<indexType>(spMatsrc->rows());
    std::vector<std::vector<indexType>> rowMap(srcRows);
    for (indexType i = 0; i < irows; ++i) {
        rowMap[rindx[i] - 1].push_back(i);
    }
    for (indexType j = 0; j < icols; ++j) {
        for (typename SparseMatrixT<T>::InnerIterator it(*spMatsrc, cindx[j] - 1); it; ++it) {
            if (it.value() == T(0)) {
                continue;
            }
            for (indexType oi : rowMap[it.row()]) {
                tripletList.emplace_back(oi, j, it.value());
            }
        }
    }
    return sparseFromTriplets<T>(irows, icols, tripletList);
}
//=============================================================================
template <class T>
void*
setVectorSubsets(indexType rows, void* res, const indexType* indx, indexType nelem,
    const void* data, bool isScalarData)
{
    auto* spMat = static_cast<SparseMatrixT<T>*>(res);
    const T* pV = static_cast<const T*>(data);
    spMat->uncompress();
    for (indexType k = 0; k < nelem; ++k) {
        indexType idx = indx[k] - 1;
        spMat->coeffRef(idx % rows, idx / rows) = isScalarData ? pV[0] : pV[k];
    }
    spMat->finalize();
    spMat->makeCompressed();
    return res;
}
//=============================================================================
template <class T>
void*
setNDimSubsets(void* res, const indexType* rindx, indexType irows, const indexType* cindx,
    indexType icols, const void* data, bool isScalarData)
{
    auto* spMat = static_cast<SparseMatrixT<T>*>(res);
    const T* pV = static_cast<const T*>(data);
    const indexType nRows = static_cast<indexType>(spMat->rows());
    const indexType nCols = static_cast<indexType>(spMat->cols());
    // Membership of the assigned rows/columns, and whether any subscript is
    // repeated (repeats carry "last write wins" semantics, including an
    // explicit zero overwriting an earlier value).
    std::vector<char> rowInTarget(nRows, 0);
    std::vector<char> colInTarget(nCols, 0);
    indexType distinctRows = 0;
    for (indexType i = 0; i < irows; ++i) {
        char& flag = rowInTarget[rindx[i] - 1];
        if (flag == 0) {
            flag = 1;
            ++distinctRows;
        }
    }
    indexType distinctCols = 0;
    for (indexType j = 0; j < icols; ++j) {
        char& flag = colInTarget[cindx[j] - 1];
        if (flag == 0) {
            flag = 1;
            ++distinctCols;
        }
    }
    if ((distinctRows != irows) || (distinctCols != icols)) {
        // Repeated subscripts take the element-by-element path, which is correct
        // for last-write-wins. They are rare, so its cost is acceptable.
        spMat->uncompress();
        for (indexType i = 0; i < irows; ++i) {
            for (indexType j = 0; j < icols; ++j) {
                spMat->coeffRef(rindx[i] - 1, cindx[j] - 1)
                    = pV[isScalarData ? 0 : (i + j * irows)];
            }
        }
        spMat->finalize();
        spMat->makeCompressed();
        return res;
    }
    // Unique subscripts: rebuild from triplets in O(nnz + irows*icols). Each
    // coeffRef insert into a growing sparse matrix costs O(nnz). Existing entries
    // inside the assigned region are dropped; data zeros leave no entry.
    std::vector<TripletT<T>> tripletList;
    tripletList.reserve(spMat->nonZeros() + irows * icols);
    for (indexType k = 0; k < nCols; ++k) {
        for (typename SparseMatrixT<T>::InnerIterator it(*spMat, k); it; ++it) {
            if (!(rowInTarget[it.row()] && colInTarget[k])) {
                tripletList.emplace_back(it.row(), k, it.value());
            }
        }
    }
    for (indexType j = 0; j < icols; ++j) {
        for (indexType i = 0; i < irows; ++i) {
            T value = pV[isScalarData ? 0 : (i + j * irows)];
            if (value != T(0)) {
                tripletList.emplace_back(rindx[i] - 1, cindx[j] - 1, value);
            }
        }
    }
    spMat->setZero();
    spMat->setFromTriplets(tripletList.begin(), tripletList.end());
    spMat->makeCompressed();
    return res;
}
//=============================================================================
template <class T>
void*
deleteCols(indexType rows, indexType cols, const void* cp, const bool* dmap, indexType keepCols)
{
    const auto* spSrc = static_cast<const SparseMatrixT<T>*>(cp);
    std::vector<TripletT<T>> tripletList;
    tripletList.reserve(spSrc ? spSrc->nonZeros() : 0);
    indexType newCol = 0;
    for (indexType j = 0; j < cols; ++j) {
        if (dmap[j]) {
            continue;
        }
        if (spSrc) {
            for (typename SparseMatrixT<T>::InnerIterator it(*spSrc, j); it; ++it) {
                tripletList.emplace_back(it.row(), newCol, it.value());
            }
        }
        newCol++;
    }
    return sparseFromTriplets<T>(rows, keepCols, tripletList);
}
//=============================================================================
template <class T>
void*
deleteRows(
    indexType cols, const void* cp, const std::vector<signedIndexType>& rowMap, indexType keepRows)
{
    const auto* spSrc = static_cast<const SparseMatrixT<T>*>(cp);
    std::vector<TripletT<T>> tripletList;
    tripletList.reserve(spSrc ? spSrc->nonZeros() : 0);
    if (spSrc) {
        for (indexType j = 0; j < cols; ++j) {
            for (typename SparseMatrixT<T>::InnerIterator it(*spSrc, j); it; ++it) {
                signedIndexType nr = rowMap[it.row()];
                if (nr >= 0 && it.value() != T(0)) {
                    tripletList.emplace_back(nr, j, it.value());
                }
            }
        }
    }
    return sparseFromTriplets<T>(keepRows, cols, tripletList);
}
//=============================================================================
template <class T>
void*
deleteVectorSubset(indexType oldRows, const void* cp, const std::vector<char>& delmap,
    indexType newRows, indexType newCols)
{
    const auto* spSrc = static_cast<const SparseMatrixT<T>*>(cp);
    std::vector<TripletT<T>> tripletList;
    indexType t = 0;
    for (indexType k = 0; k < (indexType)delmap.size(); ++k) {
        if (delmap[k]) {
            continue;
        }
        T value = spSrc->coeff(k % oldRows, k / oldRows);
        if (value != T(0)) {
            tripletList.emplace_back(t % newRows, t / newRows, value);
        }
        t++;
    }
    return sparseFromTriplets<T>(newRows, newCols, tripletList);
}
//=============================================================================
