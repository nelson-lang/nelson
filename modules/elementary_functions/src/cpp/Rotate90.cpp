//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdint>
#include <cstring>
#include <memory>
#include <vector>
#include <Eigen/Sparse>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Rotate90.hpp"
#include "SparseArrayOf.hpp"
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isRotatePodSupportedType(NelsonType cl);
static bool
isRotateReferenceSupportedType(const ArrayOf& arrayIn);
static Dimensions
rotatedDimensions(const Dimensions& dims, int k);
static ArrayOf
rotate90Reference(const ArrayOf& arrayIn, int k, const Dimensions& outDims);
static ArrayOf
rotate90Sparse(const ArrayOf& arrayIn, int k, const Dimensions& outDims, bool& needToOverload);
static void
rotate90Dispatch(
    const char* sp, char* dp, size_t es, indexType M, indexType N, indexType nPages, int k);
//=============================================================================
ArrayOf
Rotate90(const ArrayOf& arrayIn, int nbRotations, bool& needToOverload)
{
    needToOverload = false;
    int k = ((nbRotations % 4) + 4) % 4;
    const NelsonType cl = arrayIn.dataClass();
    const Dimensions dims = arrayIn.dimensions();
    const Dimensions outDims = rotatedDimensions(dims, k);
    if (arrayIn.isTable() || arrayIn.isTimetable()) {
        needToOverload = true;
        return {};
    }
    if (arrayIn.isSparse()) {
        return rotate90Sparse(arrayIn, k, outDims, needToOverload);
    }
    if (isRotateReferenceSupportedType(arrayIn)) {
        return rotate90Reference(arrayIn, k, outDims);
    }
    if (!isRotatePodSupportedType(cl)) {
        needToOverload = true;
        return {};
    }
    if (arrayIn.isEmpty()) {
        // rotating no elements gives an empty real array
        ArrayOf res = ArrayOf::empty(k == 0 ? dims : outDims);
        res.promoteType(cl);
        return decomplexify(res);
    }
    if (k == 0 || arrayIn.isScalar()) {
        ArrayOf res(arrayIn);
        res.ensureSingleOwner();
        return res;
    }
    indexType M = dims.rows();
    indexType N = dims.columns();
    indexType nd = dims.length();
    indexType nPages = 1;
    for (indexType d = 2; d < nd; ++d) {
        nPages *= dims.at(d, false);
    }
    void* out = ArrayOf::allocateArrayOf(cl, dims.elementCount());
    ArrayOf res(cl, outDims, out);
    rotate90Dispatch((const char*)arrayIn.dataPointer(), (char*)out, (size_t)arrayIn.elementSize(),
        M, N, nPages, k);
    return res;
}
//=============================================================================
static bool
isRotatePodSupportedType(NelsonType cl)
{
    switch (cl) {
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_CHAR:
        return true;
    default:
        return false;
    }
}
//=============================================================================
static bool
isRotateReferenceSupportedType(const ArrayOf& arrayIn)
{
    return arrayIn.isStringArray() || arrayIn.isCell() || arrayIn.isStruct()
        || arrayIn.isClassType() || arrayIn.isHandle();
}
//=============================================================================
static Dimensions
rotatedDimensions(const Dimensions& dims, int k)
{
    if (k == 2) {
        return dims;
    }
    Dimensions outDims;
    outDims[0] = dims.columns();
    outDims[1] = dims.rows();
    for (indexType d = 2; d < dims.length(); ++d) {
        outDims[d] = dims.at(d, false);
    }
    outDims.simplify();
    return outDims;
}
//=============================================================================
static ArrayOf
makeReferenceOutput(const ArrayOf& arrayIn, const Dimensions& outDims)
{
    void* out = ArrayOf::allocateArrayOf(
        arrayIn.dataClass(), outDims.elementCount(), arrayIn.fieldNames(), true);
    ArrayOf res(arrayIn.dataClass(), outDims, out, false, arrayIn.fieldNames());
    if (arrayIn.isClassType()) {
        res.setClassType(arrayIn.classType());
        if (arrayIn.dataClass() == NLS_CLASSDEF_ARRAY) {
            res.setClassdefLayout(arrayIn.classdefLayout());
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
rotate90Reference(const ArrayOf& arrayIn, int k, const Dimensions& outDims)
{
    if (k == 0 || arrayIn.isScalar()) {
        ArrayOf res(arrayIn);
        res.ensureSingleOwner();
        return res;
    }
    ArrayOf res = makeReferenceOutput(arrayIn, outDims);
    if (arrayIn.isEmpty()) {
        return res;
    }
    ArrayOf src = arrayIn;
    void* out = const_cast<void*>(res.dataPointer());
    const Dimensions dims = arrayIn.dimensions();
    const indexType M = dims.rows();
    const indexType N = dims.columns();
    const indexType MN = M * N;
    indexType nPages = 1;
    for (indexType d = 2; d < dims.length(); ++d) {
        nPages *= dims.at(d, false);
    }
    for (indexType p = 0; p < nPages; ++p) {
        const indexType pageOffset = p * MN;
        for (indexType c = 0; c < N; ++c) {
            for (indexType r = 0; r < M; ++r) {
                indexType dst = 0;
                if (k == 1) {
                    dst = (N - 1 - c) + r * N;
                } else if (k == 2) {
                    dst = (M - 1 - r) + (N - 1 - c) * M;
                } else {
                    dst = c + (M - 1 - r) * N;
                }
                src.copyElements(pageOffset + r + c * M, out, pageOffset + dst, 1);
            }
        }
    }
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
rotate90SparseTyped(const ArrayOf& arrayIn, int k, const Dimensions& outDims)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    const auto* src = static_cast<const SparseMatrix*>(arrayIn.sparseDataPointer());
    auto dst = std::make_unique<SparseMatrix>(static_cast<signedIndexType>(outDims.rows()),
        static_cast<signedIndexType>(outDims.columns()));
    if (src != nullptr && src->nonZeros() > 0) {
        const signedIndexType M = static_cast<signedIndexType>(arrayIn.rows());
        const signedIndexType N = static_cast<signedIndexType>(arrayIn.columns());
        std::vector<Triplet> triplets;
        triplets.reserve(static_cast<size_t>(src->nonZeros()));
        for (signedIndexType col = 0; col < src->outerSize(); ++col) {
            for (typename SparseMatrix::InnerIterator it(*src, col); it; ++it) {
                if (k == 1) {
                    triplets.emplace_back(N - 1 - it.col(), it.row(), it.value());
                } else if (k == 2) {
                    triplets.emplace_back(M - 1 - it.row(), N - 1 - it.col(), it.value());
                } else {
                    triplets.emplace_back(it.col(), M - 1 - it.row(), it.value());
                }
            }
        }
        dst->setFromTriplets(triplets.begin(), triplets.end());
    }
    return makeSparseResult<T>(arrayIn.dataClass(), outDims, std::move(dst));
}
//=============================================================================
static ArrayOf
rotate90Sparse(const ArrayOf& arrayIn, int k, const Dimensions& outDims, bool& needToOverload)
{
    needToOverload = false;
    if (!arrayIn.is2D()) {
        needToOverload = true;
        return {};
    }
    if (k == 0 || arrayIn.isScalar()) {
        ArrayOf res(arrayIn);
        res.ensureSingleOwner();
        return res;
    }
    switch (arrayIn.dataClass()) {
    case NLS_LOGICAL:
        return rotate90SparseTyped<logical>(arrayIn, k, outDims);
    case NLS_DOUBLE:
        return rotate90SparseTyped<double>(arrayIn, k, outDims);
    case NLS_SINGLE:
        return rotate90SparseTyped<single>(arrayIn, k, outDims);
    case NLS_DCOMPLEX:
        return rotate90SparseTyped<doublecomplex>(arrayIn, k, outDims);
    case NLS_SCOMPLEX:
        return rotate90SparseTyped<singlecomplex>(arrayIn, k, outDims);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
struct RotBlk16
{
    uint64_t a;
    uint64_t b;
};
//=============================================================================
// k == 2: each page is the reverse of the input page.
template <typename T>
static void
rotate180(const T* A, T* B, indexType MN, indexType nPages)
{
    indexType total = MN * nPages;
    OMP_PARALLEL_FOR_LOOP(total)
    for (ompIndexType e = 0; e < (ompIndexType)total; ++e) {
        indexType p = (indexType)e / MN;
        indexType l = (indexType)e % MN;
        B[e] = A[p * MN + (MN - 1 - l)];
    }
}
//=============================================================================
// k == 1 / k == 3: fused, cache-tiled transpose-with-reverse. The output page
// is N x M and is written with unit stride (contiguous), reading the input page
// with stride M inside each tile so both sides stay cache-resident.
//   k == 1: B(i,j) = A(j, N-1-i)      -> B[i + j*N] = A[j + (N-1-i)*M]
//   k == 3: B(i,j) = A(M-1-j, i)      -> B[i + j*N] = A[(M-1-j) + i*M]
template <typename T>
static void
rotate90Tiled(const T* A, T* B, indexType M, indexType N, indexType nPages, int k)
{
    const indexType TILE = 32;
    indexType MN = M * N;
    indexType nTi = (N + TILE - 1) / TILE;
    indexType nTj = (M + TILE - 1) / TILE;
    indexType tilesPerPage = nTi * nTj;
    indexType total = nPages * tilesPerPage;
    OMP_PARALLEL_FOR_LOOP(total)
    for (ompIndexType o = 0; o < (ompIndexType)total; ++o) {
        indexType p = (indexType)o / tilesPerPage;
        indexType t = (indexType)o % tilesPerPage;
        indexType i0 = (t / nTj) * TILE;
        indexType ie = (i0 + TILE < N) ? i0 + TILE : N;
        indexType j0 = (t % nTj) * TILE;
        indexType je = (j0 + TILE < M) ? j0 + TILE : M;
        const T* Ap = A + p * MN;
        T* Bp = B + p * MN;
        if (k == 1) {
            for (indexType j = j0; j < je; ++j) {
                for (indexType i = i0; i < ie; ++i) {
                    Bp[i + j * N] = Ap[j + (N - 1 - i) * M];
                }
            }
        } else {
            for (indexType j = j0; j < je; ++j) {
                for (indexType i = i0; i < ie; ++i) {
                    Bp[i + j * N] = Ap[(M - 1 - j) + i * M];
                }
            }
        }
    }
}
//=============================================================================
template <typename T>
static void
rotate90Typed(const char* sp, char* dp, indexType M, indexType N, indexType nPages, int k)
{
    const T* A = (const T*)sp;
    T* B = (T*)dp;
    if (k == 2) {
        rotate180<T>(A, B, M * N, nPages);
    } else {
        rotate90Tiled<T>(A, B, M, N, nPages, k);
    }
}
//=============================================================================
static void
rotate90Dispatch(
    const char* sp, char* dp, size_t es, indexType M, indexType N, indexType nPages, int k)
{
    switch (es) {
    case 1:
        rotate90Typed<uint8_t>(sp, dp, M, N, nPages, k);
        break;
    case 2:
        rotate90Typed<uint16_t>(sp, dp, M, N, nPages, k);
        break;
    case 4:
        rotate90Typed<uint32_t>(sp, dp, M, N, nPages, k);
        break;
    case 8:
        rotate90Typed<uint64_t>(sp, dp, M, N, nPages, k);
        break;
    case 16:
        rotate90Typed<RotBlk16>(sp, dp, M, N, nPages, k);
        break;
    default:
        break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
