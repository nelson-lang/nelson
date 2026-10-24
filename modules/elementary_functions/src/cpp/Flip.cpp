//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <cstdint>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "Flip.hpp"
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isFlipSupportedType(NelsonType cl);
static void
flipDispatch(const char* sp, char* dp, size_t es, indexType stride, indexType D, indexType nOuter);
//=============================================================================
template <class T>
void
TFlipLR2dReal(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matIn(ptrIn, m, n);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOut(ptrOut, m, n);
    OMP_PARALLEL_FOR_LOOP(m * n)
    for (ompIndexType j = 0; j < (ompIndexType)(m * n); ++j) {
        ompIndexType row = j % m;
        ompIndexType col = j / m;
        ompIndexType flippedCol = n - 1 - col;
        matOut(row, col) = matIn(row, flippedCol);
    }
}
//=============================================================================
template <class T>
void
TFlipLR2dComplex(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    auto* Az = reinterpret_cast<std::complex<T>*>(ptrIn);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matIn(Az, m, n);
    auto* Cz = reinterpret_cast<std::complex<T>*>(ptrOut);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matOut(Cz, m, n);
    OMP_PARALLEL_FOR_LOOP(m * n)
    for (ompIndexType j = 0; j < (ompIndexType)(m * n); ++j) {
        ompIndexType row = j % m;
        ompIndexType col = j / m;
        ompIndexType flippedCol = n - 1 - col;
        matOut(row, col) = matIn(row, flippedCol);
    }
}
//=============================================================================
template <class T>
void
TFlipUD2dReal(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matIn(ptrIn, m, n);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOut(ptrOut, m, n);
    OMP_PARALLEL_FOR_LOOP(m * n)
    for (ompIndexType idx = 0; idx < (ompIndexType)(m * n); ++idx) {
        ompIndexType j = idx / m;
        ompIndexType i = idx % m;
        matOut(i, j) = matIn(m - 1 - i, j);
    }
}
//=============================================================================
template <class T>
void
TFlipUD2dComplex(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    auto* Az = reinterpret_cast<std::complex<T>*>(ptrIn);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matIn(Az, m, n);
    auto* Cz = reinterpret_cast<std::complex<T>*>(ptrOut);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matOut(Cz, m, n);
    OMP_PARALLEL_FOR_LOOP(m * n)
    for (ompIndexType idx = 0; idx < (ompIndexType)(m * n); ++idx) {
        ompIndexType j = idx / m;
        ompIndexType i = idx % m;
        matOut(i, j) = matIn(m - 1 - i, j);
    }
}
//=============================================================================
ArrayOf
Fliplr(const ArrayOf& arrayIn, bool& needToOverload)
{
    ArrayOf res;
    if (arrayIn.isEmpty() || arrayIn.isScalar()) {
        needToOverload = false;
        // flipping no elements gives an empty real array
        return arrayIn.isEmpty() ? decomplexify(arrayIn) : arrayIn;
    }
    if (arrayIn.isSparse() || !arrayIn.is2D()) {
        needToOverload = true;
        return res;
    }
    Dimensions dims = arrayIn.dimensions();
    indexType elementCount = dims.elementCount();
    NelsonType outType = arrayIn.dataClass();
    switch (outType) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    case NLS_LOGICAL: {
        needToOverload = false;
        logical* ptr = static_cast<logical*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<logical>((logical*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_UINT8: {
        needToOverload = false;
        uint8* ptr = static_cast<uint8*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint8>((uint8*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_INT8: {
        needToOverload = false;
        int8* ptr = static_cast<int8*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int8>((int8*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_UINT16: {
        needToOverload = false;
        uint16* ptr = static_cast<uint16*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint16>((uint16*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_INT16: {
        needToOverload = false;
        int16* ptr = static_cast<int16*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int16>((int16*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_UINT32: {
        needToOverload = false;
        uint32* ptr = static_cast<uint32*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint32>((uint32*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_INT32: {
        needToOverload = false;
        int32* ptr = static_cast<int32*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int32>((int32*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_UINT64: {
        needToOverload = false;
        uint64* ptr = static_cast<uint64*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint64>((uint64*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_INT64: {
        needToOverload = false;
        int64* ptr = static_cast<int64*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int64>((int64*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_SINGLE: {
        needToOverload = false;
        single* ptr = static_cast<single*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<single>((single*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_DOUBLE: {
        needToOverload = false;
        double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<double>((double*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_SCOMPLEX: {
        needToOverload = false;
        single* ptr = static_cast<single*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dComplex<single>((single*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_DCOMPLEX: {
        needToOverload = false;
        double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dComplex<double>((double*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_CHAR: {
        needToOverload = false;
        charType* ptr = static_cast<charType*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<charType>((charType*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
Flipud(const ArrayOf& arrayIn, bool& needToOverload)
{
    ArrayOf res;
    if (arrayIn.isEmpty()) {
        needToOverload = false;
        // flipping no elements gives an empty real array
        return decomplexify(arrayIn);
    }
    if (arrayIn.isSparse() || !arrayIn.is2D()) {
        needToOverload = true;
        return res;
    }
    Dimensions dims = arrayIn.dimensions();
    indexType elementCount = dims.elementCount();
    NelsonType outType = arrayIn.dataClass();
    switch (outType) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    case NLS_LOGICAL: {
        needToOverload = false;
        logical* ptr = static_cast<logical*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<logical>((logical*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_UINT8: {
        needToOverload = false;
        uint8* ptr = static_cast<uint8*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint8>((uint8*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_INT8: {
        needToOverload = false;
        int8* ptr = static_cast<int8*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int8>((int8*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_UINT16: {
        needToOverload = false;
        uint16* ptr = static_cast<uint16*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint16>((uint16*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_INT16: {
        needToOverload = false;
        int16* ptr = static_cast<int16*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int16>((int16*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_UINT32: {
        needToOverload = false;
        uint32* ptr = static_cast<uint32*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint32>((uint32*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_INT32: {
        needToOverload = false;
        int32* ptr = static_cast<int32*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int32>((int32*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_UINT64: {
        needToOverload = false;
        uint64* ptr = static_cast<uint64*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint64>((uint64*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_INT64: {
        needToOverload = false;
        int64* ptr = static_cast<int64*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int64>((int64*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_SINGLE: {
        needToOverload = false;
        single* ptr = static_cast<single*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<single>((single*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_DOUBLE: {
        needToOverload = false;
        double* ptr = static_cast<double*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<double>((double*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_SCOMPLEX: {
        needToOverload = false;
        single* ptr = static_cast<single*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dComplex<single>((single*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_DCOMPLEX: {
        needToOverload = false;
        double* ptr = static_cast<double*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dComplex<double>((double*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    case NLS_CHAR: {
        needToOverload = false;
        charType* ptr = static_cast<charType*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<charType>((charType*)arrayIn.dataPointer(), ptr, dims.rows(), dims.columns());
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
Flip(const ArrayOf& arrayIn, indexType dim, bool& needToOverload)
{
    needToOverload = false;
    NelsonType cl = arrayIn.dataClass();
    if (arrayIn.isSparse() || !isFlipSupportedType(cl)) {
        needToOverload = true;
        return {};
    }
    if (arrayIn.isEmpty() || arrayIn.isScalar()) {
        // flipping no elements gives an empty real array
        ArrayOf res(arrayIn.isEmpty() ? decomplexify(arrayIn) : arrayIn);
        res.ensureSingleOwner();
        return res;
    }
    Dimensions dims = arrayIn.dimensions();
    indexType nd = dims.length();
    indexType D = (dim >= 1 && dim <= nd) ? dims.at(dim - 1, false) : 1;
    if (D <= 1) {
        // flipping a singleton dimension is the identity
        ArrayOf res(arrayIn);
        res.ensureSingleOwner();
        return res;
    }
    indexType stride = 1;
    for (indexType d = 0; d + 1 < dim; ++d) {
        stride *= dims.at(d, false);
    }
    indexType total = dims.elementCount();
    indexType nOuter = total / (stride * D);

    void* out = ArrayOf::allocateArrayOf(cl, total);
    ArrayOf res(cl, dims, out);
    flipDispatch((const char*)arrayIn.dataPointer(), (char*)out, (size_t)arrayIn.elementSize(),
        stride, D, nOuter);
    return res;
}
//=============================================================================
static bool
isFlipSupportedType(NelsonType cl)
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
// 16-byte element (double complex) block for the typed reverse path.
struct FlipBlk16
{
    uint64_t a;
    uint64_t b;
};
//=============================================================================
// stride == 1: reverse each contiguous segment of length D in place-to-out.
template <typename T>
static void
flipReverseSegments(const T* sp, T* dp, indexType D, indexType nOuter)
{
    OMP_PARALLEL_FOR_LOOP(nOuter)
    for (ompIndexType b = 0; b < (ompIndexType)nOuter; ++b) {
        const T* in = sp + (indexType)b * D;
        T* out = dp + (indexType)b * D;
        for (indexType c = 0; c < D; ++c) {
            out[c] = in[D - 1 - c];
        }
    }
}
//=============================================================================
// stride > 1: each (block, position-along-dim) is a contiguous run of `stride`
// elements; output run r reads input run (b*D + D-1-c) with r = b*D + c.
static void
flipRunsBytes(const char* sp, char* dp, size_t es, indexType stride, indexType D, indexType nOuter)
{
    indexType runs = nOuter * D;
    size_t runBytes = (size_t)stride * es;
    OMP_PARALLEL_FOR_LOOP(runs)
    for (ompIndexType r = 0; r < (ompIndexType)runs; ++r) {
        indexType b = (indexType)r / D;
        indexType c = (indexType)r % D;
        indexType srcRun = b * D + (D - 1 - c);
        std::memcpy(dp + (size_t)r * runBytes, sp + (size_t)srcRun * runBytes, runBytes);
    }
}
//=============================================================================
static void
flipDispatch(const char* sp, char* dp, size_t es, indexType stride, indexType D, indexType nOuter)
{
    if (stride != 1) {
        flipRunsBytes(sp, dp, es, stride, D, nOuter);
        return;
    }
    switch (es) {
    case 1:
        flipReverseSegments<uint8_t>((const uint8_t*)sp, (uint8_t*)dp, D, nOuter);
        break;
    case 2:
        flipReverseSegments<uint16_t>((const uint16_t*)sp, (uint16_t*)dp, D, nOuter);
        break;
    case 4:
        flipReverseSegments<uint32_t>((const uint32_t*)sp, (uint32_t*)dp, D, nOuter);
        break;
    case 8:
        flipReverseSegments<uint64_t>((const uint64_t*)sp, (uint64_t*)dp, D, nOuter);
        break;
    case 16:
        flipReverseSegments<FlipBlk16>((const FlipBlk16*)sp, (FlipBlk16*)dp, D, nOuter);
        break;
    default:
        flipRunsBytes(sp, dp, es, 1, D, nOuter);
        break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
