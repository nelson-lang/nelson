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
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "AllFinite.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
AllFiniteComplex(const ArrayOf& A)
{
    // Scan the interleaved real/imaginary components directly. Mapping the data
    // into an Eigen expression and calling allFinite() reads whole SIMD packets,
    // which overruns the end of the (unpadded) storage for sizes that are not a
    // multiple of the packet width. Only stored values are scanned for sparse.
    const T* ptr = nullptr;
    size_t componentCount = 0;
    if (A.isSparse()) {
        auto* spMat
            = (Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>*)(A.sparseDataPointer());
        ptr = reinterpret_cast<const T*>(spMat->valuePtr());
        componentCount = (size_t)spMat->nonZeros() * 2;
    } else {
        ptr = reinterpret_cast<const T*>(A.dataPointer());
        componentCount = (size_t)A.elementCount() * 2;
    }
    for (size_t k = 0; k < componentCount; ++k) {
        if (!std::isfinite(ptr[k])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
AllFiniteReal(const ArrayOf& A)
{
    // See AllFiniteComplex: a plain bounds-exact scan avoids the SIMD packet
    // over-read that Eigen's allFinite() performs on an unpadded Map.
    const T* ptr = nullptr;
    size_t elementCount = 0;
    if (A.isSparse()) {
        auto* spMat = (Eigen::SparseMatrix<T, 0, signedIndexType>*)(A.sparseDataPointer());
        ptr = spMat->valuePtr();
        elementCount = (size_t)spMat->nonZeros();
    } else {
        ptr = (const T*)A.dataPointer();
        elementCount = (size_t)A.elementCount();
    }
    for (size_t k = 0; k < elementCount; ++k) {
        if (!std::isfinite(ptr[k])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
AllFinite(const ArrayOf& arrayIn, bool& needToOverload)
{
    needToOverload = false;
    switch (arrayIn.dataClass()) {
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    default: {
        needToOverload = true;
        return false;
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_LOGICAL:
    case NLS_CHAR:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64: {
        return true;
    } break;
    case NLS_SINGLE: {
        return AllFiniteReal<single>(arrayIn);
    } break;
    case NLS_DOUBLE: {
        return AllFiniteReal<double>(arrayIn);
    } break;
    case NLS_DCOMPLEX: {
        return AllFiniteComplex<double>(arrayIn);
    } break;
    case NLS_SCOMPLEX: {
        return AllFiniteComplex<single>(arrayIn);
    } break;
    }
    needToOverload = true;
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
