//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#define _SCL_SECURE_NO_WARNINGS
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include <cstdio>
#include "IsSymmetric.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
isSymmetricComplex(T* data, indexType N, bool skew)
{
    if (skew) {
        for (indexType k = 0; k < N; k++) {
            if (data[2 * (k + k * N)] != 0 || data[2 * (k + k * N) + 1] != 0) {
                return false;
            }
        }
    }
    for (indexType col = 1; col < N; col++) {
        for (indexType row = 0; row < col; row++) {
            T realA = data[2 * (row + col * N)];
            T realB = data[2 * (col + row * N)];
            T imagA = data[2 * (row + col * N) + 1];
            T imagB = data[2 * (col + row * N) + 1];
            if (skew) {
                if (realA != -realB) {
                    return false;
                }
                if (imagA != -imagB) {
                    return false;
                }
            } else {
                if (realA != realB) {
                    return false;
                }
                if (imagA != imagB) {
                    return false;
                }
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isSymmetricNoSkew(T* data, indexType N)
{
    for (indexType col = 1; col < N; col++) {
        for (indexType row = 0; row < col; row++) {
            T realA = data[row + col * N];
            T realB = data[col + row * N];
            if (realA != realB) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isSymmetric(T* data, indexType N, bool skew)
{
    if (skew) {
        for (indexType k = 0; k < N; k++) {
            if (data[k + k * N] != 0) {
                return false;
            }
        }
    }
    for (indexType col = 1; col < N; col++) {
        for (indexType row = 0; row < col; row++) {
            T realA = data[row + col * N];
            T realB = data[col + row * N];
            if (skew) {
                if (realA != -realB) {
                    return false;
                }
            } else {
                if (realA != realB) {
                    return false;
                }
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isSymmetric(T* data, indexType N, double tol)
{
    for (indexType col = 1; col < N; col++) {
        for (indexType row = 0; row < col; row++) {
            T realA = data[row + col * N];
            T realB = data[col + row * N];
            if (double(realA - realB) > tol) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isSymmetricComplex(T* data, indexType N, double tol)
{
    for (indexType col = 1; col < N; col++) {
        for (indexType row = 0; row < col; row++) {
            T realA = data[2 * (row + col * N)];
            T realB = data[2 * (col + row * N)];
            T imagA = data[2 * (row + col * N) + 1];
            T imagB = data[2 * (col + row * N) + 1];
            if (realA - realB > (T)tol) {
                return false;
            }
            if (imagA - imagB > (T)tol) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
static bool
IsSymmetricInternal(const ArrayOf& A, bool skew, bool& needToOverload)
{
    needToOverload = false;
    bool res = false;
    if (!A.is2D()) {
        return false;
    }
    if (!A.isSquare()) {
        return false;
    }
    switch (A.dataClass()) {
    case NLS_LOGICAL: {
        if (skew) {
            return false;
        }
        return isSymmetricNoSkew<uint8>((uint8*)A.dataPointer(), A.rows());
    }
    case NLS_SINGLE:
        return isSymmetric<single>((single*)A.dataPointer(), A.rows(), skew);
    case NLS_DOUBLE:
        return isSymmetric<double>((double*)A.dataPointer(), A.rows(), skew);
    case NLS_SCOMPLEX:
        return isSymmetricComplex<single>((single*)A.dataPointer(), A.rows(), skew);
    case NLS_DCOMPLEX:
        return isSymmetricComplex<double>((double*)A.dataPointer(), A.rows(), skew);
    case NLS_INT8:
        return isSymmetric<int8>((int8*)A.dataPointer(), A.rows(), skew);
    case NLS_INT16:
        return isSymmetric<int16>((int16*)A.dataPointer(), A.rows(), skew);
    case NLS_INT32:
        return isSymmetric<int32>((int32*)A.dataPointer(), A.rows(), skew);
    case NLS_INT64:
        return isSymmetric<int64>((int64*)A.dataPointer(), A.rows(), skew);
    case NLS_UINT8: {
        if (skew) {
            return false;
        }
        return isSymmetricNoSkew<uint8>((uint8*)A.dataPointer(), A.rows());
    }
    case NLS_UINT16: {
        if (skew) {
            return false;
        }
        return isSymmetricNoSkew<uint16>((uint16*)A.dataPointer(), A.rows());
    }
    case NLS_UINT32: {
        if (skew) {
            return false;
        }
        return isSymmetricNoSkew<uint32>((uint32*)A.dataPointer(), A.rows());
    }
    case NLS_UINT64: {
        if (skew) {
            return false;
        }
        return isSymmetricNoSkew<uint64>((uint64*)A.dataPointer(), A.rows());
    }
    default: {
        needToOverload = true;
        res = false;
    } break;
    }
    return res;
}
//=============================================================================
bool
IsSymmetricWithSkew(const ArrayOf& A, bool& needToOverload)
{
    return IsSymmetricInternal(A, true, needToOverload);
}
//=============================================================================
bool
IsSymmetricWithoutSkew(const ArrayOf& A, bool& needToOverload)
{
    return IsSymmetricInternal(A, false, needToOverload);
}
//=============================================================================
bool
IsSymmetric(const ArrayOf& A, bool skew, const std::string& functionName)
{
    bool needToOverload;
    bool res;
    if (skew) {
        res = IsSymmetricWithSkew(A, needToOverload);
    } else {
        res = IsSymmetricWithoutSkew(A, needToOverload);
    }
    if (needToOverload) {
        std::string errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate("Nelson:linear_algebra:undefinedFunctionInputType",
                "Undefined function '%s' for input arguments of type '%s'."),
            functionName, ClassName(A));
        Error("Nelson:issymmetric:invalidInput", errorMessage);
    }
    return res;
}
//=============================================================================
bool
IsSymmetric(const ArrayOf& A, double tol, bool& needToOverload)
{
    needToOverload = false;
    bool res = false;
    if (!A.is2D()) {
        return false;
    }
    if (!A.isSquare()) {
        return false;
    }
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return isSymmetric<uint8>((uint8*)A.dataPointer(), A.rows(), tol);
    case NLS_SINGLE:
        return isSymmetric<single>((single*)A.dataPointer(), A.rows(), tol);
    case NLS_DOUBLE:
        return isSymmetric<double>((double*)A.dataPointer(), A.rows(), tol);
    case NLS_SCOMPLEX:
        return isSymmetricComplex<single>((single*)A.dataPointer(), A.rows(), tol);
    case NLS_DCOMPLEX:
        return isSymmetricComplex<double>((double*)A.dataPointer(), A.rows(), tol);
    case NLS_INT8:
        return isSymmetric<int8>((int8*)A.dataPointer(), A.rows(), tol);
    case NLS_INT16:
        return isSymmetric<int16>((int16*)A.dataPointer(), A.rows(), tol);
    case NLS_INT32:
        return isSymmetric<int32>((int32*)A.dataPointer(), A.rows(), tol);
    case NLS_INT64:
        return isSymmetric<int64>((int64*)A.dataPointer(), A.rows(), tol);
    case NLS_UINT8:
        return isSymmetric<uint8>((uint8*)A.dataPointer(), A.rows(), tol);
    case NLS_UINT16:
        return isSymmetric<uint16>((uint16*)A.dataPointer(), A.rows(), tol);
    case NLS_UINT32:
        return isSymmetric<uint32>((uint32*)A.dataPointer(), A.rows(), tol);
    case NLS_UINT64:
        return isSymmetric<uint64>((uint64*)A.dataPointer(), A.rows(), tol);
    default: {
        needToOverload = true;
        res = false;
    } break;
    }
    return res;
}
//=============================================================================
bool
IsSymmetric(const ArrayOf& A, double tol, const std::string& functionName)
{
    bool needToOverload;
    bool res = IsSymmetric(A, tol, needToOverload);
    if (needToOverload) {
        std::string errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate("Nelson:linear_algebra:undefinedFunctionInputType",
                "Undefined function '%s' for input arguments of type '%s'."),
            functionName, ClassName(A));
        Error("Nelson:issymmetric:invalidInput", errorMessage);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
