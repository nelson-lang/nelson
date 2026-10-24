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
#include <cstdio>
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include "IsHermitian.hpp"
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
isHermitianComplex(T* data, indexType N, bool skew)
{
    if (N == 1) {
        return false;
    }
    if (skew) {
        for (indexType k = 0; k < N; k++) {
            if (data[2 * (k + k * N)] != 0) {
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
                if (imagA != imagB) {
                    return false;
                }
            } else {
                if (realA != realB) {
                    return false;
                }
                if (imagA != -imagB) {
                    return false;
                }
            }
        }
    }
    return true;
}
//=============================================================================
static bool
IsHermitianInternal(const ArrayOf& A, bool skew, bool& needToOverload)
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
    case NLS_SCOMPLEX:
        return isHermitianComplex<single>((single*)A.dataPointer(), A.rows(), skew);
    case NLS_DCOMPLEX:
        return isHermitianComplex<double>((double*)A.dataPointer(), A.rows(), skew);
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        if (skew) {
            return IsSymmetricWithSkew(A, needToOverload);
        }
        return IsSymmetricWithoutSkew(A, needToOverload);

    } break;
    default: {
        needToOverload = true;
        res = false;
    } break;
    }
    return res;
}
//=============================================================================
bool
IsHermitianWithSkew(const ArrayOf& A, bool& needToOverload)
{
    return IsHermitianInternal(A, true, needToOverload);
}
//=============================================================================
bool
IsHermitianWithoutSkew(const ArrayOf& A, bool& needToOverload)
{
    return IsHermitianInternal(A, false, needToOverload);
}
//=============================================================================
bool
IsHermitian(const ArrayOf& A, bool skew, const std::string& functionName)
{
    bool needToOverload;
    bool res = IsHermitianInternal(A, skew, needToOverload);
    if (needToOverload) {
        std::string errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate("Nelson:linear_algebra:undefinedFunctionInputType",
                "Undefined function '%s' for input arguments of type '%s'."),
            functionName, ClassName(A));
        Error("Nelson:ishermitian:invalidInput", errorMessage);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
