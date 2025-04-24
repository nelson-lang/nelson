//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
    for (indexType i = 0; i < N; i++) {
        for (indexType j = 1; j < N; j++) {
            T realA = data[2 * (i + j * N)];
            T realB = data[2 * (j + i * N)];
            T imagA = data[2 * (i + j * N) + 1];
            T imagB = data[2 * (j + i * N) + 1];
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
    switch (A.getDataClass()) {
    case NLS_SCOMPLEX:
        return isHermitianComplex<single>((single*)A.getDataPointer(), A.getRows(), skew);
    case NLS_DCOMPLEX:
        return isHermitianComplex<double>((double*)A.getDataPointer(), A.getRows(), skew);
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
        std::string errorMessage
            = fmt::format(_("Undefined function '{}' for input arguments of type '{}'"),
                functionName, ClassName(A));
        Error(errorMessage);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
