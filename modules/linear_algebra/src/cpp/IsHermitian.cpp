//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#define _SCL_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <cstdio>
#include "nlsConfig.h"
#include "lapack_eigen.hpp"
#include "IsHermitian.hpp"
#include "IsSymmetric.hpp"
#include "ClassName.hpp"
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
bool
IsHermitian(const ArrayOf& A, bool skew, bool& needToOverload)
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
        return IsSymmetric(A, skew, needToOverload);
    case NLS_DOUBLE:
        return IsSymmetric(A, skew, needToOverload);
    case NLS_INT8:
        return IsSymmetric(A, skew, needToOverload);
    case NLS_INT16:
        return IsSymmetric(A, skew, needToOverload);
    case NLS_INT32:
        return IsSymmetric(A, skew, needToOverload);
    case NLS_INT64:
        return IsSymmetric(A, skew, needToOverload);
    case NLS_UINT8: {
        return IsSymmetric(A, skew, needToOverload);
    }
    case NLS_UINT16: {
        return IsSymmetric(A, skew, needToOverload);
    }
    case NLS_UINT32: {
        return IsSymmetric(A, skew, needToOverload);
    }
    case NLS_UINT64: {
        return IsSymmetric(A, skew, needToOverload);
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
IsHermitian(const ArrayOf& A, bool skew, const std::string& functionName)
{
    bool needToOverload;
    bool res = IsHermitian(A, skew, needToOverload);
    if (needToOverload) {
        char errorBuffer[1024];
        std::string fmt = _("Undefined function '%s' for input arguments of type '%s'");
        sprintf(errorBuffer, fmt.c_str(), functionName.c_str(), ClassName(A).c_str());
        Error(errorBuffer);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
