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
#include "Prod.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
RealProdT(const T* sp, T* dp, size_t planes, size_t planesize, size_t linesize, bool withnan)
{
    for (size_t i = 0; i < planes; i++) {
        for (size_t j = 0; j < planesize; j++) {
            T accum = 1;
            for (size_t k = 0; k < linesize; k++) {
                T val = sp[i * planesize * linesize + j + k * planesize];
                if (!withnan) {
                    if (!std::isnan(val)) {
                        accum *= val;
                    }
                } else {
                    accum *= val;
                }
            }
            dp[i * planesize + j] = accum;
        }
    }
}
//=============================================================================
template <class T>
void
ComplexProdT(const T* sp, T* dp, size_t planes, size_t planesize, size_t linesize, bool withnan)
{
    for (size_t i = 0; i < planes; i++) {
        for (size_t j = 0; j < planesize; j++) {
            T accum_r = 1;
            T accum_i = 0;
            for (size_t k = 0; k < linesize; k++) {
                T vr = sp[2 * (i * planesize * linesize + j + k * planesize)];
                T vi = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                if (!withnan) {
                    if (!std::isnan(vr) && !std::isnan(vi)) {
                        T t1 = accum_r * vr - accum_i * vi;
                        T t2 = accum_r * vi + accum_i * vr;
                        accum_r = t1;
                        accum_i = t2;
                    }
                } else {
                    T t1 = accum_r * vr - accum_i * vi;
                    T t2 = accum_r * vi + accum_i * vr;
                    accum_r = t1;
                    accum_i = t2;
                }
            }
            dp[2 * (i * planesize + j)] = accum_r;
            dp[2 * (i * planesize + j) + 1] = accum_i;
        }
    }
}
//=============================================================================
ArrayOf
Prod(ArrayOf A, indexType d, const std::wstring& strtype, bool withnan)
{
    ArrayOf res;
    Class classA = A.getDataClass();
    if (classA < NLS_LOGICAL || A.isSparse() || classA == NLS_CHAR) {
        std::wstring classname;
        ClassName(A, classname);
        std::wstring msg = _W("function") + L" " + classname + L"_prod" + L" " + _W("undefined.");
        Error(msg);
    }
    if (A.isEmpty(true) && A.is2D()) {
        res = ArrayOf::doubleConstructor(1);
    } else if (A.isScalar()) {
        res = A;
    } else {
        Dimensions dimsA = A.getDimensions();
        indexType workDim;
        if (d == 0) {
            size_t l = 0;
            while (dimsA[l] == 1) {
                l++;
            }
            workDim = l;
        } else {
            workDim = d - 1;
        }
        Dimensions dimsRes = dimsA;
        dimsRes.setDimensionLength(workDim, 1);
        dimsRes.simplify();
        size_t planecount;
        size_t planesize = 1;
        size_t linesize = dimsA[workDim];
        for (indexType l = 0; l < workDim; l++) {
            planesize *= dimsA[l];
        }
        planecount = 1;
        for (indexType l = workDim + 1; l < dimsA.getLength(); l++) {
            planecount *= dimsA[l];
        }
        switch (classA) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64: {
            A.promoteType(NLS_DOUBLE);
            double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(
                NLS_DOUBLE, dimsRes.getElementCount(), stringVector(), true));
            RealProdT<double>(static_cast<const double*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize, false);
            res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
            res.promoteType(classA);
        } break;
        case NLS_SINGLE: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            RealProdT<single>(static_cast<const single*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize, withnan);
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DOUBLE: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            RealProdT<double>(static_cast<const double*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize, withnan);
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_SCOMPLEX: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            ComplexProdT<single>(static_cast<const single*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize, withnan);
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DCOMPLEX: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            ComplexProdT<double>(static_cast<const double*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize, withnan);
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        default: {
            std::wstring classname;
            ClassName(A, classname);
            std::wstring msg
                = _W("function") + L" " + classname + L"_prod" + L" " + _W("undefined.");
            Error(msg);
        } break;
        }
    }
    if (strtype == L"default") {
        if (classA == NLS_DOUBLE || classA == NLS_SINGLE || classA == NLS_DCOMPLEX
            || classA == NLS_SCOMPLEX) {
            res.promoteType(classA);
        } else {
            res.promoteType(NLS_DOUBLE);
        }
    } else if (strtype == L"native") {
        res.promoteType(classA);
    } else if (strtype == L"double") {
        if (res.getDataClass() == NLS_SCOMPLEX || res.getDataClass() == NLS_DCOMPLEX) {
            res.promoteType(NLS_DCOMPLEX);
        } else {
            res.promoteType(NLS_DOUBLE);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
