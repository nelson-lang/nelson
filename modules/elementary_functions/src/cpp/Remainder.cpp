//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Remainder.hpp"
#include "ClassName.hpp"
#include "MatrixCheck.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T
FLOOR_OR_X(T x)
{
    return (std::isfinite(x) ? std::floor(x + 0.5) : x);
}
//=============================================================================
template <class T>
T
nelson_rem_integer(T x, T y)
{
    return y != 0 ? x % y : 0;
}
//=============================================================================
template <class T>
T
nelson_rem(T x, T y)
{
    T res = x;
    if (y != 0) {
        T q = x / y;
        T eps = std::numeric_limits<T>::epsilon();
        if (FLOOR_OR_X(y) != y && (std::abs((q - FLOOR_OR_X(q)) / FLOOR_OR_X(q)) < eps)) {
            res = 0;
        } else {
            T n = std::trunc(q);
            T tmp = y * n;
            res = x - tmp;
        }
    } else {
        res = std::nan("");
    }
    return res;
}
//=============================================================================
template <class T>
void
remfunc(indexType N, T* C, const T* A, int stride1, const T* B, int stride2)
{
    if ((stride1 == 1) && (stride2 == 1)) {
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = nelson_rem(A[i], B[i]);
        }
    } else {
        if (stride1) {
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                C[i] = nelson_rem(A[i], B[0]);
            }
        } else {
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                C[i] = nelson_rem(A[0], B[i]);
            }
        }
    }
}
//=============================================================================
template <class T>
void
remfunc_integer(indexType N, T* C, const T* A, int stride1, const T* B, int stride2)
{
    if ((stride1 == 1) && (stride2 == 1)) {
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = nelson_rem_integer(A[i], B[i]);
        }
    } else {
        if (stride1) {
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                C[i] = nelson_rem_integer(A[i], B[0]);
            }
        } else {
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                C[i] = nelson_rem_integer(A[0], B[i]);
            }
        }
    }
}
//=============================================================================
static void
computeEntries(const ArrayOf& A, const ArrayOf& B, int& Astride, int& Bstride, indexType& Clen,
    Dimensions& Cdim)
{
    if (A.isScalar()) {
        Astride = 0;
        Bstride = 1;
        Cdim = B.getDimensions();
    } else if (B.isScalar()) {
        Astride = 1;
        Bstride = 0;
        Cdim = A.getDimensions();
    } else {
        Astride = 1;
        Bstride = 1;
        Cdim = A.getDimensions();
    }
    Clen = Cdim.getElementCount();
}
//=============================================================================
ArrayOf
Remainder(ArrayOf A, ArrayOf B)
{
    ArrayOf M;
    int Astride = 0;
    int Bstride = 0;
    indexType Clen = 0;
    Dimensions Cdim;
    if (!A.isNumeric() || !B.isNumeric()) {
        Error(_W("Input arguments must be numeric."));
    } else if (A.isComplex() || B.isComplex()) {
        Error(_("Undefined function 'rem' for complex input argument."));
    } else if (A.isIntegerType() || B.isIntegerType()) {
        if (A.isSparse() || B.isSparse()) {
            Error(_("Undefined function 'mod' for sparse input argument."));
        }
        VectorCheck(A, B, "rem");
        computeEntries(A, B, Astride, Bstride, Clen, Cdim);
        NelsonType classA = A.getDataClass();
        NelsonType classB = B.getDataClass();
        if (classB == NLS_DOUBLE || classB == NLS_SINGLE) {
            classB = classA;
        }
        if (classA == NLS_DOUBLE || classA == NLS_SINGLE) {
            classA = classB;
        }
        if (classA == classB) {
            A.promoteType(classA);
            B.promoteType(classB);
            void* Cp;
            switch (classA) {
            case NLS_INT8: {
                Cp = new_with_exception<int8>(Clen, false);
                remfunc_integer<int8>(Clen, (int8*)Cp, (int8*)A.getDataPointer(), Astride,
                    (int8*)B.getDataPointer(), Bstride);
            } break;
            case NLS_UINT8: {
                Cp = new_with_exception<uint8>(Clen, false);
                remfunc_integer<uint8>(Clen, (uint8*)Cp, (uint8*)A.getDataPointer(), Astride,
                    (uint8*)B.getDataPointer(), Bstride);
            } break;
            case NLS_INT16: {
                Cp = new_with_exception<int16>(Clen, false);
                remfunc_integer<int16>(Clen, (int16*)Cp, (int16*)A.getDataPointer(), Astride,
                    (int16*)B.getDataPointer(), Bstride);
            } break;
            case NLS_UINT16: {
                Cp = new_with_exception<uint16>(Clen, false);
                remfunc_integer<uint16>(Clen, (uint16*)Cp, (uint16*)A.getDataPointer(), Astride,
                    (uint16*)B.getDataPointer(), Bstride);
            } break;
            case NLS_INT32: {
                Cp = new_with_exception<int32>(Clen, false);
                remfunc_integer<int32>(Clen, (int32*)Cp, (int32*)A.getDataPointer(), Astride,
                    (int32*)B.getDataPointer(), Bstride);
            } break;
            case NLS_UINT32: {
                Cp = new_with_exception<uint32>(Clen, false);
                remfunc_integer<uint32>(Clen, (uint32*)Cp, (uint32*)A.getDataPointer(), Astride,
                    (uint32*)B.getDataPointer(), Bstride);
            } break;
            case NLS_INT64: {
                Cp = new_with_exception<int64>(Clen, false);
                remfunc_integer<int64>(Clen, (int64*)Cp, (int64*)A.getDataPointer(), Astride,
                    (int64*)B.getDataPointer(), Bstride);
            } break;
            case NLS_UINT64: {
                Cp = new_with_exception<uint64>(Clen, false);
                remfunc_integer<uint64>(Clen, (uint64*)Cp, (uint64*)A.getDataPointer(), Astride,
                    (uint64*)B.getDataPointer(), Bstride);
            } break;
            default: {
                Error(_("Integers type not managed."));
            } break;
            }
            M = ArrayOf(classA, Cdim, Cp);
        } else {
            Error(_("Integers must be combined with integers of the same class."));
        }
    } else {
        if (A.isSparse() || B.isSparse()) {
            Error(_("Undefined function 'rem' for sparse input argument."));
        }
        VectorCheck(A, B, "rem");
        computeEntries(A, B, Astride, Bstride, Clen, Cdim);
        void* Cp;
        if (A.isSingleType() || B.isSingleType()) {
            Cp = new_with_exception<double>(Clen, false);
            A.promoteType(NLS_DOUBLE);
            B.promoteType(NLS_DOUBLE);
            remfunc<double>(Clen, (double*)Cp, (double*)A.getDataPointer(), Astride,
                (double*)B.getDataPointer(), Bstride);
            M = ArrayOf(NLS_DOUBLE, Cdim, Cp);
            M.promoteType(NLS_SINGLE);
        } else {
            Cp = new_with_exception<double>(Clen, false);
            remfunc<double>(Clen, (double*)Cp, (double*)A.getDataPointer(), Astride,
                (double*)B.getDataPointer(), Bstride);
            M = ArrayOf(NLS_DOUBLE, Cdim, Cp);
        }
    }
    return M;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
