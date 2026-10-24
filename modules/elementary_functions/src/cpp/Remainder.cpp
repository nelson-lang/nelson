//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include <type_traits>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Remainder.hpp"
#include "ClassName.hpp"
#include "MatrixCheck.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
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
    if (y == 0) {
        return 0;
    }
    if (std::is_signed<T>::value && y == (T)-1) {
        // avoid integer overflow for intmin % -1; rem(a, -1) == 0 for any integer a
        return (T)0;
    }
    return (T)(x % y);
}
//=============================================================================
template <class T>
T
nelson_rem(T x, T y)
{
    T res = x;
    if (std::isinf(y)) {
        // rem(x, +/-Inf) == x for finite x, NaN otherwise
        return std::isfinite(x) ? x : static_cast<T>(std::nan(""));
    }
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
    if (res == 0) {
        // a zero remainder carries the sign of the dividend
        res = std::copysign((T)0, x);
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
template <class T>
void
remfunc_broadcast(T* C, const T* A, const T* B, const BinaryBroadcast& info)
{
    indexType N = info.dimensions.elementCount();
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        indexType ia = binaryOperatorBroadcastIndex(info, i, info.aStrides);
        indexType ib = binaryOperatorBroadcastIndex(info, i, info.bStrides);
        C[i] = nelson_rem(A[ia], B[ib]);
    }
}
//=============================================================================
template <class T>
void
remfunc_integer_broadcast(T* C, const T* A, const T* B, const BinaryBroadcast& info)
{
    indexType N = info.dimensions.elementCount();
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        indexType ia = binaryOperatorBroadcastIndex(info, i, info.aStrides);
        indexType ib = binaryOperatorBroadcastIndex(info, i, info.bStrides);
        C[i] = nelson_rem_integer(A[ia], B[ib]);
    }
}
//=============================================================================
template <class T>
static void
run_remfunc(indexType N, T* C, const T* A, int Astride, const T* B, int Bstride, bool broadcast,
    const BinaryBroadcast& info)
{
    if (broadcast) {
        remfunc_broadcast<T>(C, A, B, info);
    } else {
        remfunc<T>(N, C, A, Astride, B, Bstride);
    }
}
//=============================================================================
template <class T>
static void
run_remfunc_integer(indexType N, T* C, const T* A, int Astride, const T* B, int Bstride,
    bool broadcast, const BinaryBroadcast& info)
{
    if (broadcast) {
        remfunc_integer_broadcast<T>(C, A, B, info);
    } else {
        remfunc_integer<T>(N, C, A, Astride, B, Bstride);
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
        Cdim = B.dimensions();
    } else if (B.isScalar()) {
        Astride = 1;
        Bstride = 0;
        Cdim = A.dimensions();
    } else {
        Astride = 1;
        Bstride = 1;
        Cdim = A.dimensions();
    }
    Clen = Cdim.elementCount();
}
//=============================================================================
// Decide between the fast scalar/same-size path (Astride/Bstride) and the
// implicit-expansion (broadcasting) path used for compatible but unequal sizes.
static void
prepareEntries(const ArrayOf& A, const ArrayOf& B, int& Astride, int& Bstride, indexType& Clen,
    Dimensions& Cdim, bool& broadcast, BinaryBroadcast& info, const std::string& opname)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (A.isScalar() || B.isScalar() || SameSizeCheck(dimsA, dimsB)) {
        broadcast = false;
        computeEntries(A, B, Astride, Bstride, Clen, Cdim);
    } else {
        broadcast = true;
        info = binaryOperatorBroadcast(A, B, opname);
        Cdim = info.dimensions;
        Clen = Cdim.elementCount();
    }
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
    bool broadcast = false;
    BinaryBroadcast binfo;
    if (A.isLogical() || A.isCharacterArray()) {
        A.promoteType(NLS_DOUBLE);
    }
    if (B.isLogical() || B.isCharacterArray()) {
        B.promoteType(NLS_DOUBLE);
    }
    if (!A.isNumeric() || !B.isNumeric()) {
        Error(L"Nelson:validators:mustBeNumeric", L"%s must be numeric.", L"Input arguments");
    } else if (A.isComplex() || B.isComplex()) {
        Error("Nelson:elementary_functions:undefinedFunctionRemComplexInput",
            "Undefined function '%s' for complex input argument.", "rem");
    } else if (A.isIntegerType() || B.isIntegerType()) {
        if (A.isSparse() || B.isSparse()) {
            Error("Nelson:elementary_functions:undefinedFunctionRemSparseInput",
                "Undefined function '%s' for sparse input argument.", "rem");
        }
        CheckNumeric(A, B, "rem");
        prepareEntries(A, B, Astride, Bstride, Clen, Cdim, broadcast, binfo, "rem");
        NelsonType classA = A.dataClass();
        NelsonType classB = B.dataClass();
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
                run_remfunc_integer<int8>(Clen, (int8*)Cp, (int8*)A.dataPointer(), Astride,
                    (int8*)B.dataPointer(), Bstride, broadcast, binfo);
            } break;
            case NLS_UINT8: {
                Cp = new_with_exception<uint8>(Clen, false);
                run_remfunc_integer<uint8>(Clen, (uint8*)Cp, (uint8*)A.dataPointer(), Astride,
                    (uint8*)B.dataPointer(), Bstride, broadcast, binfo);
            } break;
            case NLS_INT16: {
                Cp = new_with_exception<int16>(Clen, false);
                run_remfunc_integer<int16>(Clen, (int16*)Cp, (int16*)A.dataPointer(), Astride,
                    (int16*)B.dataPointer(), Bstride, broadcast, binfo);
            } break;
            case NLS_UINT16: {
                Cp = new_with_exception<uint16>(Clen, false);
                run_remfunc_integer<uint16>(Clen, (uint16*)Cp, (uint16*)A.dataPointer(), Astride,
                    (uint16*)B.dataPointer(), Bstride, broadcast, binfo);
            } break;
            case NLS_INT32: {
                Cp = new_with_exception<int32>(Clen, false);
                run_remfunc_integer<int32>(Clen, (int32*)Cp, (int32*)A.dataPointer(), Astride,
                    (int32*)B.dataPointer(), Bstride, broadcast, binfo);
            } break;
            case NLS_UINT32: {
                Cp = new_with_exception<uint32>(Clen, false);
                run_remfunc_integer<uint32>(Clen, (uint32*)Cp, (uint32*)A.dataPointer(), Astride,
                    (uint32*)B.dataPointer(), Bstride, broadcast, binfo);
            } break;
            case NLS_INT64: {
                Cp = new_with_exception<int64>(Clen, false);
                run_remfunc_integer<int64>(Clen, (int64*)Cp, (int64*)A.dataPointer(), Astride,
                    (int64*)B.dataPointer(), Bstride, broadcast, binfo);
            } break;
            case NLS_UINT64: {
                Cp = new_with_exception<uint64>(Clen, false);
                run_remfunc_integer<uint64>(Clen, (uint64*)Cp, (uint64*)A.dataPointer(), Astride,
                    (uint64*)B.dataPointer(), Bstride, broadcast, binfo);
            } break;
            default: {
                Error("Nelson:elementary_functions:integersTypeManaged",
                    "Integers type not supported.");
            } break;
            }
            M = ArrayOf(classA, Cdim, Cp);
        } else {
            Error("Nelson:elementary_functions:integersCombinedIntegersSameClass",
                "Integers must be combined with integers of the same class.");
        }
    } else {
        if (A.isSparse() || B.isSparse()) {
            Error("Nelson:elementary_functions:undefinedFunctionRemSparseInput",
                "Undefined function '%s' for sparse input argument.", "rem");
        }
        CheckNumeric(A, B, "rem");
        prepareEntries(A, B, Astride, Bstride, Clen, Cdim, broadcast, binfo, "rem");
        void* Cp;
        if (A.isSingleType() || B.isSingleType()) {
            Cp = new_with_exception<double>(Clen, false);
            A.promoteType(NLS_DOUBLE);
            B.promoteType(NLS_DOUBLE);
            run_remfunc<double>(Clen, (double*)Cp, (double*)A.dataPointer(), Astride,
                (double*)B.dataPointer(), Bstride, broadcast, binfo);
            M = ArrayOf(NLS_DOUBLE, Cdim, Cp);
            M.promoteType(NLS_SINGLE);
        } else {
            Cp = new_with_exception<double>(Clen, false);
            run_remfunc<double>(Clen, (double*)Cp, (double*)A.dataPointer(), Astride,
                (double*)B.dataPointer(), Bstride, broadcast, binfo);
            M = ArrayOf(NLS_DOUBLE, Cdim, Cp);
        }
    }
    return M;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
