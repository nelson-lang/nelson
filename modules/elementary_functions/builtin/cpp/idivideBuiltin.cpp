//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <type_traits>
#include <limits>
#include "idivideBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
static inline T
intDivideMode(T a, T b, int mode)
{
    T q = (T)(a / b);
    T r = (T)(a % b);
    if (r == 0) {
        return q;
    }
    if (mode == 0) {
        return q; // fix (truncate toward zero)
    }
    if constexpr (std::is_signed_v<T>) {
        bool opposite = ((a < 0) != (b < 0));
        if (mode == 3) {
            return opposite ? (T)(q - 1) : q; // floor
        }
        if (mode == 2) {
            return opposite ? q : (T)(q + 1); // ceil
        }
        T ar = (r < 0) ? (T)(-r) : r;
        T ab = (b < 0) ? (T)(-b) : b;
        if (ar >= (T)(ab - ar)) {
            return opposite ? (T)(q - 1) : (T)(q + 1); // round half away from zero
        }
        return q;
    } else {
        if (mode == 3) {
            return q; // floor (unsigned)
        }
        if (mode == 2) {
            return (T)(q + 1); // ceil (unsigned, r != 0)
        }
        if (r >= (T)(b - r)) {
            return (T)(q + 1); // round half away from zero
        }
        return q;
    }
}
//=============================================================================
// division by zero saturates like the floating-point quotient cast to the class:
// +Inf -> intmax, -Inf -> intmin, 0/0 (NaN) -> 0
template <class T>
static inline T
intDivideByZero(T a)
{
    if (a > 0) {
        return std::numeric_limits<T>::max();
    }
    if constexpr (std::is_signed_v<T>) {
        if (a < 0) {
            return std::numeric_limits<T>::min();
        }
    }
    return (T)0;
}
//=============================================================================
template <class T>
static inline T
intDivideSafe(T a, T b, int mode)
{
    if constexpr (std::is_signed_v<T>) {
        // a single test for the two special divisors, 0 and -1
        using U = std::make_unsigned_t<T>;
        if (static_cast<U>(static_cast<U>(b) + static_cast<U>(1)) <= static_cast<U>(1)) {
            if (b == 0) {
                return intDivideByZero<T>(a);
            }
            // intmin / -1 overflows (hardware trap): the exact quotient saturates to intmax
            return (a == std::numeric_limits<T>::min()) ? std::numeric_limits<T>::max() : (T)(-a);
        }
    } else {
        if (b == 0) {
            return intDivideByZero<T>(a);
        }
    }
    return intDivideMode<T>(a, b, mode);
}
//=============================================================================
template <class T>
static ArrayOf
idivideTyped(const ArrayOf& a, const ArrayOf& b, NelsonType cls, int mode)
{
    indexType na = a.elementCount();
    indexType nb = b.elementCount();
    indexType n = (na >= nb) ? na : nb;
    Dimensions outDims = (na >= nb) ? a.dimensions() : b.dimensions();
    const T* pa = (const T*)a.dataPointer();
    const T* pb = (const T*)b.dataPointer();
    T* out = (T*)ArrayOf::allocateArrayOf(cls, n);
    for (indexType i = 0; i < n; ++i) {
        T av = pa[(na == 1) ? 0 : i];
        T bv = pb[(nb == 1) ? 0 : i];
        out[i] = intDivideSafe<T>(av, bv, mode);
    }
    return ArrayOf(cls, outDims, out);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::idivideBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf a = argIn[0];
    ArrayOf b = argIn[1];
    bool aInt = a.isIntegerType();
    bool bInt = b.isIntegerType();
    if (!aInt && !bInt) {
        Error(L"Nelson:elementary_functions:leastOneInputBelongInteger",
            L"At least one input must belong to an integer class.");
    }
    if (aInt && bInt && a.dataClass() != b.dataClass()) {
        Error(L"Nelson:elementary_functions:integerInputsSameClass",
            L"Integer inputs must be of the same class.");
    }
    NelsonType cls = aInt ? a.dataClass() : b.dataClass();
    int mode = 0; // fix
    if (argIn.size() == 3) {
        if (!argIn[2].isRowVectorCharacterArray() && !argIn[2].isScalarStringArray()) {
            Error(L"Nelson:elementary_functions:thirdArgumentFixRoundFloor",
                L"Third argument must be 'fix', 'round', 'floor', or 'ceil'.");
        }
        std::wstring opt = argIn[2].asWideString();
        if (opt == L"fix") {
            mode = 0;
        } else if (opt == L"round") {
            mode = 1;
        } else if (opt == L"ceil") {
            mode = 2;
        } else if (opt == L"floor") {
            mode = 3;
        } else {
            Error(L"Nelson:elementary_functions:thirdArgumentFixRoundFloor",
                L"Third argument must be 'fix', 'round', 'floor', or 'ceil'.");
        }
    }
    ArrayOf aa(a);
    if (!aInt) {
        aa.promoteType(cls);
    }
    ArrayOf bb(b);
    if (!bInt) {
        bb.promoteType(cls);
    }
    indexType na = aa.elementCount();
    indexType nb = bb.elementCount();
    if (na != nb && na != 1 && nb != 1) {
        Error(L"Nelson:elementary_functions:arraysIncompatibleSizes",
            L"Arrays have incompatible sizes for this operation.");
    }
    switch (cls) {
    case NLS_INT8:
        retval << idivideTyped<int8>(aa, bb, cls, mode);
        break;
    case NLS_UINT8:
        retval << idivideTyped<uint8>(aa, bb, cls, mode);
        break;
    case NLS_INT16:
        retval << idivideTyped<int16>(aa, bb, cls, mode);
        break;
    case NLS_UINT16:
        retval << idivideTyped<uint16>(aa, bb, cls, mode);
        break;
    case NLS_INT32:
        retval << idivideTyped<int32>(aa, bb, cls, mode);
        break;
    case NLS_UINT32:
        retval << idivideTyped<uint32>(aa, bb, cls, mode);
        break;
    case NLS_INT64:
        retval << idivideTyped<int64>(aa, bb, cls, mode);
        break;
    case NLS_UINT64:
        retval << idivideTyped<uint64>(aa, bb, cls, mode);
        break;
    default:
        Error(L"Nelson:elementary_functions:leastOneInputBelongInteger",
            L"At least one input must belong to an integer class.");
        break;
    }
    return retval;
}
//=============================================================================
