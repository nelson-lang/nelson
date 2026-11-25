//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <complex>
#include "Types.hpp"
//=============================================================================
namespace Nelson::TypeTraits {
//=============================================================================
// Compile-time description of the NelsonType enum. These helpers replace the
// hand-written switch(dataClass()) ladders that are duplicated across the
// codebase (element byte sizes, real/complex mapping, integer/float tests).
//
// Every function is constexpr and inlinable, so a call compiles to the same
// code as the inline switch it replaces: there is no runtime cost over the
// hand-rolled version.
//=============================================================================
/**
 * Storage size, in bytes, of one element of a fixed-width class.
 *
 * Returns 0 for reference or variable-size classes (struct, cell, string,
 * handle, function handle, ...), whose element footprint is not fixed. The
 * numeric, logical, char and complex sizes match ArrayOf::elementSize().
 */
constexpr indexType
byteSize(NelsonType type) noexcept
{
    switch (type) {
    case NLS_LOGICAL:
        return sizeof(logical);
    case NLS_UINT8:
        return sizeof(uint8);
    case NLS_INT8:
        return sizeof(int8);
    case NLS_UINT16:
        return sizeof(uint16);
    case NLS_INT16:
        return sizeof(int16);
    case NLS_UINT32:
        return sizeof(uint32);
    case NLS_INT32:
        return sizeof(int32);
    case NLS_UINT64:
        return sizeof(uint64);
    case NLS_INT64:
        return sizeof(int64);
    case NLS_SINGLE:
        return sizeof(single);
    case NLS_DOUBLE:
        return sizeof(double);
    case NLS_SCOMPLEX:
        return sizeof(single) * 2;
    case NLS_DCOMPLEX:
        return sizeof(double) * 2;
    case NLS_CHAR:
        return sizeof(charType);
    default:
        return 0;
    }
}
//=============================================================================
/**
 * Return whether the class is a complex floating-point class.
 */
constexpr bool
isComplex(NelsonType type) noexcept
{
    return type == NLS_DCOMPLEX || type == NLS_SCOMPLEX;
}
//=============================================================================
/**
 * Return whether the class is one of the signed or unsigned integer classes.
 * Equivalent to the IS_INTEGER_TYPE macro.
 */
constexpr bool
isInteger(NelsonType type) noexcept
{
    return type >= NLS_INT8 && type <= NLS_UINT64;
}
//=============================================================================
/**
 * Return whether the class is one of the signed integer classes.
 * Equivalent to the IS_SIGNED_INTEGER_TYPE macro.
 */
constexpr bool
isSignedInteger(NelsonType type) noexcept
{
    return type >= NLS_INT8 && type <= NLS_INT64;
}
//=============================================================================
/**
 * Return whether the class is one of the unsigned integer classes.
 * Equivalent to the IS_UNSIGNED_INTEGER_TYPE macro.
 */
constexpr bool
isUnsignedInteger(NelsonType type) noexcept
{
    return type >= NLS_UINT8 && type <= NLS_UINT64;
}
//=============================================================================
/**
 * Return whether the class is a real floating-point class (double or single).
 */
constexpr bool
isFloat(NelsonType type) noexcept
{
    return type == NLS_DOUBLE || type == NLS_SINGLE;
}
//=============================================================================
/**
 * Return whether the class is numeric: a real or complex floating-point class
 * or an integer class. Logical and char are not numeric here.
 */
constexpr bool
isNumeric(NelsonType type) noexcept
{
    return isFloat(type) || isComplex(type) || isInteger(type);
}
//=============================================================================
/**
 * Map a class to its real counterpart: complex classes lose the imaginary
 * part (DCOMPLEX -> DOUBLE, SCOMPLEX -> SINGLE); every other class is returned
 * unchanged.
 */
constexpr NelsonType
realClass(NelsonType type) noexcept
{
    switch (type) {
    case NLS_DCOMPLEX:
        return NLS_DOUBLE;
    case NLS_SCOMPLEX:
        return NLS_SINGLE;
    default:
        return type;
    }
}
//=============================================================================
/**
 * Map a class to its complex counterpart (DOUBLE -> DCOMPLEX,
 * SINGLE -> SCOMPLEX); every other class is returned unchanged.
 */
constexpr NelsonType
complexClass(NelsonType type) noexcept
{
    switch (type) {
    case NLS_DOUBLE:
        return NLS_DCOMPLEX;
    case NLS_SINGLE:
        return NLS_SCOMPLEX;
    default:
        return type;
    }
}
//=============================================================================
// Compile-time mapping from a C++ element type to its NelsonType tag. Only the
// unambiguous fixed-width element types are provided. Note that `logical` and
// `uint8` are the same underlying type (uint8_t), so uint8_t maps to NLS_UINT8;
// code that needs the logical class refers to NLS_LOGICAL explicitly.
//=============================================================================
template <class T> struct nelsonTypeTag;
template <> struct nelsonTypeTag<double>
{
    static constexpr NelsonType value = NLS_DOUBLE;
};
template <> struct nelsonTypeTag<single>
{
    static constexpr NelsonType value = NLS_SINGLE;
};
template <> struct nelsonTypeTag<int8>
{
    static constexpr NelsonType value = NLS_INT8;
};
template <> struct nelsonTypeTag<int16>
{
    static constexpr NelsonType value = NLS_INT16;
};
template <> struct nelsonTypeTag<int32>
{
    static constexpr NelsonType value = NLS_INT32;
};
template <> struct nelsonTypeTag<int64>
{
    static constexpr NelsonType value = NLS_INT64;
};
template <> struct nelsonTypeTag<uint8>
{
    static constexpr NelsonType value = NLS_UINT8;
};
template <> struct nelsonTypeTag<uint16>
{
    static constexpr NelsonType value = NLS_UINT16;
};
template <> struct nelsonTypeTag<uint32>
{
    static constexpr NelsonType value = NLS_UINT32;
};
template <> struct nelsonTypeTag<uint64>
{
    static constexpr NelsonType value = NLS_UINT64;
};
template <> struct nelsonTypeTag<std::complex<double>>
{
    static constexpr NelsonType value = NLS_DCOMPLEX;
};
template <> struct nelsonTypeTag<std::complex<single>>
{
    static constexpr NelsonType value = NLS_SCOMPLEX;
};
//=============================================================================
/**
 * Convenience accessor for nelsonTypeTag<T>::value.
 */
template <class T> inline constexpr NelsonType nelsonTypeOf = nelsonTypeTag<T>::value;
//=============================================================================
} // namespace Nelson::TypeTraits
//=============================================================================
