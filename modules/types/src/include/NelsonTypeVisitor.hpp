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
#include <stdexcept>
#include <type_traits>
#include "NelsonTypeTraits.hpp"
#include "nlsTypes_exports.h"
#include "Error.hpp"
//=============================================================================
namespace Nelson::TypeTraits {
//=============================================================================
// Runtime-to-compile-time class dispatch. These helpers turn a NelsonType tag
// into a call to a generic functor instantiated for the matching C++ element
// type, so a hand-written switch(dataClass()) that only forwards to a
// template<T> kernel is replaced by a single visit() call.
//
// The functor is a generic lambda taking a value tag: the visited type is
// decltype(tag). It must compile for every element type in the selected set;
// use `if constexpr (isComplexElement<T>)` inside the body when the real and
// complex branches differ. Every return statement must yield the same type,
// which is also the type returned by the visit helper.
//
//   return TypeTraits::visitReal(value.dataClass(), [&](auto tag) {
//       using T = decltype(tag);
//       return reduceKernel<T>(value);
//   });
//
// The default branch raises an internal error: call sites must have validated
// that the class belongs to the selected set (usually after the overload
// dispatch has handled every non-numeric class).
//=============================================================================
/**
 * Compile-time test used inside a visitor body to branch on complex elements.
 */
template <class T>
inline constexpr bool isComplexElement
    = std::is_same_v<T, std::complex<double>> || std::is_same_v<T, std::complex<single>>;
//=============================================================================
// Defined out of line in NelsonTypeVisitor.cpp so the RaiseError dependency lives
// inside nlsTypes; consumers of the visitor only link nlsTypes, not error_manager.
[[noreturn]] NLSTYPES_IMPEXP void
raiseUnsupportedVisitedType();
//=============================================================================
/**
 * Dispatch over the real floating-point classes (single, double).
 */
template <class F>
decltype(auto)
visitFloat(NelsonType type, F&& f)
{
    switch (type) {
    case NLS_SINGLE:
        return f(single {});
    case NLS_DOUBLE:
        return f(double {});
    default:
        raiseUnsupportedVisitedType();
    }
}
//=============================================================================
/**
 * Dispatch over the integer classes only (int8..uint64). Used where a switch
 * forwards each integer class to a kernel that is not valid for floating-point
 * types.
 */
template <class F>
decltype(auto)
visitInteger(NelsonType type, F&& f)
{
    switch (type) {
    case NLS_INT8:
        return f(int8 {});
    case NLS_INT16:
        return f(int16 {});
    case NLS_INT32:
        return f(int32 {});
    case NLS_INT64:
        return f(int64 {});
    case NLS_UINT8:
        return f(uint8 {});
    case NLS_UINT16:
        return f(uint16 {});
    case NLS_UINT32:
        return f(uint32 {});
    case NLS_UINT64:
        return f(uint64 {});
    default:
        raiseUnsupportedVisitedType();
    }
}
//=============================================================================
/**
 * Dispatch over every real numeric class: the integer classes plus single and
 * double.
 */
template <class F>
decltype(auto)
visitReal(NelsonType type, F&& f)
{
    switch (type) {
    case NLS_INT8:
        return f(int8 {});
    case NLS_INT16:
        return f(int16 {});
    case NLS_INT32:
        return f(int32 {});
    case NLS_INT64:
        return f(int64 {});
    case NLS_UINT8:
        return f(uint8 {});
    case NLS_UINT16:
        return f(uint16 {});
    case NLS_UINT32:
        return f(uint32 {});
    case NLS_UINT64:
        return f(uint64 {});
    case NLS_SINGLE:
        return f(single {});
    case NLS_DOUBLE:
        return f(double {});
    default:
        raiseUnsupportedVisitedType();
    }
}
//=============================================================================
/**
 * Dispatch over every fixed-width storage class: the numeric classes plus
 * logical and char. Logical and char are passed as their underlying element
 * type (logical, charType). The functor body must handle the complex element
 * types (guard real-only code with isComplexElement<T>).
 */
template <class F>
decltype(auto)
visitStorage(NelsonType type, F&& f)
{
    switch (type) {
    case NLS_LOGICAL:
        return f(logical {});
    case NLS_INT8:
        return f(int8 {});
    case NLS_INT16:
        return f(int16 {});
    case NLS_INT32:
        return f(int32 {});
    case NLS_INT64:
        return f(int64 {});
    case NLS_UINT8:
        return f(uint8 {});
    case NLS_UINT16:
        return f(uint16 {});
    case NLS_UINT32:
        return f(uint32 {});
    case NLS_UINT64:
        return f(uint64 {});
    case NLS_SINGLE:
        return f(single {});
    case NLS_DOUBLE:
        return f(double {});
    case NLS_CHAR:
        return f(charType {});
    case NLS_SCOMPLEX:
        return f(std::complex<single> {});
    case NLS_DCOMPLEX:
        return f(std::complex<double> {});
    default:
        raiseUnsupportedVisitedType();
    }
}
//=============================================================================
/**
 * Dispatch over every numeric class: the real numeric classes plus the complex
 * floating-point classes. The functor body must handle the complex element
 * types (guard real-only code with isComplexElement<T>).
 */
template <class F>
decltype(auto)
visitNumeric(NelsonType type, F&& f)
{
    switch (type) {
    case NLS_INT8:
        return f(int8 {});
    case NLS_INT16:
        return f(int16 {});
    case NLS_INT32:
        return f(int32 {});
    case NLS_INT64:
        return f(int64 {});
    case NLS_UINT8:
        return f(uint8 {});
    case NLS_UINT16:
        return f(uint16 {});
    case NLS_UINT32:
        return f(uint32 {});
    case NLS_UINT64:
        return f(uint64 {});
    case NLS_SINGLE:
        return f(single {});
    case NLS_DOUBLE:
        return f(double {});
    case NLS_SCOMPLEX:
        return f(std::complex<single> {});
    case NLS_DCOMPLEX:
        return f(std::complex<double> {});
    default:
        raiseUnsupportedVisitedType();
    }
}
//=============================================================================
} // namespace Nelson::TypeTraits
//=============================================================================
