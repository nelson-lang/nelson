//=============================================================================
// Copyright (c) 2019-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4018)
#endif
//=============================================================================
#include <algorithm>
#include <cmath>
#include <limits>
#include <type_traits>
#include <typeinfo>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Dimensions.hpp"
#include "Types.hpp"
#include "SparseType.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename TIN, typename TOUT>
inline TOUT
numeric_cast(TIN value)
{
    if constexpr (std::is_floating_point<TIN>::value && std::is_integral<TOUT>::value) {
        // Saturate with exact power-of-two bounds: TOUT max + 1 (== 2^digits) and
        // -(2^digits) are exactly representable as floating point, while TOUT max
        // itself may not be (e.g. int64 max rounds up to 2^63 as a double, which
        // made "value > max" miss the overflow and wrap around).
        const long double rounded = std::round(static_cast<long double>(value));
        if (std::isnan(rounded)) {
            return static_cast<TOUT>(0);
        }
        const long double upperBound = std::ldexp(1.0L, std::numeric_limits<TOUT>::digits);
        if (rounded >= upperBound) {
            return std::numeric_limits<TOUT>::max();
        }
        if constexpr (std::numeric_limits<TOUT>::is_signed) {
            if (rounded < -upperBound) {
                return std::numeric_limits<TOUT>::min();
            }
        } else {
            if (rounded <= 0.0L) {
                return static_cast<TOUT>(0);
            }
        }
        return static_cast<TOUT>(rounded);
    } else {
        const bool positive_overflow_possible
            = std::numeric_limits<TOUT>::max() < std::numeric_limits<TIN>::max();
        const bool negative_overflow_possible = std::numeric_limits<TIN>::is_signed
            || (std::numeric_limits<TOUT>::lowest() > std::numeric_limits<TIN>::lowest());

        // unsigned <-- unsigned
        if ((!std::numeric_limits<TOUT>::is_signed) && (!std::numeric_limits<TIN>::is_signed)) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
        }
        // unsigned <-- signed
        else if ((!std::numeric_limits<TOUT>::is_signed) && std::numeric_limits<TIN>::is_signed) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
            if (negative_overflow_possible && (value < 0)) {
                return std::numeric_limits<TOUT>::min();
            }
        }
        // signed <-- unsigned
        else if (std::numeric_limits<TOUT>::is_signed && (!std::numeric_limits<TIN>::is_signed)) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
        } else if (std::numeric_limits<TOUT>::is_signed && std::numeric_limits<TIN>::is_signed) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
            if (negative_overflow_possible && (value < std::numeric_limits<TOUT>::lowest())) {
                return std::numeric_limits<TOUT>::min();
            }
        }
        return static_cast<TOUT>(value);
    }
}
//=============================================================================
template <class TIN, class TOUT>
void
saturate(NelsonType classIn, NelsonType classOut, const void* pIn, void* pOut, indexType count)
{
    const TIN* sp = static_cast<const TIN*>(pIn);
    TOUT* qp = static_cast<TOUT*>(pOut);
    if (classIn != classOut) {
        bool checkNaN = false;
        if (typeid(TOUT) != typeid(single) || typeid(TOUT) != typeid(double)) {
            if (typeid(TIN) == typeid(single) || typeid(TIN) == typeid(double)) {
                checkNaN = true;
            }
        }
        if (checkNaN) {
            OMP_PARALLEL_FOR_LOOP(count)
            for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
                if (std::isnan((double)sp[i])) {
                    qp[i] = (TOUT)0;
                } else {
                    qp[i] = (TOUT)numeric_cast<TIN, TOUT>(sp[i]);
                }
            }
        } else {
            OMP_PARALLEL_FOR_LOOP(count)
            for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
                qp[i] = numeric_cast<TIN, TOUT>(sp[i]);
            }
        }
    }
}
//=============================================================================
/**
 * Promote our data to a new type, converting the data as we go.
 * Reference source or destination types cannot be converted. Rules for the other types:
 *
 * Source type
 *  - string
 *    - logical dest = (source == 0) ? 0 : 1
 *    - real dest = (double) source
 *    - complex dest = (double) source
 *  - logical
 *    - string dest = (char) source
 *    - real   dest = (double) source
 *    - complex dest = (double) source
 *  - real
 *    - string dest = (char) source
 *    - logical dest = (source == 0) ? 0 : 1
 *    - complex dest = (double) source
 *  - complex
 *    - string dest = (char) real(source)
 *    - logical dest = (real(source) == 0 && imag(source) == 0) ? 0:1
 *    - real dest = real(source)
 */
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteAsReal(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        dstPtr[i] = (TOUT)ptr[i];
    }
    return dstPtr;
}
//=============================================================================
template <class TIN>
charType*
promoteAsChar(NelsonType dstClass, const TIN* ptr, indexType count)
{
    charType* dstPtr = (charType*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        long double value = static_cast<long double>(ptr[i]);
        if (!std::isfinite(value)) {
            dstPtr[i] = (value > 0) ? static_cast<charType>(65535) : static_cast<charType>(0);
        } else if (value < 0) {
            dstPtr[i] = static_cast<charType>(0);
        } else if (value > 65535) {
            dstPtr[i] = static_cast<charType>(65535);
        } else {
            dstPtr[i] = static_cast<charType>(value);
        }
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteAsLogical(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        dstPtr[i] = (ptr[i] == 0) ? 0 : 1;
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteComplexAsLogical(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < count; ++i) {
        dstPtr[i] = (ptr[(i * 2)] || ptr[(i * 2) + 1]) ? 1 : 0;
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteComplexAsReal(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i = i + 1) {
        dstPtr[i] = (TOUT)ptr[i * 2];
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteComplexAsComplex(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count * 2; i = i + 2) {
        dstPtr[i] = (TOUT)ptr[i];
        dstPtr[i + 1] = (TOUT)ptr[i + 1];
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteAsComplex(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count, stringVector(), true);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i = i + 1) {
        dstPtr[i << 1] = (TOUT)ptr[i];
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteComplexAsInteger(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    bool checkNaN = false;
    if (typeid(TOUT) != typeid(single) || typeid(TOUT) != typeid(double)) {
        if (typeid(TIN) == typeid(single) || typeid(TIN) == typeid(double)) {
            checkNaN = true;
        }
    }
    if (checkNaN) {
        OMP_PARALLEL_FOR_LOOP(count)
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            if (std::isnan((double)ptr[i * 2])) {
                dstPtr[i] = (TOUT)0;
            } else {
                dstPtr[i] = (TOUT)numeric_cast<TIN, TOUT>(ptr[i * 2]);
            }
        }
    } else {
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            dstPtr[i] = numeric_cast<TIN, TOUT>(ptr[i * 2]);
        }
    }
    return dstPtr;
}
//=============================================================================
void
ArrayOf::promoteType(NelsonType dstClass, const stringVector& fNames)
{
    if (hasScalarStorage()) {
        if (dstClass == scalarType_) {
            return;
        }
        materializeScalar();
    }
    if (dp->dataClass != dstClass && !isEmpty()) {
        bool allowedCases = ((dstClass == NLS_CLASS_ARRAY) && (dp->dataClass == NLS_STRUCT_ARRAY))
            || ((dstClass == NLS_CLASSDEF_ARRAY) && (dp->dataClass == NLS_STRUCT_ARRAY))
            || ((dstClass == NLS_STRUCT_ARRAY) && (dp->dataClass == NLS_CLASS_ARRAY))
            || ((dstClass == NLS_STRUCT_ARRAY) && (dp->dataClass == NLS_CLASSDEF_ARRAY))
            || ((dstClass == NLS_FUNCTION_HANDLE) && (dp->dataClass == NLS_STRUCT_ARRAY))
            || ((dstClass == NLS_STRUCT_ARRAY) && (dp->dataClass == NLS_FUNCTION_HANDLE));
        if ((dstClass == NLS_STRING_ARRAY) || (dstClass == NLS_CELL_ARRAY)
            || (dp->dataClass <= NLS_CHAR && dstClass > NLS_CHAR) && !allowedCases) {
            Error(L"Nelson:types:conversionNotSupported:cannotConvertBaseTypesReferenceTypes",
                L"Cannot convert base types to reference types.");
        }
    }

    if (isEmpty() && (!isSparse() || dp->data() == nullptr)) {
        dp = dp->putData(dstClass, dp->dimensions_, nullptr, isSparse(), fNames);
        return;
    }
    if (isSparse()) {
        dp = dp->putData(dstClass, dp->dimensions_,
            Eigen_TypeConvertSparse(
                dstClass, dp->dimensions_[0], dp->dimensions_[1], dp->data(), dp->dataClass),
            true);
        return;
    }
    indexType count = elementCount();
    void* dstPtr = nullptr;
    switch (dp->dataClass) {
    case NLS_HANDLE: {
        if (dstClass == NLS_HANDLE) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"handle-arrays");
    } break;
    case NLS_GO_HANDLE: {
        if (dstClass == NLS_GO_HANDLE) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"graphic handle-arrays");
    } break;
    case NLS_CELL_ARRAY: {
        if (dstClass == NLS_CELL_ARRAY) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"cell-arrays");

    } break;
    case NLS_FUNCTION_HANDLE: {
        if (dstClass == NLS_STRUCT_ARRAY) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"function_handle");

    } break;
    case NLS_CLASS_ARRAY: {
        if (dstClass == NLS_CLASS_ARRAY) {
            if (dp->classTypeName() != classType()) {
                Error(L"Nelson:types:conversionNotSupported:cannotCombineClassesDifferentTypes",
                    L"Cannot combine classes with different types.");
            }
            return;
        }
        if (dstClass == NLS_STRUCT_ARRAY) {
            // The class flip mutates Data in place: detach shared storage first.
            ensureSingleOwner();
            dp->promoteClassToStruct();
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"class");
    } break;
    case NLS_CLASSDEF_ARRAY: {
        if (dstClass == NLS_CLASSDEF_ARRAY) {
            if (dp->classTypeName() != classType()) {
                Error(L"Nelson:types:conversionNotSupported:cannotCombineClassesDifferentTypes",
                    L"Cannot combine classes with different types.");
            }
            return;
        }
        if (dstClass == NLS_STRUCT_ARRAY) {
            ensureSingleOwner();
            dp->promoteClassToStruct();
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"class");
    } break;
    case NLS_STRUCT_ARRAY: {
        if (dstClass == NLS_STRUCT_ARRAY) {
            // TODO: Generalize this code to allow for one more field in destination
            // than in source...
            const stringVector& fieldNames = dp->fieldNames();
            if (fieldNames.size() > fNames.size()) {
                Error(L"Nelson:types:structFieldsMismatch",
                    L"Cannot combine structures with different fields if the combination requires "
                    L"fields to be deleted from one of the structures.");
            }
            // Every field name in fieldnames must be present in fnames.
            int extraCount = 0;
            int matchCount = 0;
            indexType i;
            for (i = 0; i < (int)fNames.size(); i++) {
                int64 ndx = fieldIndex(fNames[i]);
                if (ndx == -1) {
                    extraCount++;
                } else {
                    matchCount++;
                }
            }
            if (matchCount != fieldNames.size()) {
                Error(L"Nelson:types:structFieldsMismatch",
                    L"Cannot combine structures with different fields if the combination requires "
                    L"fields to be deleted from one of the structures.");
            }
            void* dstPtr = allocateArrayOf(dp->dataClass, elementCount(), fNames, false);
            const ArrayOf* src_rp = (const ArrayOf*)dp->data();
            ArrayOf* dst_rp = (ArrayOf*)dstPtr;
            indexType elCount(elementCount());
            indexType fieldCount(fieldNames.size());
            indexType newFieldCount(fNames.size());
            ;
            for (i = 0; i < fieldCount; i++) {
                int64 newNdx = fieldIndexFromList(fieldNames[i], fNames);
                for (indexType j = 0; j < elCount; j++) {
                    dst_rp[j * newFieldCount + newNdx] = src_rp[j * fieldCount + i];
                }
            }
            dp = dp->putData(dp->dataClass, dp->dimensions_, dstPtr, false, fNames);
            return;
        }
        if (dstClass == NLS_CLASS_ARRAY) {
            // The class flip mutates Data in place: detach shared storage first.
            ensureSingleOwner();
            dp->promoteStructToClass();
            return;
        }
        if (dstClass == NLS_CLASSDEF_ARRAY) {
            ensureSingleOwner();
            dp->promoteStructToClassdef();
            return;
        }
        if (dstClass == NLS_FUNCTION_HANDLE) {
            ensureSingleOwner();
            dp->promoteStructToFunctionHandle();
            return;
        }

        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"struct-arrays");

    } break;
    case NLS_STRING_ARRAY: {
        if (dstClass == NLS_STRING_ARRAY) {
            return;
        }
        Error(L"Nelson:types:conversionNotSupported:cannotConvertValueAnyOtherType",
            L"Cannot convert %s to any other type.", L"string-arrays");

    } break;
    case NLS_LOGICAL: {
        const logical* sp = (const logical*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            return;
        } break;
        case NLS_UINT8: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, uint8>(dstClass, sp, count));
        } break;
        case NLS_INT8: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, int8>(dstClass, sp, count));
        } break;
        case NLS_UINT16: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, uint16>(dstClass, sp, count));
        } break;
        case NLS_INT16: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, int16>(dstClass, sp, count));
        } break;
        case NLS_UINT32: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, uint32>(dstClass, sp, count));
        } break;
        case NLS_INT32: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, int32>(dstClass, sp, count));
        } break;
        case NLS_UINT64: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, uint64>(dstClass, sp, count));
        } break;
        case NLS_INT64: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, int64>(dstClass, sp, count));
        } break;
        case NLS_SINGLE: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, single>(dstClass, sp, count));
        } break;
        case NLS_DOUBLE: {
            dstPtr = static_cast<void*>(promoteAsReal<logical, double>(dstClass, sp, count));
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = static_cast<void*>(promoteAsComplex<logical, single>(dstClass, sp, count));
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = static_cast<void*>(promoteAsComplex<logical, double>(dstClass, sp, count));
        } break;
        case NLS_CHAR: {
            Error(L"Nelson:types:conversionNotSupported:conversionCharFromLogicalNotPossible",
                L"Conversion to char from logical is not possible.");
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_UINT8: {
        const uint8* sp = (const uint8*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<uint8, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            return;
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<uint8, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteAsReal<uint8, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<uint8, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<uint8, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<uint8>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_INT8: {
        const int8* sp = (const int8*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<int8, logical>(dstClass, sp, count);
        } break;
        case NLS_INT8: {
            return;
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<int8, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteAsReal<int8, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<int8, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<int8, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<int8>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_UINT16: {
        const uint16* sp = static_cast<const uint16*>(dp->data());
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<uint16, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            return;
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<uint16, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteAsReal<uint16, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<uint16, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<uint16, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<uint16>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_INT16: {
        const int16* sp = (const int16*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<int16, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            return;
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<int16, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteAsReal<int16, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<int16, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<int16, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<int16>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_UINT32: {
        const uint32* sp = (const uint32*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<uint32, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            return;
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<uint32, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteAsReal<uint32, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<uint32, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<uint32, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<uint32>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_INT32: {
        const int32* sp = (const int32*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<int32, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            return;
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<int32, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteAsReal<int32, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<int32, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<int32, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<int32>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_UINT64: {
        const uint64* sp = (const uint64*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<uint64, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            return;
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<uint64, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteAsReal<uint64, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<uint64, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<uint64, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<uint64>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_INT64: {
        const int64* sp = (const int64*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<int64, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            return;
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<int64, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteAsReal<int64, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<int64, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<int64, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<int64>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_SINGLE: {
        const single* sp = (const single*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<single, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            return;
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteAsReal<single, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<single, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<single, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<single>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_DOUBLE: {
        const double* sp = (const double*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<double, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<double, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            return;
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteAsComplex<double, single>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteAsComplex<double, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteAsChar<double>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_SCOMPLEX: {
        const single* sp = (const single*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteComplexAsLogical<single, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = promoteComplexAsInteger<single, uint8>(dstClass, sp, count);
        } break;
        case NLS_INT8: {
            dstPtr = promoteComplexAsInteger<single, int8>(dstClass, sp, count);
        } break;
        case NLS_UINT16: {
            dstPtr = promoteComplexAsInteger<single, uint16>(dstClass, sp, count);
        } break;
        case NLS_INT16: {
            dstPtr = promoteComplexAsInteger<single, int16>(dstClass, sp, count);
        } break;
        case NLS_UINT32: {
            dstPtr = promoteComplexAsInteger<single, uint32>(dstClass, sp, count);
        } break;
        case NLS_INT32: {
            dstPtr = promoteComplexAsInteger<single, int32>(dstClass, sp, count);
        } break;
        case NLS_UINT64: {
            dstPtr = promoteComplexAsInteger<single, uint64>(dstClass, sp, count);
        } break;
        case NLS_INT64: {
            dstPtr = promoteComplexAsInteger<single, int64>(dstClass, sp, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteComplexAsReal<single, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteComplexAsReal<single, double>(dstClass, sp, count);
        } break;
        case NLS_SCOMPLEX: {
            return;
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = promoteComplexAsComplex<single, double>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteComplexAsReal<single, charType>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_DCOMPLEX: {
        const double* sp = (const double*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteComplexAsLogical<double, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = promoteComplexAsInteger<double, uint8>(dstClass, sp, count);
        } break;
        case NLS_INT8: {
            dstPtr = promoteComplexAsInteger<double, int8>(dstClass, sp, count);
        } break;
        case NLS_UINT16: {
            dstPtr = promoteComplexAsInteger<double, uint16>(dstClass, sp, count);
        } break;
        case NLS_INT16: {
            dstPtr = promoteComplexAsInteger<double, int16>(dstClass, sp, count);
        } break;
        case NLS_UINT32: {
            dstPtr = promoteComplexAsInteger<double, uint32>(dstClass, sp, count);
        } break;
        case NLS_INT32: {
            dstPtr = promoteComplexAsInteger<double, int32>(dstClass, sp, count);
        } break;
        case NLS_UINT64: {
            dstPtr = promoteComplexAsInteger<double, uint64>(dstClass, sp, count);
        } break;
        case NLS_INT64: {
            dstPtr = promoteComplexAsInteger<double, int64>(dstClass, sp, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteComplexAsReal<double, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            dstPtr = promoteComplexAsReal<double, double>(dstClass, sp, count);
        } break;
        case NLS_DCOMPLEX: {
            return;
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = promoteComplexAsComplex<double, single>(dstClass, sp, count);
        } break;
        case NLS_CHAR: {
            dstPtr = promoteComplexAsReal<double, charType>(dstClass, sp, count);
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_CHAR: {
        charType* sp = (charType*)dp->data();
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = static_cast<void*>(promoteAsLogical<charType, logical>(dstClass, sp, count));
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, uint8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, int8>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, uint16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, int16>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, uint32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, int32>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, uint64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, int64>(dp->dataClass, dstClass, dp->data(), dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = static_cast<void*>(promoteAsReal<charType, single>(dstClass, sp, count));
        } break;
        case NLS_DOUBLE: {
            dstPtr = static_cast<void*>(promoteAsReal<charType, double>(dstClass, sp, count));
        } break;
        case NLS_SCOMPLEX: {
            dstPtr = static_cast<void*>(promoteAsComplex<charType, single>(dstClass, sp, count));
        } break;
        case NLS_DCOMPLEX: {
            dstPtr = static_cast<void*>(promoteAsComplex<charType, double>(dstClass, sp, count));
        } break;
        case NLS_CHAR: {
            return;
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    case NLS_MISSING_ARRAY: {
        switch (dstClass) {
        case NLS_SINGLE: {
            single* sptr = (single*)ArrayOf::allocateArrayOf(dstClass, count);
            std::fill_n(sptr, count, std::nanf(""));
            dstPtr = sptr;
        } break;
        case NLS_DOUBLE: {
            double* sptr = (double*)ArrayOf::allocateArrayOf(dstClass, count);
            std::fill_n(sptr, count, std::nan(""));
            dstPtr = sptr;
        } break;
        case NLS_SCOMPLEX: {
            single* sptr = (single*)ArrayOf::allocateArrayOf(dstClass, count);
            for (indexType k = 0; k < count; k++) {
                sptr[2 * k] = std::nanf("");
                sptr[2 * k + 1] = 0;
            }
            dstPtr = sptr;
        } break;
        case NLS_DCOMPLEX: {
            double* sptr = (double*)ArrayOf::allocateArrayOf(dstClass, count);
            for (indexType k = 0; k < count; k++) {
                sptr[2 * k] = std::nan("");
                sptr[2 * k + 1] = 0;
            }
            dstPtr = sptr;
        } break;
        case NLS_MISSING_ARRAY: {
            return;
        } break;
        default: {
            Error("Nelson:types:typeNotManaged", "Type not supported.");
        } break;
        }
    } break;
    default: {
        Error(L"Nelson:types:typeNotManaged", L"Type not supported.");
    } break;
    }
    replaceData(dstClass, dp->dimensions_, dstPtr);
}
//=============================================================================
void
ArrayOf::promoteType(NelsonType dstClass)
{
    stringVector dummy;
    promoteType(dstClass, dummy);
}
//=============================================================================
bool
ArrayOf::canBePromotedTo(NelsonType dstClass)
{
    if (isEmpty()) {
        return true;
    }
    const NelsonType sourceClass = dataClass();
    if (sourceClass != dstClass) {
        if ((dstClass == NLS_STRING_ARRAY) || (dstClass == NLS_CELL_ARRAY)
            || (dstClass == NLS_STRUCT_ARRAY) || (dstClass == NLS_CLASS_ARRAY)
            || (dstClass == NLS_CLASSDEF_ARRAY) || (dstClass == NLS_FUNCTION_HANDLE)) {
            return false;
        }
    }
    if (sourceClass == dstClass) {
        return true;
    }
    if (isSparse()) {
        if ((sourceClass == NLS_SINGLE) || (sourceClass == NLS_SCOMPLEX)
            || (sourceClass == NLS_DOUBLE) || (sourceClass == NLS_DCOMPLEX)
            || (sourceClass == NLS_LOGICAL)) {
            return true;
        }
    }
    switch (sourceClass) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY: {
        return false;
    } break;
    case NLS_LOGICAL: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_UINT8: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_INT8: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_INT8:
        case NLS_UINT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_UINT16: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return true;
        } break;
        }
    } break;
    case NLS_INT16: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_INT16:
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_UINT32: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_INT32: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_UINT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_UINT64: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_INT64: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_INT64:
        case NLS_UINT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_SINGLE: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_DOUBLE: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_SCOMPLEX: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_DCOMPLEX: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_DCOMPLEX:
        case NLS_SCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    case NLS_CHAR: {
        switch (dstClass) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    } break;
    default: {
        return false;
    } break;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
