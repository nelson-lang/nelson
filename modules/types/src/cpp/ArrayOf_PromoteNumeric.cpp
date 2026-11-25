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
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "nlsBuildConfig.h"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ArrayOf_PromoteTypeKernels.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Numeric/char/missing source promotion extracted from ArrayOf::promoteType. It reads
// the source buffer and returns a freshly allocated destination buffer, or nullptr when
// the conversion is a no-op (same class). It touches no private ArrayOf state.
void*
promoteNumericData(NelsonType srcClass, NelsonType dstClass, const void* src, indexType count)
{
    void* dstPtr = nullptr;
    switch (srcClass) {
    case NLS_LOGICAL: {
        const logical* sp = (const logical*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            return nullptr;
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
        const uint8* sp = (const uint8*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<uint8, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            return nullptr;
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, uint64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint8, int64>(srcClass, dstClass, src, dstPtr, count);
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
        const int8* sp = (const int8*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<int8, logical>(dstClass, sp, count);
        } break;
        case NLS_INT8: {
            return nullptr;
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, uint64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int8, int64>(srcClass, dstClass, src, dstPtr, count);
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
        const uint16* sp = static_cast<const uint16*>(src);
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<uint16, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            return nullptr;
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, uint64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint16, int64>(srcClass, dstClass, src, dstPtr, count);
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
        const int16* sp = (const int16*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<int16, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            return nullptr;
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, uint64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int16, int64>(srcClass, dstClass, src, dstPtr, count);
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
        const uint32* sp = (const uint32*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<uint32, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            return nullptr;
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, uint64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint32, int64>(srcClass, dstClass, src, dstPtr, count);
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
        const int32* sp = (const int32*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<int32, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            return nullptr;
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, uint64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int32, int64>(srcClass, dstClass, src, dstPtr, count);
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
        const uint64* sp = (const uint64*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<uint64, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            return nullptr;
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<uint64, int64>(srcClass, dstClass, src, dstPtr, count);
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
        const int64* sp = (const int64*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<int64, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            return nullptr;
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<int64, uint64>(srcClass, dstClass, src, dstPtr, count);
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
        const single* sp = (const single*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<single, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, uint64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<single, int64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_SINGLE: {
            return nullptr;
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
        const double* sp = (const double*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = promoteAsLogical<double, logical>(dstClass, sp, count);
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, uint64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<double, int64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_SINGLE: {
            dstPtr = promoteAsReal<double, single>(dstClass, sp, count);
        } break;
        case NLS_DOUBLE: {
            return nullptr;
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
        const single* sp = (const single*)src;
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
            return nullptr;
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
        const double* sp = (const double*)src;
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
            return nullptr;
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
        charType* sp = (charType*)src;
        switch (dstClass) {
        case NLS_LOGICAL: {
            dstPtr = static_cast<void*>(promoteAsLogical<charType, logical>(dstClass, sp, count));
        } break;
        case NLS_UINT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, uint8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT8: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, int8>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, uint16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT16: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, int16>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, uint32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT32: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, int32>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_UINT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, uint64>(srcClass, dstClass, src, dstPtr, count);
        } break;
        case NLS_INT64: {
            dstPtr = ArrayOf::allocateArrayOf(dstClass, count);
            saturate<charType, int64>(srcClass, dstClass, src, dstPtr, count);
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
            return nullptr;
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
            return nullptr;
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
    return dstPtr;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
