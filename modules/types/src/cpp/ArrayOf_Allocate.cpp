//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "Error.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
void*
ArrayOf::allocateArrayOf(
    NelsonType type, indexType length, const stringVector& names, bool initializeValues)
{
    switch (type) {
    case NLS_GO_HANDLE:
    case NLS_HANDLE: {
        return (void*)new_with_exception<nelson_handle>(length);
    } break;
    case NLS_STRING_ARRAY: {
        auto* dp = new_with_exception<ArrayOf>(length, false);
        for (indexType i = 0; i < length; i++) {
            dp[i] = ArrayOf(NLS_DOUBLE);
        }
        return dp;
    } break;
    case NLS_CELL_ARRAY: {
        auto* dp = new_with_exception<ArrayOf>(length, false);
        for (indexType i = 0; i < length; i++) {
            dp[i] = ArrayOf(NLS_DOUBLE);
        }
        return dp;
    } break;
    case NLS_FUNCTION_HANDLE: {
        auto n = static_cast<indexType>(length * names.size());
        auto* dp = new_with_exception<ArrayOf>(n, false);
        for (indexType i = 0; i < (n); i++) {
            dp[i] = ArrayOf(NLS_DOUBLE);
        }
        return dp;
    } break;
    case NLS_CLASS_ARRAY: {
        auto n = static_cast<indexType>(length * names.size());
        auto* dp = new_with_exception<ArrayOf>(n, false);
        for (indexType i = 0; i < (n); i++) {
            dp[i] = ArrayOf(NLS_DOUBLE);
        }
        return dp;
    } break;
    case NLS_STRUCT_ARRAY: {
        if (!haveValidFieldNames(names)) {
            Error(_W("Field names must be valid."));
        }
        if (!haveUniqueFieldNames(names)) {
            Error(_W("Duplicated field detected."));
        }
        auto n = static_cast<indexType>(length * names.size());
        auto* dp = new_with_exception<ArrayOf>(n, false);
        for (indexType i = 0; i < (n); i++) {
            dp[i] = ArrayOf(NLS_DOUBLE);
        }
        return dp;
    } break;
    case NLS_LOGICAL: {
        return (void*)new_with_exception<logical>(length, initializeValues);
    } break;
    case NLS_UINT8: {
        return (void*)new_with_exception<uint8>(length, initializeValues);
    } break;
    case NLS_INT8: {
        return (void*)new_with_exception<int8>(length, initializeValues);
    } break;
    case NLS_UINT16: {
        return (void*)new_with_exception<uint16>(length, initializeValues);
    } break;
    case NLS_INT16: {
        return (void*)new_with_exception<int16>(length, initializeValues);
    } break;
    case NLS_UINT32: {
        return (void*)new_with_exception<uint32>(length, initializeValues);
    } break;
    case NLS_INT32: {
        return (void*)new_with_exception<int32>(length, initializeValues);
    } break;
    case NLS_UINT64: {
        return (void*)new_with_exception<uint64>(length, initializeValues);
    } break;
    case NLS_INT64: {
        return (void*)new_with_exception<int64>(length, initializeValues);
    } break;
    case NLS_SINGLE: {
        return (void*)new_with_exception<single>(length, initializeValues);
    } break;
    case NLS_DOUBLE: {
        return (void*)new_with_exception<double>(length, initializeValues);
    } break;
    case NLS_SCOMPLEX: {
        return (void*)new_with_exception<single>(2 * length, initializeValues);
    } break;
    case NLS_DCOMPLEX: {
        return (void*)new_with_exception<double>(2 * length, initializeValues);
    } break;
    case NLS_CHAR: {
        return (void*)new_with_exception<charType>(length, initializeValues);
    } break;
    case NLS_MISSING_ARRAY: {
        return (void*)new_with_exception<double>(length, initializeValues);
    } break;
    default: {
    } break;
    }
    return nullptr;
}
//=============================================================================
void
ArrayOf::deleteArrayOf(void* dp, NelsonType dataclass)
{
    switch (dataclass) {
    case NLS_GO_HANDLE:
    case NLS_HANDLE: {
        auto* rp = static_cast<nelson_handle*>(dp);
        delete[] rp;
    } break;
    case NLS_MISSING_ARRAY: {
        auto* rp = static_cast<double*>(dp);
        delete[] rp;
    } break;
    case NLS_STRING_ARRAY: {
        auto* rp = static_cast<ArrayOf*>(dp);
        delete[] rp;
    } break;
    case NLS_CELL_ARRAY: {
        auto* rp = static_cast<ArrayOf*>(dp);
        delete[] rp;
    } break;
    case NLS_FUNCTION_HANDLE: {
        auto* rp = static_cast<ArrayOf*>(dp);
        delete[] rp;
    } break;
    case NLS_CLASS_ARRAY: {
        auto* rp = static_cast<ArrayOf*>(dp);
        delete[] rp;
    } break;
    case NLS_STRUCT_ARRAY: {
        auto* rp = static_cast<ArrayOf*>(dp);
        delete[] rp;
    } break;
    case NLS_LOGICAL: {
        auto* rp = static_cast<logical*>(dp);
        delete[] rp;
    } break;
    case NLS_UINT8: {
        auto* rp = static_cast<uint8*>(dp);
        delete[] rp;
    } break;
    case NLS_INT8: {
        int8* rp = static_cast<int8*>(dp);
        delete[] rp;
    } break;
    case NLS_UINT16: {
        auto* rp = static_cast<uint16*>(dp);
        delete[] rp;
    } break;
    case NLS_INT16: {
        auto* rp = static_cast<int16*>(dp);
        delete[] rp;
    } break;
    case NLS_UINT32: {
        auto* rp = static_cast<uint32*>(dp);
        delete[] rp;
    } break;
    case NLS_INT32: {
        auto* rp = static_cast<int32*>(dp);
        delete[] rp;
    } break;
    case NLS_UINT64: {
        auto* rp = static_cast<uint64*>(dp);
        delete[] rp;
    } break;
    case NLS_INT64: {
        auto* rp = static_cast<int64*>(dp);
        delete[] rp;
    } break;
    case NLS_SINGLE: {
        auto* rp = static_cast<single*>(dp);
        delete[] rp;
    } break;
    case NLS_DOUBLE: {
        auto* rp = static_cast<double*>(dp);
        delete[] rp;
    } break;
    case NLS_SCOMPLEX: {
        auto* rp = static_cast<single*>(dp);
        delete[] rp;
    } break;
    case NLS_DCOMPLEX: {
        auto* rp = static_cast<double*>(dp);
        delete[] rp;
    } break;
    case NLS_CHAR: {
        auto* rp = static_cast<charType*>(dp);
        delete[] rp;
    } break;
    default: {
    } break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
