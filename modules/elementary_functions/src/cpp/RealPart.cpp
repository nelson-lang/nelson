//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "RealPart.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
RealPart(const ArrayOf& arrayIn)
{
    ArrayOf res;
    if (arrayIn.isSparse()) {
        Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_real'");
    }
    switch (arrayIn.getDataClass()) {
    case NLS_SCOMPLEX: {
        size_t len = arrayIn.getElementCount();
        void* ptr = ArrayOf::allocateArrayOf(NLS_SINGLE, len, stringVector(), true);
        auto* rp = static_cast<single*>(ptr);
        auto* sp = (single*)arrayIn.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            rp[i] = sp[2 * i];
        }
        res = ArrayOf(NLS_SINGLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_DCOMPLEX: {
        size_t len = arrayIn.getElementCount();
        void* ptr = ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), true);
        auto* rp = static_cast<double*>(ptr);
        auto* dp = (double*)arrayIn.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            rp[i] = dp[2 * i];
        }
        res = ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY:
    default: {
        Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_real'");
    } break;
    case NLS_CHAR: {
        size_t len = arrayIn.getElementCount();
        void* ptr = ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
        auto* rp = static_cast<double*>(ptr);
        auto* dp = (charType*)arrayIn.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            rp[i] = static_cast<double>(dp[i]);
        }
        res = ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_LOGICAL: {
        size_t len = arrayIn.getElementCount();
        void* ptr = ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
        auto* rp = static_cast<double*>(ptr);
        auto* dp = (logical*)arrayIn.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            rp[i] = (dp[i] == 0 ? 0 : 1);
        }
        res = ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_DOUBLE:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE: {
        res = arrayIn;
        res.ensureSingleOwner();
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
