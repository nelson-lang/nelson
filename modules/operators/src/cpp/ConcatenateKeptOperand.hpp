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
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// The operand kept next to an empty one still takes the numeric, logical or
// char class that every operand decides together ([int8([]), 1] is int8).
inline ArrayOf
concatenateKeptOperand(const ArrayOf& value, NelsonType commonType)
{
    ArrayOf kept(value);
    switch (commonType) {
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
        kept.promoteType(commonType);
    } break;
    default: {
    } break;
    }
    return kept;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
