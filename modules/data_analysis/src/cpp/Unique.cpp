//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Unique.hpp"
#include "UniqueReal.hpp"
#include "UniqueRealRows.hpp"
#include "UniqueComplex.hpp"
#include "UniqueComplexRows.hpp"
#include "UniqueInteger.hpp"
#include "UniqueIntegerRows.hpp"
#include "UniqueString.hpp"
#include "UniqueStringRows.hpp"
#include "UniqueCell.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Unique(const ArrayOf& input, int nLhs, bool asRows, bool& needOverload)
{
    needOverload = false;
    switch (input.getDataClass()) {
    case NLS_DOUBLE: {
        if (input.isSparse()) {
            needOverload = true;
            return {};
        }
        if (asRows) {
            return UniqueRealRows<double>(input, nLhs);
        }
        return UniqueReal<double>(input, nLhs);
    } break;
    case NLS_SINGLE: {
        if (asRows) {
            return UniqueRealRows<single>(input, nLhs);
        }
        return UniqueReal<single>(input, nLhs);

    } break;
    case NLS_DCOMPLEX: {
        if (input.isSparse()) {
            needOverload = true;
            return {};
        }
        if (asRows) {
            return UniqueComplexRows<double>(input, nLhs);
        }
        return UniqueComplex<double>(input, nLhs);
    } break;
    case NLS_SCOMPLEX: {
        if (asRows) {
            return UniqueComplexRows<single>(input, nLhs);
        }
        return UniqueComplex<single>(input, nLhs);
    } break;
    case NLS_INT8: {
        if (asRows) {
            return UniqueIntegerRows<int8>(input, nLhs);
        }
        return UniqueInteger<int8>(input, nLhs);
    } break;
    case NLS_INT16: {
        if (asRows) {
            return UniqueIntegerRows<int16>(input, nLhs);
        }
        return UniqueInteger<int16>(input, nLhs);
    } break;
    case NLS_INT32: {
        if (asRows) {
            return UniqueIntegerRows<int32>(input, nLhs);
        }
        return UniqueInteger<int32>(input, nLhs);
    } break;
    case NLS_INT64: {
        if (asRows) {
            return UniqueIntegerRows<int64>(input, nLhs);
        }
        return UniqueInteger<int64>(input, nLhs);
    } break;
    case NLS_UINT8: {
        if (asRows) {
            return UniqueIntegerRows<uint8>(input, nLhs);
        }
        return UniqueInteger<uint8>(input, nLhs);
    } break;
    case NLS_UINT16: {
        if (asRows) {
            return UniqueIntegerRows<uint16>(input, nLhs);
        }
        return UniqueInteger<uint16>(input, nLhs);
    } break;
    case NLS_UINT32: {
        if (asRows) {
            return UniqueIntegerRows<uint32>(input, nLhs);
        }
        return UniqueInteger<uint32>(input, nLhs);
    } break;
    case NLS_UINT64: {
        if (asRows) {
            return UniqueIntegerRows<uint64>(input, nLhs);
        }
        return UniqueInteger<uint64>(input, nLhs);
    } break;
    case NLS_LOGICAL: {
        if (input.isSparse()) {
            needOverload = true;
            return {};
        }
        if (asRows) {
            return UniqueIntegerRows<logical>(input, nLhs);
        }
        return UniqueInteger<logical>(input, nLhs);
    } break;
    case NLS_CHAR: {
        if (asRows) {
            return UniqueIntegerRows<charType>(input, nLhs);
        }
        return UniqueInteger<charType>(input, nLhs);
    } break;
    case NLS_CELL_ARRAY: {
        return UniqueCell(input, nLhs);
    } break;
    case NLS_STRING_ARRAY: {
        if (asRows) {
            return UniqueStringRows(input, nLhs);
        }
        return UniqueString(input, nLhs);
    } break;
    case NLS_STRUCT_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_UNKNOWN:
    default: {
        needOverload = true;
    } break;
    }
    return {};
}
//=============================================================================
}
//=============================================================================
