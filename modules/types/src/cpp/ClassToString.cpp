//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassToString.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
ClassToString(NelsonType classType)
{
    std::wstring classString;
    switch (classType) {
    case NLS_GO_HANDLE: {
        classString = utf8_to_wstring(NLS_GO_HANDLE_STR);
    } break;
    case NLS_HANDLE: {
        classString = utf8_to_wstring(NLS_HANDLE_STR);
    } break;
    case NLS_CELL_ARRAY: {
        classString = utf8_to_wstring(NLS_CELL_ARRAY_STR);
    } break;
    case NLS_STRING_ARRAY: {
        classString = utf8_to_wstring(NLS_STRING_ARRAY_STR);
    } break;
    case NLS_STRUCT_ARRAY: {
        classString = utf8_to_wstring(NLS_STRUCT_ARRAY_STR);
    } break;
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        classString = utf8_to_wstring(NLS_DOUBLE_STR);
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        classString = utf8_to_wstring(NLS_SINGLE_STR);
    } break;
    case NLS_LOGICAL:
        classString = utf8_to_wstring(NLS_LOGICAL_STR);
        break;
    case NLS_UINT8:
        classString = utf8_to_wstring(NLS_UINT8_STR);
        break;
    case NLS_INT8:
        classString = utf8_to_wstring(NLS_INT8_STR);
        break;
    case NLS_UINT16:
        classString = utf8_to_wstring(NLS_UINT16_STR);
        break;
    case NLS_INT16:
        classString = utf8_to_wstring(NLS_INT16_STR);
        break;
    case NLS_UINT32:
        classString = utf8_to_wstring(NLS_UINT32_STR);
        break;
    case NLS_INT32:
        classString = utf8_to_wstring(NLS_INT32_STR);
        break;
    case NLS_UINT64:
        classString = utf8_to_wstring(NLS_UINT64_STR);
        break;
    case NLS_INT64:
        classString = utf8_to_wstring(NLS_INT64_STR);
        break;
    case NLS_CHAR:
        classString = utf8_to_wstring(NLS_CHAR_STR);
        break;
    default:
        break;
    }
    return classString;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
