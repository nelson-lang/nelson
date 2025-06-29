//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "ClassToString.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// !!! Warning !!!
// Must be synchro with types.hpp
static std::vector<std::string> typeNames
    = { NLS_DOUBLE_STR, NLS_SINGLE_STR, NLS_DOUBLE_STR, /* NLS_DCOMPLEX */
          NLS_SINGLE_STR, /* NLS_SCOMPLEX */
          NLS_INT8_STR, NLS_INT16_STR, NLS_INT32_STR, NLS_INT64_STR, NLS_UINT8_STR, NLS_UINT16_STR,
          NLS_UINT32_STR, NLS_UINT64_STR, NLS_LOGICAL_STR, NLS_MISSING_ARRAY_STR, NLS_CHAR_STR,
          NLS_STRUCT_ARRAY_STR, NLS_CELL_ARRAY_STR, NLS_STRING_ARRAY_STR, NLS_FUNCTION_HANDLE_STR,
          NLS_CLASS_ARRAY_STR, NLS_HANDLE_STR, NLS_GO_HANDLE_STR, NLS_UNKNOWN_STR };
//=============================================================================
std::string
ClassToString(NelsonType classType)
{
    if (classType < 0 || classType > typeNames.size()) {
        return NLS_UNKNOWN_STR;
    }
    return typeNames[classType];
}
//=============================================================================
std::wstring
ClassToStringW(NelsonType classType)
{
    return utf8_to_wstring(typeNames[classType]);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
