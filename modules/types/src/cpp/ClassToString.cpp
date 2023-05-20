//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include "ClassToString.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::map<NelsonType, std::wstring> typeNames
    = { { NLS_GO_HANDLE, TOWSTRING(NLS_GO_HANDLE_STR) }, { NLS_HANDLE, TOWSTRING(NLS_HANDLE_STR) },
          { NLS_CELL_ARRAY, TOWSTRING(NLS_CELL_ARRAY_STR) },
          { NLS_STRING_ARRAY, TOWSTRING(NLS_STRING_ARRAY_STR) },
          { NLS_STRUCT_ARRAY, TOWSTRING(NLS_STRUCT_ARRAY_STR) },
          { NLS_DCOMPLEX, TOWSTRING(NLS_DOUBLE_STR) }, { NLS_DOUBLE, TOWSTRING(NLS_DOUBLE_STR) },
          { NLS_SCOMPLEX, TOWSTRING(NLS_SINGLE_STR) }, { NLS_SINGLE, TOWSTRING(NLS_SINGLE_STR) },
          { NLS_LOGICAL, TOWSTRING(NLS_LOGICAL_STR) }, { NLS_UINT8, TOWSTRING(NLS_UINT8_STR) },
          { NLS_INT8, TOWSTRING(NLS_INT8_STR) }, { NLS_UINT16, TOWSTRING(NLS_UINT16_STR) },
          { NLS_INT16, TOWSTRING(NLS_INT16_STR) }, { NLS_UINT32, TOWSTRING(NLS_UINT32_STR) },
          { NLS_INT32, TOWSTRING(NLS_INT32_STR) }, { NLS_UINT64, TOWSTRING(NLS_UINT64_STR) },
          { NLS_INT64, TOWSTRING(NLS_INT64_STR) }, { NLS_CHAR, TOWSTRING(NLS_CHAR_STR) } };

//=============================================================================
std::wstring
ClassToString(NelsonType classType)
{
    auto it = typeNames.find(classType);
    if (it != typeNames.end()) {
        return it->second;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
