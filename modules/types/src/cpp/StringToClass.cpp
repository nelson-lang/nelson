//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <unordered_map>
#include "StringToClass.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::unordered_map<std::string, NelsonType> stringToType = {

    { NLS_DOUBLE_STR, NLS_DOUBLE },
    { NLS_SINGLE_STR, NLS_SINGLE },
    { NLS_DOUBLE_STR, NLS_DCOMPLEX },
    { NLS_SINGLE_STR, NLS_SCOMPLEX },
    { NLS_UINT8_STR, NLS_UINT8 },
    { NLS_INT8_STR, NLS_INT8 },
    { NLS_UINT16_STR, NLS_UINT16 },
    { NLS_INT16_STR, NLS_INT16 },
    { NLS_UINT32_STR, NLS_UINT32 },
    { NLS_INT32_STR, NLS_INT32 },
    { NLS_UINT64_STR, NLS_UINT64 },
    { NLS_INT64_STR, NLS_INT64 },
    { NLS_LOGICAL_STR, NLS_LOGICAL },
    { NLS_CHAR_STR, NLS_CHAR },
    { NLS_STRUCT_ARRAY_STR, NLS_STRUCT_ARRAY },
    { NLS_CELL_ARRAY_STR, NLS_CELL_ARRAY },
    { NLS_STRING_ARRAY_STR, NLS_STRING_ARRAY },
    { NLS_MISSING_ARRAY_STR, NLS_MISSING_ARRAY },
    { NLS_CLASS_ARRAY_STR, NLS_CLASS_ARRAY },
    { NLS_HANDLE_STR, NLS_HANDLE },
    { NLS_GO_HANDLE_STR, NLS_GO_HANDLE },
    { NLS_FUNCTION_HANDLE_STR, NLS_FUNCTION_HANDLE },
    { NLS_UNKNOWN_STR, NLS_UNKNOWN },
};
//=============================================================================
NelsonType
StringToClass(const std::string& classname, bool& haveError)
{
    auto it = stringToType.find(classname);
    if (it != stringToType.end()) {
        haveError = false;
        return it->second;
    }
    haveError = true;
    return NLS_UNKNOWN;
}
//=============================================================================
NelsonType
StringToClass(const std::string& classname)
{
    bool haveError;
    NelsonType destClass = StringToClass(classname, haveError);
    if (haveError) {
        Error(_W("input must be a valid class name."));
    }
    return destClass;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
