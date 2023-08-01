//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <unordered_map>
#include "StringToClass.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::unordered_map<std::wstring, NelsonType> stringToType = {
    { TOWSTRING(NLS_GO_HANDLE_STR), NLS_GO_HANDLE },
    { TOWSTRING(NLS_HANDLE_STR), NLS_HANDLE },
    { TOWSTRING(NLS_INT8_STR), NLS_INT8 },
    { TOWSTRING(NLS_INT16_STR), NLS_INT16 },
    { TOWSTRING(NLS_INT32_STR), NLS_INT32 },
    { TOWSTRING(NLS_INT64_STR), NLS_INT64 },
    { TOWSTRING(NLS_UINT8_STR), NLS_UINT8 },
    { TOWSTRING(NLS_UINT16_STR), NLS_UINT16 },
    { TOWSTRING(NLS_UINT32_STR), NLS_UINT32 },
    { TOWSTRING(NLS_UINT64_STR), NLS_UINT64 },
    { TOWSTRING(NLS_SINGLE_STR), NLS_SINGLE },
    { TOWSTRING(NLS_DOUBLE_STR), NLS_DOUBLE },
    { TOWSTRING(NLS_LOGICAL_STR), NLS_LOGICAL },
    { TOWSTRING(NLS_CHAR_STR), NLS_CHAR },
    { TOWSTRING(NLS_CELL_ARRAY_STR), NLS_CELL_ARRAY },
    { TOWSTRING(NLS_STRING_ARRAY_STR), NLS_STRING_ARRAY },
    { TOWSTRING(NLS_STRUCT_ARRAY_STR), NLS_STRUCT_ARRAY },
    { TOWSTRING(NLS_CLASS_ARRAY_STR), NLS_CLASS_ARRAY },
    { TOWSTRING(NLS_FUNCTION_HANDLE_STR), NLS_FUNCTION_HANDLE },
};
//=============================================================================
NelsonType
StringToClass(const std::wstring& classname, bool& haveError)
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
StringToClass(const std::wstring& classname)
{
    bool haveError;
    NelsonType destClass = StringToClass(classname, haveError);
    if (haveError) {
        Error(_W("input must be a valid class name."));
    }
    return destClass;
}
//=============================================================================
NelsonType
StringToClass(const std::string& classname)
{
    return StringToClass(utf8_to_wstring(classname));
}
//=============================================================================
NelsonType
StringToClass(const std::string& classname, bool& haveError)
{
    return StringToClass(utf8_to_wstring(classname), haveError);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
