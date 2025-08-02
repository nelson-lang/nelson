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
#define CHAR_TO_STRING(ch) #ch
//=============================================================================
#define CAT_4_STRINGS(S1, S2, S3, S4) S1 S2 S3 S4
//=============================================================================
#define OVERLOAD_SYMBOL_CHAR '@'
#define OVERLOAD_SYMBOL_STR CHAR_TO_STRING(OVERLOAD_SYMBOL_CHAR)
//=============================================================================
#define OVERLOAD_FUNCTION_NAME(typeName, operatorName)                                             \
    CAT_4_STRINGS("@", typeName, "/", operatorName)
//=============================================================================
namespace Nelson {
//=============================================================================
inline std::string
getOverloadFunctionName(const std::string& typeName, const std::string& functionName)
{
    std::string result;
    result.reserve(2 + typeName.length() + functionName.length());
    result += "@";
    result += typeName;
    result += "/";
    result += functionName;
    return result;
}
//=============================================================================
}
