//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "i18n.hpp"
#include "nlsText_completion_exports.h"
#include <string>
//=============================================================================
#define POSTFIX_BUILTIN _W("Builtin")
#define POSTFIX_MACRO _W("Macro")
#define POSTFIX_VARIABLE _W("Variable")
#define POSTFIX_FILES _W("File or directory")
#define POSTFIX_FIELD _W("Field")
#define POSTFIX_METHOD _W("Method")
#define POSTFIX_PROPERTY _W("Property")
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP std::wstring
completerLine(const std::wstring& currentLine, const std::wstring& stringToAdd,
    const std::wstring& filePattern, const std::wstring& defaultPattern, bool stringToAddIsPath);
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP std::wstring
getPartialLineAsPath(const std::wstring& line);
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP std::wstring
getPartialLine(const std::wstring& line);
//=============================================================================
} // namespace Nelson
//=============================================================================
