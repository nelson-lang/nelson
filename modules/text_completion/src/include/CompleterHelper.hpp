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
#include "i18n.hpp"
#include "nlsText_completion_exports.h"
#include "Types.hpp"
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
class ArrayOf;
class Context;
class Evaluator;
//=============================================================================
// Completion contract shared by every frontend (replxx, Qt terminal, text
// editor, web console/editor):
// - computeCompletion returns completionPrefix, the exact suffix of the input
//   line that must be replaced (possibly empty, e.g. after a trailing dot),
// - every returned entry is the full replacement text for that suffix.
// Frontends must not perform any other text surgery.
//=============================================================================
// Returns the token to replace at the end of line: the leaf file name for path
// completions, otherwise the trailing identifier-like token (empty when the
// line ends with a separator symbol such as '.').
NLSTEXT_COMPLETION_IMPEXP std::wstring
getCompletionTokenToReplace(const std::wstring& line, bool isPathCompletion);
//=============================================================================
// Applies one completion entry to currentLine following the contract above.
NLSTEXT_COMPLETION_IMPEXP std::wstring
completerLine(
    const std::wstring& currentLine, const std::wstring& stringToAdd, bool stringToAddIsPath);
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP std::wstring
getPartialLineAsPath(const std::wstring& line);
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP std::wstring
getPartialLine(const std::wstring& line);
//=============================================================================
// True when completing line would list files (path, file command or quoted string).
NLSTEXT_COMPLETION_IMPEXP bool
isFileCompletionContext(
    const std::wstring& line, const std::wstring& filepart, const std::wstring& textpart);
//=============================================================================
std::string
getLookupSymbolFromCompletionExpression(const std::string& expression);
//=============================================================================
bool
lookupCompletionVariable(Context* context, const std::string& variableName, ArrayOf& value);
//=============================================================================
bool
resolveCompletionExpressionValue(
    Context* context, Evaluator* eval, const std::string& expression, ArrayOf& value);
//=============================================================================
bool
isValidCompletionHandle(const ArrayOf& value);
//=============================================================================
Evaluator*
getCompletionEvaluator();
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP bool
computeCompletion(Evaluator* eval, const std::wstring& line, std::wstring& completionPrefix,
    wstringVector& files, wstringVector& builtin, wstringVector& macros, wstringVector& variables,
    wstringVector& fields, wstringVector& properties, wstringVector& methods);
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP bool
computeCompletion(const std::wstring& line, std::wstring& completionPrefix, wstringVector& files,
    wstringVector& builtin, wstringVector& macros, wstringVector& variables, wstringVector& fields,
    wstringVector& properties, wstringVector& methods);
//=============================================================================
} // namespace Nelson
//=============================================================================
