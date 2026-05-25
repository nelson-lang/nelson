//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CompleterHelper.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
Evaluator*
getCompletionEvaluator()
{
    return nullptr;
}
//=============================================================================
static size_t
searchMatchingPrefixAndSuffix(const std::wstring& line, const std::wstring& toFind)
{
    return 0;
}
//=============================================================================
std::wstring
getPartialLine(const std::wstring& line)
{
    return line;
}
//=============================================================================
std::string
getLookupSymbolFromCompletionExpression(const std::string& expression)
{
    std::string symbol = expression;
    size_t lastCharacterIndex = symbol.find_last_of(" \t,;");
    if (lastCharacterIndex != std::string::npos) {
        symbol = symbol.substr(lastCharacterIndex + 1);
    }

    size_t indexingStart = symbol.find_first_of("([{");
    if (indexingStart != std::string::npos) {
        symbol = symbol.substr(0, indexingStart);
    }
    return symbol;
}
//=============================================================================
std::wstring
getPartialLineAsPath(const std::wstring& line)
{
    if (line.empty()) {
        return {};
    }

    return line;
}
//=============================================================================
std::wstring
getCompletionLeafPrefix(const std::wstring& completionPrefix)
{
    size_t lastSep = completionPrefix.find_last_of(L"/\\");
    if (lastSep == std::wstring::npos) {
        return completionPrefix;
    }
    return completionPrefix.substr(lastSep + 1);
}
//=============================================================================
std::wstring
completerLine(const std::wstring& currentLine, const std::wstring& stringToAdd,
    const std::wstring& filePattern, const std::wstring& defaultPattern, bool stringToAddIsPath)
{
    std::wstring lineModified;
    if (currentLine.empty()) {
        return lineModified;
    }
    return currentLine;
}
//=============================================================================
bool
lookupCompletionVariable(Context* context, const std::string& variableName, ArrayOf& value)
{
    return false;
}
//=============================================================================
bool
computeCompletion(const std::wstring& line, std::wstring& completionPrefix, wstringVector& files,
    wstringVector& builtin, wstringVector& macros, wstringVector& variables, wstringVector& fields,
    wstringVector& properties, wstringVector& methods)
{
    completionPrefix = line;
    return false;
}
//=============================================================================
bool
computeCompletion(Evaluator* eval, const std::wstring& line, std::wstring& completionPrefix,
    wstringVector& files, wstringVector& builtin, wstringVector& macros, wstringVector& variables,
    wstringVector& fields, wstringVector& properties, wstringVector& methods)
{
    return computeCompletion(
        line, completionPrefix, files, builtin, macros, variables, fields, properties, methods);
}
//=============================================================================
stringVector
getCompletionDictionary(std::wstring& completionPrefix)
{
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
