//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
computeCompletion(const std::wstring& line, std::wstring& completionPrefix, wstringVector& files,
    wstringVector& builtin, wstringVector& macros, wstringVector& variables, wstringVector& fields,
    wstringVector& properties, wstringVector& methods)
{
    return false;
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
