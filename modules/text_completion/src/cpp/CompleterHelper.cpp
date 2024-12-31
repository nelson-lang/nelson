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
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "IsValidVariableName.hpp"
#include "BuiltinCompleter.hpp"
#include "FileCompleter.hpp"
#include "MacroCompleter.hpp"
#include "VariableCompleter.hpp"
#include "FieldCompleter.hpp"
#include "MethodCompleter.hpp"
#include "PropertyCompleter.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
static size_t
searchMatchingPrefixAndSuffix(const std::wstring& line, const std::wstring& toFind)
{
    // line: cd c:/Program Fi
    // toFind: Program Files (x86)/
    std::wstring toFindCopy = std::wstring(toFind);
    StringHelpers::to_upper(toFindCopy);
    auto* pointerToFindCopy = const_cast<wchar_t*>(toFindCopy.c_str());
    wchar_t lastChar = towupper(*line.rbegin());
    size_t lineLength = line.size();
    wchar_t* movingPointerToFindCopy = nullptr;
    do {
        movingPointerToFindCopy = wcsrchr(pointerToFindCopy, lastChar);
        if (movingPointerToFindCopy == nullptr) {
            break;
        }
        movingPointerToFindCopy[0] = '\0';
        auto* pointerOnLine
            = const_cast<wchar_t*>(line.c_str() + lineLength - wcslen(pointerToFindCopy) - 1);
        std::wstring toFindModified = std::wstring(pointerToFindCopy);
        std::wstring lineModified = std::wstring(pointerOnLine);
        if (StringHelpers::iequals(toFindModified, lineModified.substr(0, toFindModified.size()))) {
            return (pointerOnLine - line.c_str());
        }
    } while (movingPointerToFindCopy != nullptr);
    return lineLength;
}
//=============================================================================
static inline bool
isCharacterInSymbols(wchar_t character, const std::wstring& symbols)
{
    return std::find(symbols.begin(), symbols.end(), character) != symbols.end();
}
//=============================================================================
std::wstring
getPartialLine(const std::wstring& line)
{
    std::wstring symbols = L"+-*/\\([ ^,;={.&|\'])}:'><~@\t";
    size_t index = std::wstring::npos;
    for (wchar_t symbol : symbols) {
        size_t pch = line.rfind(symbol);
        if (pch != std::wstring::npos) {
            if (index == std::wstring::npos) {
                index = pch;
            } else {
                index = std::max(index, pch);
            }
        }
    }

    if (index != std::wstring::npos && index + 1 < line.length()) {
        std::wstring prefix = line.substr(0, index);
        if (!IsValidVariableName(prefix) || isCharacterInSymbols(line[index], symbols)) {
            return line.substr(index + 1);
        }
    }
    return line;
}
//=============================================================================
std::wstring
getPartialLineAsPath(const std::wstring& line)
{
    if (line.empty()) {
        return {};
    }
    std::wstring symbols = std::wstring(L";,'");
    std::wstring lineWithoutSpaceAtBeginning = line;
    StringHelpers::trim_left(lineWithoutSpaceAtBeginning);
    if (lineWithoutSpaceAtBeginning.empty()) {
        return {};
    }
    size_t lengthLine = line.size();
    std::wstring returnedLine;
    bool symbol_found = false;
    size_t index = 0;
    // cd c:/Program Files (x86)/
    for (wchar_t symbol : symbols) {
        std::string::size_type filePos = lineWithoutSpaceAtBeginning.rfind(symbol);
        if (filePos != std::wstring::npos) {
            index = std::max(index, filePos);
            symbol_found = true;
            break;
        }
    }
    if (!symbol_found) {
        // example: cd c:/
        size_t lastPositionBlank = lineWithoutSpaceAtBeginning.rfind(L' ');
        size_t firstPositionBlank = lineWithoutSpaceAtBeginning.find(L' ');
        if (lastPositionBlank != std::wstring::npos && firstPositionBlank != std::wstring::npos) {
            symbol_found = true;
            index = firstPositionBlank;
        }
    }
    if (symbol_found) {
        index++;
    }
    while (lineWithoutSpaceAtBeginning[index] == L' ') {
        if (index + 1 >= lengthLine) {
            break;
        }
        index++;
    }
    returnedLine = lineWithoutSpaceAtBeginning.substr(index);

    return returnedLine;
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
    size_t lengthCurrentLine = currentLine.size();
    if (stringToAdd.empty()) {
        lineModified = currentLine;
        return lineModified;
    }
    if (stringToAddIsPath) {
        std::wstring filePatternBuf;
        bool bfilePatternBuf = false;
        if (!filePattern.empty()) {
            filePatternBuf = filePattern;
        } else {
            filePatternBuf = getPartialLineAsPath(currentLine);
            bfilePatternBuf = true;
        }
        if (!filePatternBuf.empty()) {
            std::wstring res;
            FileSystemWrapper::Path pathToSplit(filePatternBuf);
            if (pathToSplit.has_parent_path()) {
                res = pathToSplit.parent_path().generic_wstring();
            }
            if (res.empty()) {
                FileSystemWrapper::Path pwd = FileSystemWrapper::Path::current_path();
                res = pwd.generic_wstring();
                if (res.empty()) {
                    lineModified = currentLine;
                    return lineModified;
                }
            }
        }
    }
    size_t iposInsert = searchMatchingPrefixAndSuffix(currentLine, stringToAdd);
    if ((currentLine[lengthCurrentLine - 1] == L'/')
        || (currentLine[lengthCurrentLine - 1] == L'\\')) {
        iposInsert = lengthCurrentLine;
    }
    lineModified = currentLine.substr(0, iposInsert);
    lineModified = lineModified + stringToAdd;
    return lineModified;
}
//=============================================================================
bool
computeCompletion(const std::wstring& line, std::wstring& completionPrefix, wstringVector& files,
    wstringVector& builtin, wstringVector& macros, wstringVector& variables, wstringVector& fields,
    wstringVector& properties, wstringVector& methods)
{
    bool showpopup = false;
    completionPrefix = line;
    std::wstring filepart = getPartialLineAsPath(completionPrefix);
    files = FileCompleter(filepart);
    std::wstring textpart = getPartialLine(completionPrefix);
    if (!filepart.empty() && !files.empty() && (filepart != textpart)) {
        showpopup = true;
    } else if (!textpart.empty()) {
        size_t index = completionPrefix.size() - textpart.size();
        std::wstring prefix;
        if ((index > 1) && (completionPrefix[index - 1] == L'.')) {
            prefix = completionPrefix.substr(0, index);
        }

        builtin = BuiltinCompleter(prefix + textpart);
        macros = MacroCompleter(prefix + textpart);
        variables = VariableCompleter(prefix + textpart);

        fields = FieldCompleter(prefix + textpart);
        properties = PropertyCompleter(prefix + textpart);
        methods = MethodCompleter(prefix + textpart);

        if (!files.empty() || !builtin.empty() || !macros.empty() || !variables.empty()
            || !fields.empty() || !properties.empty() || !methods.empty()) {
            completionPrefix = textpart;
            showpopup = true;
        }
    }
    return showpopup;
}
//=============================================================================
stringVector
getCompletionDictionary(std::wstring& completionPrefix)
{
    stringVector dictionary;
    wstringVector tempResult;

    // Concatenate results from different completers
    auto appendResults = [&](const wstringVector& results) {
        tempResult.insert(tempResult.end(), results.begin(), results.end());
    };

    appendResults(BuiltinCompleter(completionPrefix));
    appendResults(MacroCompleter(completionPrefix));
    appendResults(VariableCompleter(completionPrefix));
    appendResults(FieldCompleter(completionPrefix));
    appendResults(PropertyCompleter(completionPrefix));
    appendResults(MethodCompleter(completionPrefix));

    // Sort and remove duplicates
    std::sort(tempResult.begin(), tempResult.end());
    tempResult.erase(std::unique(tempResult.begin(), tempResult.end()), tempResult.end());

    // Convert wstringVector to stringVector
    for (const auto& item : tempResult) {
        dictionary.push_back(wstring_to_utf8(item));
    }
    return dictionary;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
