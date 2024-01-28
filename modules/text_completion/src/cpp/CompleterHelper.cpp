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
std::wstring
getPartialLine(const std::wstring& line)
{
    std::wstring symbols = L"+-*/\\([ ^,;={.&|\'])}:\'><~@\t";
    size_t index = std::wstring::npos;
    for (wchar_t symbol : symbols) {
        size_t len = 0;
        size_t pch = line.rfind(symbol);
        if (pch != std::wstring::npos) {
            len = pch;
            if (index == std::wstring::npos) {
                index = len;
            } else {
                index = std::max(index, len);
            }
        }
    }
    return line.substr(index + 1);
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
        while (lineWithoutSpaceAtBeginning[index] == L' ') {
            if (index + 1 >= lengthLine) {
                break;
            }
            index++;
        }
        returnedLine = lineWithoutSpaceAtBeginning.substr(index);
    }
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
} // namespace Nelson
