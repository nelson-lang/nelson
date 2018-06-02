//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "CompleterHelper.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static size_t
searchMatchingPrefixAndSuffix(std::wstring line, std::wstring toFind)
{
    // line: cd c:/Program Fi
    // toFind: Program Files (x86)/
    std::wstring toFindCopy = std::wstring(toFind);
    boost::to_upper(toFindCopy);
    wchar_t* pointerToFindCopy = (wchar_t*)toFindCopy.c_str();
    wchar_t lastChar = towupper(*line.rbegin());
    size_t lineLength = line.size();
    wchar_t* movingPointerToFindCopy = nullptr;
    do {
        movingPointerToFindCopy = wcsrchr(pointerToFindCopy, lastChar);
        if (movingPointerToFindCopy == nullptr) {
            break;
        }
        movingPointerToFindCopy[0] = '\0';
        wchar_t* pointerOnLine
            = (wchar_t*)(line.c_str() + lineLength - wcslen(pointerToFindCopy) - 1);
        std::wstring toFindModified = std::wstring(pointerToFindCopy);
        std::wstring lineModified = std::wstring(pointerOnLine);
        if (boost::iequals(toFindModified, lineModified.substr(0, toFindModified.size()))) {
            return (pointerOnLine - line.c_str());
        }
    } while (movingPointerToFindCopy != nullptr);
    return lineLength;
}
//=============================================================================
std::wstring
getPartialLine(std::wstring line)
{
    std::wstring symbols = L"+-*/\\([ ^,;={.&|\'])}:\'><~@\t";
    size_t index = -1;
    for (size_t i = 0; i < symbols.size(); i++) {
        size_t len = 0;
        size_t pch = line.rfind(symbols[i]);
        if (pch != std::wstring::npos) {
            len = pch;
            index = std::max(index, len);
        }
    }
    return line.substr(index + 1);
}
//=============================================================================
std::wstring
getPartialLineAsPath(std::wstring line)
{
    if (line.empty()) {
        return std::wstring();
    }
    std::wstring symbols = std::wstring(L";,'");
    std::wstring lineWithoutSpaceAtBeginning = line;
    boost::trim_left(lineWithoutSpaceAtBeginning);
    if (lineWithoutSpaceAtBeginning.empty()) {
        return std::wstring();
    }
    size_t lengthLine = line.size();
    std::wstring returnedLine;
    bool symbol_found = false;
    size_t index = 0;
    // cd c:/Program Files (x86)/
    for (size_t i = 0; i < symbols.size(); i++) {
        std::string::size_type filePos = lineWithoutSpaceAtBeginning.rfind(symbols[i]);
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
            } else {
                index++;
            }
        }
        returnedLine = lineWithoutSpaceAtBeginning.substr(index);
    }
    return returnedLine;
}
//=============================================================================
std::wstring
completerLine(std::wstring currentLine, std::wstring stringToAdd, std::wstring filePattern,
    std::wstring defaultPattern, bool stringToAddIsPath)
{
    std::wstring lineModified;
    if (currentLine.empty()) {
        return lineModified;
    }
    size_t lengthCurrentLine = currentLine.size();
    if (stringToAdd.empty()) {
        size_t lengthLineModified = lengthCurrentLine;
        lineModified = currentLine;
        return lineModified;
    }
    if (stringToAddIsPath == true) {
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
            boost::filesystem::path pathToSplit = filePatternBuf;
            if (pathToSplit.has_parent_path()) {
                res = pathToSplit.parent_path().generic_wstring();
            }
            if (res.empty()) {
                try {
                    boost::filesystem::path pwd = boost::filesystem::current_path();
                    res = pwd.generic_wstring();
                } catch (const boost::filesystem::filesystem_error&) {
                }
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
}
