//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#pragma once
//=============================================================================
#include "Types.hpp"
#include "nlsHistory_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
class NLSHISTORY_MANAGER_IMPEXP HistoryManager
{
    //=============================================================================
private:
    wstringVector commands;
    std::wstring filename;
    std::wstring token;
    int64 token_position;
    wstringVector tokens_found;
    bool bEmptyLineAtNextState;
    bool
    copyPreviousFile(const std::wstring& filename);
    size_t saveLastNCommands;
    size_t saveAfterNCommands;
    size_t nbCommands;
    bool bAllowDuplicatedLines;
    bool bRemoveExit;
    bool
    detectExit(const std::wstring& line);
    bool bSaveEnabled;
    //=============================================================================
public:
    HistoryManager();
    ~HistoryManager();

    bool
    appendLine(const std::wstring& line);
    bool
    appendLines(const wstringVector& lines);

    bool
    remove(size_t pos);
    bool
    remove(size_t firstpos, size_t lastpos);

    std::wstring
    getFirstLine();
    std::wstring
    getLastLine();

    std::wstring
    getPreviousLine();
    std::wstring
    getNextLine();

    bool
    loadFromFile(const std::wstring& filename);
    bool
    loadFromFile();

    bool
    saveToFile(const std::wstring& filename);
    bool
    saveToFile();

    bool
    setFilename(const std::wstring& filename);
    std::wstring
    getFilename();

    bool
    clear(bool bWithHeader);

    bool
    setToken(const std::wstring& token);

    size_t
    getNumberOfLines();
    std::wstring
    getNthLine(size_t N);

    bool
    appendHeader();

    wstringVector
    get();
    std::wstring
    get(size_t pos);
    wstringVector
    get(size_t firstpos, size_t lastpos);

    void
    setLastNCommandsSize(size_t newsize);
    size_t
    getLastNCommandsSize();

    void
    setSaveAfterNCommands(size_t nLines);
    size_t
    getSaveAfterNCommands();

    void
    setAllowDuplicatedLines(bool bAllow = true);
    bool
    getAllowDuplicatedLines();

    void
    setRemoveExit(bool bRemove);
    bool
    getRemoveExit();

    size_t
    getCurrentSize();

    void
    setSaveEnabled(bool bSave);
    bool
    getSaveEnabled();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
