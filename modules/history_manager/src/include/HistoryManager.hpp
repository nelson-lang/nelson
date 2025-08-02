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
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
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
