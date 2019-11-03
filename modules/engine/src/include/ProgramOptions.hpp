//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class Option
{
public:
    Option(const std::wstring& fullOption, const std::wstring& shortOption,
        const std::wstring& description, bool bMultiple, bool bWithFieldValue);
    ~Option();
    std::wstring
    getFullOption();
    std::wstring
    getShortOption();
    std::wstring
    getDescription();
    std::wstring
    getFullDescription();

    bool
    isMultiple();
    bool
    withFieldValue();
    bool
    isSameFieldname(const std::wstring& name);

private:
    std::wstring _fullOption;
    std::wstring _shortOption;
    std::wstring _description;
    bool _bMultiple;
    bool _bWithFieldValue;
};
//=============================================================================
class ProgramOptions
{
public:
    ProgramOptions(wstringVector args, NELSON_ENGINE_MODE mode);
    ~ProgramOptions();
    bool
    isValid();
    std::wstring
    getErrorMessage();
    std::wstring
    getFileToExecute();
    std::wstring
    getCommandToExecute();
    std::wstring
    getOptionsHelp();
    std::wstring
    getLanguage();
    std::wstring
    getSocketIoUri();
    uint64
    getTimeout();
    wstringVector
    getFilesToOpen();
    wstringVector
    getFilesToLoad();
    bool
    haveOptionsHelp();
    bool
    haveVersion();
    bool
    haveFileToExecute();
    bool
    haveCommandToExecute();
    bool
    haveLanguage();
    bool
    haveNoStartup();
    bool
    haveNoUserStartup();
    bool
    haveNoUserModules();
    bool
    haveQuietMode();
    bool
    haveTimeout();
    bool
    haveOpenFiles();
    bool
    haveLoadFiles();
    bool
    haveSocketIoUri();

private:
    bool
    parse();
    bool
    parseOption(Option& op, bool& bFind);
    bool
    parseOptionWithValue(Option& op, bool& bFind, std::wstring& value);
    bool
    parseOptionWithValues(Option& op, wstringVector& values);
    bool _isvalid;
    bool _ishelp;
    bool _isversion;
    bool _startup;
    bool _userstartup;
    bool _usermodules;
    bool _quietmode;
    std::wstring _error;
    std::wstring _file;
    std::wstring _command;
    std::wstring _options;
    std::wstring _lang;
    std::wstring _socketioUri;
    uint64 _timeout;
    wstringVector _args;
    wstringVector _filesToOpen;
    wstringVector _filesToLoad;
    NELSON_ENGINE_MODE _mode;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
