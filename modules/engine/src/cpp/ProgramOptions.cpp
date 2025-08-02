//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "ProgramOptions.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define PREFIX_FULLOPTION_STR L"--"
#define PREFIX_SHORTOPTION_STR L"-"
#define PREFIX_SHORTOPTION_WINDOWS_STR L"/"
//=============================================================================
Option::Option(const std::wstring& fullOption, const std::wstring& shortOption,
    const std::wstring& description, bool bMultiple, bool bWithFieldValue)
{
    _fullOption = fullOption;
    _shortOption = shortOption;
    _description = description;
    _bMultiple = bMultiple;
    _bWithFieldValue = bWithFieldValue;
}
//=============================================================================
Option::~Option()
{
    _fullOption.clear();
    _shortOption.clear();
    _description.clear();
    _bMultiple = false;
    _bWithFieldValue = false;
}
//=============================================================================
std::wstring
Option::getFullOption()
{
    return _fullOption;
}
//=============================================================================
std::wstring
Option::getShortOption()
{
    return _shortOption;
}
//=============================================================================
std::wstring
Option::getDescription()
{
    return _description;
}
//=============================================================================
std::wstring
Option::getFullDescription()
{
    std::wstring fulldescription;
    if (_shortOption.empty()) {
        fulldescription = PREFIX_FULLOPTION_STR + _fullOption + L": " + _description;
    } else {
        fulldescription = PREFIX_FULLOPTION_STR + _fullOption + L", " + PREFIX_SHORTOPTION_STR
            + _shortOption + L": " + _description;
    }
    return fulldescription;
}
//=============================================================================
bool
Option::isMultiple()
{
    return _bMultiple;
}
//=============================================================================
bool
Option::withFieldValue()
{
    return _bWithFieldValue;
}
//=============================================================================
bool
Option::isSameFieldname(const std::wstring& name)
{
    std::wstring fopt = PREFIX_FULLOPTION_STR + _fullOption;
    std::wstring sopt = PREFIX_SHORTOPTION_STR + _shortOption;
#ifdef _MSC_VER
    std::wstring wsopt = PREFIX_SHORTOPTION_WINDOWS_STR + _shortOption;
    return (name == fopt) || (name == sopt) || (name == wsopt);
#else
    return (name == fopt) || (name == sopt);
#endif
}
//=============================================================================
ProgramOptions::ProgramOptions(wstringVector args, NELSON_ENGINE_MODE mode)
{
    _ishelp = false;
    _isversion = false;
    _startup = false;
    _userstartup = false;
    _usermodules = false;
    _quietmode = false;
    _minimize = false;
    _ipc = false;
    _withoutFileWatcher = false;
    _error.clear();
    _file.clear();
    _command.clear();
    _options.clear();
    _lang.clear();
    _socketioUri.clear();
    _args = std::move(args);
    _mode = mode;
    _isvalid = parse();
}
//=============================================================================
ProgramOptions::~ProgramOptions()
{
    _isvalid = false;
    _ishelp = false;
    _isversion = false;
    _startup = false;
    _userstartup = false;
    _usermodules = false;
    _quietmode = false;
    _ipc = false;
    _withoutFileWatcher = false;
    _minimize = false;
    _error.clear();
    _file.clear();
    _command.clear();
    _options.clear();
    _socketioUri.clear();
    _lang.clear();
}
//=============================================================================
bool
ProgramOptions::parseOptionWithValues(Option& op, wstringVector& values)
{
    if (_args.size() > 2) {
        if (op.isSameFieldname(_args[1])) {
            size_t inc = 1;
            for (size_t k = 2; k < _args.size(); k = k + inc) {
                values.push_back(_args[k]);
            }
        }
    }
    return true;
}
//=============================================================================
bool
ProgramOptions::parseOptionWithValue(Option& op, bool& bFind, std::wstring& value)
{
    if (op.withFieldValue()) {
        size_t nbElements = 0;
        size_t inc = 1;
        for (size_t k = 1; k < _args.size(); k = k + inc) {
            if (op.isSameFieldname(_args[k])) {
                if (k + 1 < _args.size()) {
                    value = _args[k + 1];
                    inc = inc + 1;
                    nbElements++;
                } else {
                    _error = _W("no value after ") + _args[k];
                    return false;
                }
            } else {
                inc = 1;
            }
        }
        if (nbElements == 0) {
            bFind = false;
            return true;
        }
        if (nbElements == 1) {
            bFind = true;
            return true;
        }
        bFind = false;
        _error = _W("multiple option ") + op.getFullOption();
        return false;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::parseOption(Option& op, bool& bFind)
{
    size_t nbElements = 0;
    if (!op.withFieldValue()) {
        size_t inc = 1;
        for (size_t k = 1; k < _args.size(); k = k + inc) {
            if (op.isSameFieldname(_args[k])) {
                nbElements++;
            }
        }
    }
    if (nbElements == 0) {
        bFind = false;
        return true;
    }
    if (nbElements == 1) {
        bFind = true;
        return true;
    }
    bFind = true;
    _error = _W("multiple option ") + op.getFullOption();
    return false;

    return false;
}
//=============================================================================
bool
ProgramOptions::parse()
{
    // nelson --version
    // nelson others_arguments --version others_arguments
    // nelson --help
    // nelson others_arguments --help others_arguments
    // nelson --nostartup others_arguments (except nouserstartup)
    // nelson others_arguments --nostartup others_arguments (except nouserstartup)
    // nelson --nouserstartup others_arguments (except nostartup)
    // nelson others_arguments --nouserstartup others_arguments (except nostartup)
    // nelson --file filename others_arguments (except execute)
    // nelson others_arguments --file filename others_arguments (except execute)
    // nelson --execute command others_arguments (except file)
    // nelson others_arguments --execute command (except file)
    // nelson --language lang others_arguments
    // nelson others_arguments --language lang others_arguments
    // nelson --quiet others_arguments
    // nelson others_arguments --quiet lang others_arguments
    // nelson others_arguments --timeout 10
    // nelson --noipc others_arguments (except noipc)

    bool bRes;
    Option helpOption(L"help", L"h", _W("display this help message"), false, false);
    Option versionOption(L"version", L"v", _W("display the version number"), false, false);
    Option nostartupOption(L"nostartup", L"", _W("no main startup file"), false, false);
    Option nouserstartupOption(L"nouserstartup", L"", _W("no user startup file"), false, false);
    Option nousermodulesOption(L"nousermodules", L"", _W("no user modules loaded"), false, false);
    Option minimizeOption(L"minimize", L"", _W("minimize main window (GUI only)"), false, false);
    Option noIpcOption(L"noipc", L"", _W("no ipc features"), false, false);
    Option withoutFileWatcherOption(
        L"withoutfilewatcher", L"", _W("without file watcher"), false, false);
    Option commandtoexecuteOption(L"execute", L"e", _W("command to execute"), false, true);
    Option filetoexecuteOption(L"file", L"f", _W("file to execute in an new process"), false, true);
    Option filetoexecuteIPCOption(
        L"", L"F", _W("file to execute in an existing process"), false, true);
    Option languageOption(L"language", L"l", _W("language used in current session"), false, true);
    Option socketIoOption(L"socketio", L"", _W("socket.io uri address"), false, true);
    Option quietOption(
        L"quiet", L"q", _W("does not print banner and version at startup"), false, false);
    Option timeoutOption(L"timeout", L"", _W("kill nelson process after n seconds"), false, true);
    Option openFilesOption(L"open", L"o", _W("opens files in text editor"), false, true);
    Option loadFilesOption(L"mat", L"m", _W("load .nh5, .mat files in Nelson"), false, true);
    _options = L"\nUsage:\n";
    _options = _options + helpOption.getFullDescription() + L"\n";
    _options = _options + versionOption.getFullDescription() + L"\n";
    _options = _options + nostartupOption.getFullDescription() + L"\n";
    _options = _options + nouserstartupOption.getFullDescription() + L"\n";
    _options = _options + nousermodulesOption.getFullDescription() + L"\n";
    _options = _options + noIpcOption.getFullDescription() + L"\n";
    if (_mode == NELSON_ENGINE_MODE::GUI) {
        _options = _options + minimizeOption.getFullDescription() + L"\n";
    }
    _options = _options + commandtoexecuteOption.getFullDescription() + L"\n";
    _options = _options + filetoexecuteOption.getFullDescription() + L"\n";
    _options = _options + filetoexecuteIPCOption.getFullDescription() + L"\n";
    _options = _options + languageOption.getFullDescription() + L"\n";
    if (_mode == NELSON_ENGINE_MODE::BASIC_SIO_CLIENT) {
        _options = _options + socketIoOption.getFullDescription() + L"\n";
    }
    _options = _options + quietOption.getFullDescription() + L"\n";
    _options = _options + timeoutOption.getFullDescription() + L"\n";
    _options = _options + openFilesOption.getFullDescription() + L"\n";
    _options = _options + loadFilesOption.getFullDescription() + L"\n";
    _options = _options + withoutFileWatcherOption.getFullDescription() + L"\n";

    bRes = parseOption(helpOption, _ishelp);
    bRes = bRes && parseOption(versionOption, _isversion);
    bRes = bRes && parseOption(nostartupOption, _startup);
    bRes = bRes && parseOption(nouserstartupOption, _userstartup);
    bRes = bRes && parseOption(nousermodulesOption, _usermodules);
    if (_mode == NELSON_ENGINE_MODE::GUI) {
        bRes = bRes && parseOption(minimizeOption, _minimize);
    }
    bRes = bRes && parseOption(noIpcOption, _ipc);
    bRes = bRes && parseOption(withoutFileWatcherOption, _withoutFileWatcher);
    bRes = bRes && parseOptionWithValues(openFilesOption, _filesToOpen);
    bRes = bRes && parseOptionWithValues(loadFilesOption, _filesToLoad);
    bool bFind = false;
    bRes = bRes && parseOptionWithValue(commandtoexecuteOption, bFind, _command);
    bRes = bRes && parseOptionWithValue(filetoexecuteOption, bFind, _file);
    bRes = bRes && parseOptionWithValue(filetoexecuteIPCOption, bFind, _fileIPC);
    bRes = bRes && parseOptionWithValue(languageOption, bFind, _lang);
    if (_mode == NELSON_ENGINE_MODE::BASIC_SIO_CLIENT) {
        bRes = bRes && parseOptionWithValue(socketIoOption, bFind, _socketioUri);
        if (!bFind && !_ishelp) {
            _error = _W("socketio address required.");
            return false;
        }
    }
    if (_mode != NELSON_ENGINE_MODE::GUI) {
        if (!_fileIPC.empty()) {
            _error = _W("'-F' option, GUI mode required.");
            return false;
        }
        if (!_filesToOpen.empty()) {
            _error = _W("'open' in editor, GUI mode required.");
            return false;
        }
    }
    std::wstring _timeout_str;
    bRes = bRes && parseOptionWithValue(timeoutOption, bFind, _timeout_str);
    if (bFind) {
        long long ll = 0;
        if (!StringHelpers::str2longlong(_timeout_str, ll)) {
            _error = _W("wrong value for timeout option.");
            return false;
        }
        if (ll > 0) {
            _timeout = static_cast<uint64>(ll);
        } else {
            _error = _W("wrong value for timeout option.");
            return false;
        }
    } else {
        _timeout = static_cast<uint64>(0);
    }
    bRes = bRes && parseOption(quietOption, _quietmode);
    return bRes;
}
//=============================================================================
bool
ProgramOptions::haveOptionsHelp()
{
    if (_isvalid) {
        return _ishelp;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveOptionsMinimize()
{
    if (_isvalid) {
        return _minimize;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveVersion()
{
    if (_isvalid) {
        return _isversion;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveFileToExecute()
{
    if (_isvalid) {
        return !_file.empty();
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveFileToExecuteIPC()
{
    if (_isvalid) {
        return !_fileIPC.empty();
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveCommandToExecute()
{
    if (_isvalid) {
        return !_command.empty();
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveLanguage()
{
    if (_isvalid) {
        return !_lang.empty();
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveSocketIoUri()
{
    if (_isvalid) {
        return !_socketioUri.empty();
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveNoStartup()
{
    if (_isvalid) {
        return _startup;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveNoIpc()
{
    if (_isvalid) {
        return _ipc;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveWithoutFileWatcher()
{
    if (_isvalid) {
        return _withoutFileWatcher;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveNoUserStartup()
{
    if (_isvalid) {
        return _userstartup;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveNoUserModules()
{
    if (_isvalid) {
        return _usermodules;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveQuietMode()
{
    if (_isvalid) {
        return _quietmode;
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveTimeout()
{
    if (_isvalid) {
        return (_timeout > 0);
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveOpenFiles()
{
    if (_isvalid) {
        return (!_filesToOpen.empty());
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::haveLoadFiles()
{
    if (_isvalid) {
        return (!_filesToLoad.empty());
    }
    return false;
}
//=============================================================================
bool
ProgramOptions::isValid()
{
    return _isvalid;
}
//=============================================================================
std::wstring
ProgramOptions::getErrorMessage()
{
    return _error;
}
//=============================================================================
std::wstring
ProgramOptions::getFileToExecute()
{
    return _file;
}
//=============================================================================
std::wstring
ProgramOptions::getFileToExecuteIPC()
{
    return _fileIPC;
}
//=============================================================================
std::wstring
ProgramOptions::getCommandToExecute()
{
    return _command;
}
//=============================================================================
std::wstring
ProgramOptions::getOptionsHelp()
{
    return _options;
}
//=============================================================================
std::wstring
ProgramOptions::getLanguage()
{
    return _lang;
}
//=============================================================================
std::wstring
ProgramOptions::getSocketIoUri()
{
    return _socketioUri;
}
//=============================================================================
uint64
ProgramOptions::getTimeout()
{
    return _timeout;
}
//=============================================================================
wstringVector
ProgramOptions::getFilesToOpen()
{
    return _filesToOpen;
}
//=============================================================================
wstringVector
ProgramOptions::getFilesToLoad()
{
    return _filesToLoad;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
