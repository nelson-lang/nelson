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
#include "ProgramOptions.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
#define PREFIX_FULLOPTION_STR L"--"
#define PREFIX_SHORTOPTION_STR L"-"
#define PREFIX_SHORTOPTION_WINDOWS_STR L"/"
//=============================================================================
Option::Option(std::wstring fullOption, std::wstring shortOption, std::wstring description,
    bool bMultiple, bool bWithFieldValue)
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
    _fullOption = L"";
    _shortOption = L"";
    _description = L"";
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
Option::isSameFieldname(std::wstring name)
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
ProgramOptions::ProgramOptions(wstringVector args)
{
    _ishelp = false;
    _isversion = false;
    _startup = false;
    _userstartup = false;
    _quietmode = false;
    _error = L"";
    _file = L"";
    _command = L"";
    _options = L"";
    _lang = L"";
    _args = args;
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
    _quietmode = false;
    _error = L"";
    _file = L"";
    _command = L"";
    _options = L"";
    _lang = L"";
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
        } else if (nbElements == 1) {
            bFind = true;
            return true;
        } else {
            bFind = false;
            _error = _W("multiple option ") + op.getFullOption();
            return false;
        }
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
    } else if (nbElements == 1) {
        bFind = true;
        return true;
    } else {
        bFind = true;
        _error = _W("multiple option ") + op.getFullOption();
        return false;
    }
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
    bool bRes;
    Option helpOption(L"help", L"h", _W("display this help message"), false, false);
    Option versionOption(L"version", L"v", _W("display the version number"), false, false);
    Option nostartupOption(L"nostartup", L"", _W("no main startup file"), false, false);
    Option nouserstartupOption(L"nouserstartup", L"", _W("no user startup file"), false, false);
    Option commandtoexecuteOption(L"execute", L"e", _W("command to execute"), false, true);
    Option filetoexecuteOption(L"file", L"f", _W("file to execute"), false, true);
    Option languageOption(L"language", L"l", _W("language used in current session"), false, true);
    Option quietOption(
        L"quiet", L"q", _W("does not print banner and version at startup"), false, false);
    Option timeoutOption(L"timeout", L"", _W("kill nelson process after n seconds"), false, true);
    Option openFilesOption(L"open", L"o", _W("opens files in text editor"), false, true);
    _options = L"\nUsage:\n";
    _options = _options + helpOption.getFullDescription() + L"\n";
    _options = _options + versionOption.getFullDescription() + L"\n";
    _options = _options + nostartupOption.getFullDescription() + L"\n";
    _options = _options + nouserstartupOption.getFullDescription() + L"\n";
    _options = _options + commandtoexecuteOption.getFullDescription() + L"\n";
    _options = _options + filetoexecuteOption.getFullDescription() + L"\n";
    _options = _options + languageOption.getFullDescription() + L"\n";
    _options = _options + quietOption.getFullDescription() + L"\n";
    _options = _options + timeoutOption.getFullDescription() + L"\n";
    _options = _options + openFilesOption.getFullDescription() + L"\n";
    bRes = parseOption(helpOption, _ishelp);
    bRes = bRes && parseOption(versionOption, _isversion);
    bRes = bRes && parseOption(nostartupOption, _startup);
    bRes = bRes && parseOption(nouserstartupOption, _userstartup);
    bRes = bRes && parseOptionWithValues(openFilesOption, _filesToOpen);
    bool bFind = false;
    bRes = bRes && parseOptionWithValue(commandtoexecuteOption, bFind, _command);
    bRes = bRes && parseOptionWithValue(filetoexecuteOption, bFind, _file);
    bRes = bRes && parseOptionWithValue(languageOption, bFind, _lang);
    std::wstring _timeout_str = L"";
    bRes = bRes && parseOptionWithValue(timeoutOption, bFind, _timeout_str);
    if (bFind) {
        try {
            long long ll = boost::lexical_cast<long long>(_timeout_str);
            if (ll > 0) {
                _timeout = (uint64)ll;
            } else {
                _error = _W("wrong value for timeout option.");
                return false;
            }
        } catch (const boost::bad_lexical_cast&) {
            _error = _W("wrong value for timeout option.");
            return false;
        }
    } else {
        _timeout = (uint64)0;
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
ProgramOptions::haveNoStartup()
{
    if (_isvalid) {
        return _startup;
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
        return (_filesToOpen.size() > 0);
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
}
//=============================================================================
