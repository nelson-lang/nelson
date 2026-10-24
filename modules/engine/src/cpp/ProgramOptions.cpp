//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <array>
#include <string>
#include <vector>
#include "CLI11.hpp"
#include "StringHelpers.hpp"
#include "ProgramOptions.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class ProgramOptionId
{
    Help,
    Version,
    NoStartup,
    NoUserStartup,
    NoUserModules,
    NoIpc,
    Minimize,
    Execute,
    File,
    FileIpc,
    Language,
    Quiet,
    Timeout,
    Open,
    Mat,
    AssociatedFiles,
    WebUrl,
    WebPort,
    WebServer,
    WebGraphics,
    WithoutFileWatcher
};
//=============================================================================
enum class ProgramOptionMode
{
    All,
    Gui,
    Web,
    AdvTerminal
};
//=============================================================================
struct ProgramOptionDefinition
{
    ProgramOptionId id;
    const wchar_t* fullOption;
    const wchar_t* shortOption;
    ProgramOptionMode mode;
    bool withValue;
    bool withMultipleValues;
};
//=============================================================================
enum class ProgramOptionErrorId
{
    ModeSelector,
    GuiModeRequired,
    WebModeRequired,
    InvalidWebPort,
    InvalidTimeout,
    WrongNumberOfValues,
    InvalidValue,
    MissingRequiredOption,
    UnexpectedArgument,
    UnknownOption,
    InvalidArguments,
    DuplicateOption,
    UnexpectedValue,
    MissingValue
};
//=============================================================================
template <typename... Definitions>
static constexpr auto
makeProgramOptionDefinitions(Definitions... definitions)
{
    return std::array<ProgramOptionDefinition, sizeof...(Definitions)> { { definitions... } };
}
//=============================================================================
struct ParsedProgramOptions
{
    int helpCount = 0;
    int versionCount = 0;
    int nostartupCount = 0;
    int nouserstartupCount = 0;
    int nousermodulesCount = 0;
    int noipcCount = 0;
    int withoutFileWatcherCount = 0;
    int webServerCount = 0;
    int webGraphicsCount = 0;
    int quietCount = 0;
    int minimizeCount = 0;
    std::string command;
    std::string file;
    std::string fileIPC;
    std::string language;
    std::string timeoutValue;
    std::string webUrl;
    std::string webPortValue;
    std::vector<std::string> filesToOpen;
    std::vector<std::string> filesToLoad;
    std::vector<std::string> associatedFiles;
};
//=============================================================================
static const auto&
programOptionDefinitions()
{
    static const auto definitions
        = makeProgramOptionDefinitions(ProgramOptionDefinition { ProgramOptionId::Help, L"help",
                                           L"h", ProgramOptionMode::All, false, false },
            ProgramOptionDefinition {
                ProgramOptionId::Version, L"version", L"v", ProgramOptionMode::All, false, false },
            ProgramOptionDefinition { ProgramOptionId::NoStartup, L"no-startup", L"",
                ProgramOptionMode::All, false, false },
            ProgramOptionDefinition { ProgramOptionId::NoUserStartup, L"no-user-startup", L"",
                ProgramOptionMode::All, false, false },
            ProgramOptionDefinition { ProgramOptionId::NoUserModules, L"no-user-modules", L"",
                ProgramOptionMode::All, false, false },
            ProgramOptionDefinition {
                ProgramOptionId::NoIpc, L"no-ipc", L"", ProgramOptionMode::All, false, false },
            ProgramOptionDefinition {
                ProgramOptionId::Minimize, L"minimize", L"", ProgramOptionMode::Gui, false, false },
            ProgramOptionDefinition {
                ProgramOptionId::Execute, L"execute", L"e", ProgramOptionMode::All, true, false },
            ProgramOptionDefinition {
                ProgramOptionId::File, L"file", L"f", ProgramOptionMode::All, true, false },
            ProgramOptionDefinition {
                ProgramOptionId::FileIpc, L"file-ipc", L"F", ProgramOptionMode::All, true, false },
            ProgramOptionDefinition {
                ProgramOptionId::Language, L"language", L"l", ProgramOptionMode::All, true, false },
            ProgramOptionDefinition {
                ProgramOptionId::Quiet, L"quiet", L"q", ProgramOptionMode::All, false, false },
            ProgramOptionDefinition {
                ProgramOptionId::Timeout, L"timeout", L"", ProgramOptionMode::All, true, false },
            ProgramOptionDefinition {
                ProgramOptionId::Open, L"open", L"o", ProgramOptionMode::All, true, true },
            ProgramOptionDefinition {
                ProgramOptionId::Mat, L"mat", L"m", ProgramOptionMode::All, true, true },
            ProgramOptionDefinition { ProgramOptionId::AssociatedFiles, L"associated-files", L"",
                ProgramOptionMode::All, true, true },
            ProgramOptionDefinition {
                ProgramOptionId::WebUrl, L"url", L"", ProgramOptionMode::Web, true, false },
            ProgramOptionDefinition {
                ProgramOptionId::WebPort, L"port", L"", ProgramOptionMode::Web, true, false },
            ProgramOptionDefinition {
                ProgramOptionId::WebServer, L"web", L"", ProgramOptionMode::Web, false, false },
            ProgramOptionDefinition { ProgramOptionId::WebGraphics, L"webview", L"",
                ProgramOptionMode::AdvTerminal, false, false },
            ProgramOptionDefinition { ProgramOptionId::WithoutFileWatcher, L"without-file-watcher",
                L"", ProgramOptionMode::All, false, false });
    return definitions;
}
//=============================================================================
static std::wstring
commandLineError(ProgramOptionErrorId id, const std::wstring& message);
//=============================================================================
static const wchar_t*
programOptionErrorIdentifier(ProgramOptionErrorId id);
//=============================================================================
static void
addOptionToParser(
    CLI::App& app, const ProgramOptionDefinition& definition, ParsedProgramOptions& parsed);
//=============================================================================
static bool
isOptionAvailable(const ProgramOptionDefinition& definition, NELSON_ENGINE_MODE mode);
//=============================================================================
static std::wstring
optionDescription(ProgramOptionId id);
//=============================================================================
static std::wstring
optionNamesForCli11(const ProgramOptionDefinition& definition);
//=============================================================================
static bool
isModeSelector(const std::wstring& arg, NELSON_ENGINE_MODE mode);
//=============================================================================
static void
appendNormalizedShortOption(
    std::vector<std::string>& utf8Args, const std::wstring& arg, NELSON_ENGINE_MODE mode);
//=============================================================================
static std::vector<std::string>
toUtf8Arguments(const wstringVector& args, NELSON_ENGINE_MODE mode);
//=============================================================================
static std::wstring
toWideError(const CLI::Error& error);
//=============================================================================
static bool
validateNelsonOptions(const wstringVector& args, NELSON_ENGINE_MODE mode, std::wstring& error);
//=============================================================================
static bool
validateOptionValue(const wstringVector& args, NELSON_ENGINE_MODE mode,
    const ProgramOptionDefinition& definition, size_t& index, std::wstring& error);
//=============================================================================
static bool
optionHasInlineValue(const std::wstring& arg);
//=============================================================================
static bool
optionHasEmptyInlineValue(const std::wstring& arg);
//=============================================================================
static std::wstring
optionNameWithoutInlineValue(const std::wstring& arg);
//=============================================================================
static const ProgramOptionDefinition*
findOptionDefinition(const std::wstring& arg, NELSON_ENGINE_MODE mode);
//=============================================================================
static bool
isOptionAlias(const std::wstring& arg, const ProgramOptionDefinition& definition);
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
    if (_fullOption.empty()) {
        fulldescription = L"-" + _shortOption + L": " + _description;
    } else if (_shortOption.empty()) {
        fulldescription = L"--" + _fullOption + L": " + _description;
    } else {
        fulldescription = L"--" + _fullOption + L", -" + _shortOption + L": " + _description;
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
    std::wstring fopt = L"--" + _fullOption;
    if (!_fullOption.empty() && name == fopt) {
        return true;
    }
    if (_shortOption.empty()) {
        return false;
    }
    std::wstring sopt = L"-" + _shortOption;
#ifdef _MSC_VER
    std::wstring wsopt = L"/" + _shortOption;
    return (name == sopt) || (name == wsopt);
#else
    return name == sopt;
#endif
}
//=============================================================================
ProgramOptions::ProgramOptions(wstringVector args, NELSON_ENGINE_MODE mode)
{
    _args = std::move(args);
    _mode = mode;
    resetParsedState();
    _isvalid = parse();
}
//=============================================================================
ProgramOptions::~ProgramOptions()
{
    resetParsedState();
    _args.clear();
}
//=============================================================================
void
ProgramOptions::resetParsedState()
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
    _webServerMode = false;
    _webGraphics = false;
    _error.clear();
    _file.clear();
    _fileIPC.clear();
    _command.clear();
    _options.clear();
    _webUrl.clear();
    _webPort = 0;
    _lang.clear();
    _filesToOpen.clear();
    _filesToLoad.clear();
    _associatedFiles.clear();
    _userArguments.clear();
}
//=============================================================================
bool
ProgramOptions::parse()
{
    resetParsedState();
    _options = L"\nUsage:\n";
    for (const auto& definition : programOptionDefinitions()) {
        if (isOptionAvailable(definition, _mode)) {
            Option option(definition.fullOption, definition.shortOption,
                optionDescription(definition.id), definition.withMultipleValues,
                definition.withValue);
            _options = _options + option.getFullDescription() + L"\n";
        }
    }

    wstringVector nelsonArguments;
    for (size_t k = 1; k < _args.size(); ++k) {
        if (_args[k] == L"--") {
            for (size_t j = k + 1; j < _args.size(); ++j) {
                _userArguments.push_back(_args[j]);
            }
            break;
        }
        if (isModeSelector(_args[k], _mode)) {
            _error = commandLineError(ProgramOptionErrorId::ModeSelector,
                Nelson::resolveMessageTemplate(L"Nelson:engine:modeSelectorOptionOnly",
                    L"mode selector option is only valid for the nelson launcher."));
            return false;
        }
        const std::wstring optionName = optionNameWithoutInlineValue(_args[k]);
        if (_mode != NELSON_ENGINE_MODE::WEB_GUI
            && (optionName == L"--url" || optionName == L"--port")) {
            _error = commandLineError(ProgramOptionErrorId::WebModeRequired,
                Nelson::resolveMessageTemplate(L"Nelson:engine:urlPortOptionsRequire",
                    L"'--url' and '--port' options require web mode."));
            return false;
        }
        nelsonArguments.push_back(_args[k]);
    }
    if (!validateNelsonOptions(nelsonArguments, _mode, _error)) {
        return false;
    }

    CLI::App app { "Nelson options" };
    app.allow_extras();
    app.set_help_flag();

    ParsedProgramOptions parsed;
    for (const auto& definition : programOptionDefinitions()) {
        if (!isOptionAvailable(definition, _mode)) {
            continue;
        }
        addOptionToParser(app, definition, parsed);
    }

    std::vector<std::string> argsToParse = toUtf8Arguments(nelsonArguments, _mode);
    std::reverse(argsToParse.begin(), argsToParse.end());
    try {
        app.parse(argsToParse);
    } catch (const CLI::Error& e) {
        _error = toWideError(e);
        return false;
    }

    _ishelp = parsed.helpCount > 0;
    _isversion = parsed.versionCount > 0;
    _startup = parsed.nostartupCount > 0;
    _userstartup = parsed.nouserstartupCount > 0;
    _usermodules = parsed.nousermodulesCount > 0;
    _ipc = parsed.noipcCount > 0;
    _withoutFileWatcher = parsed.withoutFileWatcherCount > 0;
    _webServerMode = parsed.webServerCount > 0;
    _webGraphics = parsed.webGraphicsCount > 0;
    _quietmode = parsed.quietCount > 0;
    _minimize = parsed.minimizeCount > 0;
    _command = utf8_to_wstring(parsed.command);
    _file = utf8_to_wstring(parsed.file);
    _fileIPC = utf8_to_wstring(parsed.fileIPC);
    _lang = utf8_to_wstring(parsed.language);
    _webUrl = utf8_to_wstring(parsed.webUrl);
    _filesToOpen = utf8_to_wstring(parsed.filesToOpen);
    _filesToLoad = utf8_to_wstring(parsed.filesToLoad);
    _associatedFiles = utf8_to_wstring(parsed.associatedFiles);

    if (_mode != NELSON_ENGINE_MODE::GUI) {
        if (!_fileIPC.empty()) {
            _error = commandLineError(ProgramOptionErrorId::GuiModeRequired,
                Nelson::resolveMessageTemplate(
                    L"Nelson:engine:fOptionGuiMode", L"'-F' option, GUI mode required."));
            return false;
        }
        if (!_filesToOpen.empty()) {
            _error = commandLineError(ProgramOptionErrorId::GuiModeRequired,
                Nelson::resolveMessageTemplate(
                    L"Nelson:engine:openEditorGuiMode", L"'open' in editor, GUI mode required."));
            return false;
        }
        if (!_associatedFiles.empty()) {
            _error = commandLineError(ProgramOptionErrorId::GuiModeRequired,
                resolveMessageTemplate(L"Nelson:engine:openingAssociatedFilesRequires",
                    L"opening associated files requires GUI mode."));
            return false;
        }
    }
    if (!parsed.timeoutValue.empty()) {
        long long ll = 0;
        if (!StringHelpers::str2longlong(utf8_to_wstring(parsed.timeoutValue), ll)) {
            _error = commandLineError(ProgramOptionErrorId::InvalidTimeout,
                Nelson::resolveMessageTemplate(
                    L"Nelson:engine:wrongTimeoutOption", L"wrong value for timeout option."));
            return false;
        }
        if (ll > 0) {
            _timeout = static_cast<uint64>(ll);
        } else {
            _error = commandLineError(ProgramOptionErrorId::InvalidTimeout,
                Nelson::resolveMessageTemplate(
                    L"Nelson:engine:wrongTimeoutOption", L"wrong value for timeout option."));
            return false;
        }
    } else {
        _timeout = static_cast<uint64>(0);
    }
    if (!parsed.webPortValue.empty()) {
        long long port = 0;
        if (!StringHelpers::str2longlong(utf8_to_wstring(parsed.webPortValue), port) || port < 1
            || port > 65535) {
            _error = commandLineError(ProgramOptionErrorId::InvalidWebPort,
                resolveMessageTemplate(
                    L"Nelson:engine:webPortBetween", L"web port must be between 1 and 65535."));
            return false;
        }
        _webPort = static_cast<uint64>(port);
    }
    return true;
}
//=============================================================================
bool
ProgramOptions::haveOptionsHelp()
{
    return _isvalid && _ishelp;
}
//=============================================================================
bool
ProgramOptions::haveOptionsMinimize()
{
    return _isvalid && _minimize;
}
//=============================================================================
bool
ProgramOptions::haveVersion()
{
    return _isvalid && _isversion;
}
//=============================================================================
bool
ProgramOptions::haveFileToExecute()
{
    return _isvalid && !_file.empty();
}
//=============================================================================
bool
ProgramOptions::haveFileToExecuteIPC()
{
    return _isvalid && !_fileIPC.empty();
}
//=============================================================================
bool
ProgramOptions::haveCommandToExecute()
{
    return _isvalid && !_command.empty();
}
//=============================================================================
bool
ProgramOptions::haveLanguage()
{
    return _isvalid && !_lang.empty();
}
//=============================================================================
bool
ProgramOptions::haveNoStartup()
{
    return _isvalid && _startup;
}
//=============================================================================
bool
ProgramOptions::haveNoIpc()
{
    return _isvalid && _ipc;
}
//=============================================================================
bool
ProgramOptions::haveWithoutFileWatcher()
{
    return _isvalid && _withoutFileWatcher;
}
//=============================================================================
bool
ProgramOptions::haveNoUserStartup()
{
    return _isvalid && _userstartup;
}
//=============================================================================
bool
ProgramOptions::haveNoUserModules()
{
    return _isvalid && _usermodules;
}
//=============================================================================
bool
ProgramOptions::haveQuietMode()
{
    return _isvalid && _quietmode;
}
//=============================================================================
bool
ProgramOptions::haveTimeout()
{
    return _isvalid && (_timeout > 0);
}
//=============================================================================
bool
ProgramOptions::haveOpenFiles()
{
    return _isvalid && !_filesToOpen.empty();
}
//=============================================================================
bool
ProgramOptions::haveLoadFiles()
{
    return _isvalid && !_filesToLoad.empty();
}
//=============================================================================
bool
ProgramOptions::haveAssociatedFiles()
{
    return _isvalid && !_associatedFiles.empty();
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
ProgramOptions::getWebUrl()
{
    return _webUrl;
}
//=============================================================================
uint64
ProgramOptions::getWebPort()
{
    return _webPort;
}
//=============================================================================
bool
ProgramOptions::haveWebUrl()
{
    return _isvalid && !_webUrl.empty();
}
//=============================================================================
bool
ProgramOptions::haveWebPort()
{
    return _isvalid && _webPort != 0;
}
//=============================================================================
bool
ProgramOptions::haveWebServerMode()
{
    return _isvalid && _webServerMode;
}
//=============================================================================
bool
ProgramOptions::haveWebGraphics()
{
    return _isvalid && _webGraphics;
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
wstringVector
ProgramOptions::getAssociatedFiles()
{
    return _associatedFiles;
}
//=============================================================================
wstringVector
ProgramOptions::getUserArguments()
{
    return _userArguments;
}
//=============================================================================
static std::wstring
commandLineError(ProgramOptionErrorId id, const std::wstring& message)
{
    return L"[" + std::wstring(programOptionErrorIdentifier(id)) + L"] " + message;
}
//=============================================================================
static const wchar_t*
programOptionErrorIdentifier(ProgramOptionErrorId id)
{
    switch (id) {
    case ProgramOptionErrorId::ModeSelector:
        return L"Nelson:ProgramOptions:ModeSelector";
    case ProgramOptionErrorId::GuiModeRequired:
        return L"Nelson:ProgramOptions:GuiModeRequired";
    case ProgramOptionErrorId::WebModeRequired:
        return L"Nelson:ProgramOptions:WebModeRequired";
    case ProgramOptionErrorId::InvalidWebPort:
        return L"Nelson:ProgramOptions:InvalidWebPort";
    case ProgramOptionErrorId::InvalidTimeout:
        return L"Nelson:ProgramOptions:InvalidTimeout";
    case ProgramOptionErrorId::WrongNumberOfValues:
        return L"Nelson:ProgramOptions:WrongNumberOfValues";
    case ProgramOptionErrorId::InvalidValue:
        return L"Nelson:ProgramOptions:InvalidValue";
    case ProgramOptionErrorId::MissingRequiredOption:
        return L"Nelson:ProgramOptions:MissingRequiredOption";
    case ProgramOptionErrorId::UnexpectedArgument:
        return L"Nelson:ProgramOptions:UnexpectedArgument";
    case ProgramOptionErrorId::UnknownOption:
        return L"Nelson:ProgramOptions:UnknownOption";
    case ProgramOptionErrorId::InvalidArguments:
        return L"Nelson:ProgramOptions:InvalidArguments";
    case ProgramOptionErrorId::DuplicateOption:
        return L"Nelson:ProgramOptions:DuplicateOption";
    case ProgramOptionErrorId::UnexpectedValue:
        return L"Nelson:ProgramOptions:UnexpectedValue";
    case ProgramOptionErrorId::MissingValue:
        return L"Nelson:ProgramOptions:MissingValue";
    }
    return L"Nelson:ProgramOptions:InvalidArguments";
}
//=============================================================================
static void
addOptionToParser(
    CLI::App& app, const ProgramOptionDefinition& definition, ParsedProgramOptions& parsed)
{
    std::string optionNames = wstring_to_utf8(optionNamesForCli11(definition));
    std::string description = wstring_to_utf8(optionDescription(definition.id));
    switch (definition.id) {
    case ProgramOptionId::Help:
        app.add_flag(optionNames, parsed.helpCount, description);
        break;
    case ProgramOptionId::Version:
        app.add_flag(optionNames, parsed.versionCount, description);
        break;
    case ProgramOptionId::NoStartup:
        app.add_flag(optionNames, parsed.nostartupCount, description);
        break;
    case ProgramOptionId::NoUserStartup:
        app.add_flag(optionNames, parsed.nouserstartupCount, description);
        break;
    case ProgramOptionId::NoUserModules:
        app.add_flag(optionNames, parsed.nousermodulesCount, description);
        break;
    case ProgramOptionId::NoIpc:
        app.add_flag(optionNames, parsed.noipcCount, description);
        break;
    case ProgramOptionId::Minimize:
        app.add_flag(optionNames, parsed.minimizeCount, description);
        break;
    case ProgramOptionId::Execute:
        app.add_option(optionNames, parsed.command, description);
        break;
    case ProgramOptionId::File:
        app.add_option(optionNames, parsed.file, description);
        break;
    case ProgramOptionId::FileIpc:
        app.add_option(optionNames, parsed.fileIPC, description);
        break;
    case ProgramOptionId::Language:
        app.add_option(optionNames, parsed.language, description);
        break;
    case ProgramOptionId::Quiet:
        app.add_flag(optionNames, parsed.quietCount, description);
        break;
    case ProgramOptionId::Timeout:
        app.add_option(optionNames, parsed.timeoutValue, description);
        break;
    case ProgramOptionId::Open:
        app.add_option(optionNames, parsed.filesToOpen, description)->expected(1, -1);
        break;
    case ProgramOptionId::Mat:
        app.add_option(optionNames, parsed.filesToLoad, description)->expected(1, -1);
        break;
    case ProgramOptionId::AssociatedFiles:
        app.add_option(optionNames, parsed.associatedFiles, description)->expected(1, -1);
        break;
    case ProgramOptionId::WebUrl:
        app.add_option(optionNames, parsed.webUrl, description);
        break;
    case ProgramOptionId::WebPort:
        app.add_option(optionNames, parsed.webPortValue, description);
        break;
    case ProgramOptionId::WebServer:
        app.add_flag(optionNames, parsed.webServerCount, description);
        break;
    case ProgramOptionId::WebGraphics:
        app.add_flag(optionNames, parsed.webGraphicsCount, description);
        break;
    case ProgramOptionId::WithoutFileWatcher:
        app.add_flag(optionNames, parsed.withoutFileWatcherCount, description);
        break;
    }
}
//=============================================================================
static bool
isModeSelector(const std::wstring& arg, NELSON_ENGINE_MODE mode)
{
    if (arg == L"--webview") {
        // On the advanced terminal `--webview` is the web graphics option, not a
        // launcher mode selector.
        return mode != NELSON_ENGINE_MODE::ADVANCED_TERMINAL;
    }
    if (arg == L"--web") {
        return mode != NELSON_ENGINE_MODE::WEB_GUI;
    }
    return arg == L"--cli" || arg == L"--adv-cli" || arg == L"--gui";
}
//=============================================================================
// CLI11 keeps the '=' of a short option in its value ("-e=quit" runs "=quit"):
// "-e=value", and the Windows forms "/e" and "/e=value", become "-e" "value".
static void
appendNormalizedShortOption(
    std::vector<std::string>& utf8Args, const std::wstring& arg, NELSON_ENGINE_MODE mode)
{
    for (const auto& definition : programOptionDefinitions()) {
        if (!isOptionAvailable(definition, mode) || definition.shortOption[0] == L'\0') {
            continue;
        }
        std::wstring shortOption = definition.shortOption;
        bool inlineValue = arg.rfind(L"-" + shortOption + L"=", 0) == 0;
#ifdef _MSC_VER
        if (arg == L"/" + shortOption) {
            utf8Args.push_back(wstring_to_utf8(L"-" + shortOption));
            return;
        }
        inlineValue = inlineValue || arg.rfind(L"/" + shortOption + L"=", 0) == 0;
#endif
        if (inlineValue) {
            utf8Args.push_back(wstring_to_utf8(L"-" + shortOption));
            utf8Args.push_back(wstring_to_utf8(arg.substr(shortOption.size() + 2)));
            return;
        }
    }
    utf8Args.push_back(wstring_to_utf8(arg));
}
//=============================================================================
static std::vector<std::string>
toUtf8Arguments(const wstringVector& args, NELSON_ENGINE_MODE mode)
{
    std::vector<std::string> utf8Args;
    utf8Args.reserve(args.size());
    for (const auto& arg : args) {
        appendNormalizedShortOption(utf8Args, arg, mode);
    }
    return utf8Args;
}
//=============================================================================
static std::wstring
toWideError(const CLI::Error& error)
{
    const std::string errorName = error.get_name();
    if (errorName == "ArgumentMismatch") {
        return commandLineError(ProgramOptionErrorId::WrongNumberOfValues,
            Nelson::resolveMessageTemplate(L"Nelson:engine:wrongNumberCommandLine",
                L"wrong number of command line option values."));
    }
    if (errorName == "ConversionError" || errorName == "ValidationError") {
        return commandLineError(ProgramOptionErrorId::InvalidValue,
            Nelson::resolveMessageTemplate(
                L"Nelson:engine:wrongCommandLineOption", L"wrong command line option value."));
    }
    if (errorName == "RequiredError") {
        return commandLineError(ProgramOptionErrorId::MissingRequiredOption,
            Nelson::resolveMessageTemplate(L"Nelson:engine:missingRequiredCommandLine",
                L"missing required command line option."));
    }
    if (errorName == "ExtrasError") {
        return commandLineError(ProgramOptionErrorId::UnexpectedArgument,
            Nelson::resolveMessageTemplate(
                L"Nelson:engine:unexpectedCommandLine", L"unexpected command line argument."));
    }
    if (errorName == "OptionNotFound") {
        return commandLineError(ProgramOptionErrorId::UnknownOption,
            resolveMessageTemplate(
                L"Nelson:engine:unknownCommandLineOption", L"unknown command line option."));
    }
    return commandLineError(ProgramOptionErrorId::InvalidArguments,
        resolveMessageTemplate(
            L"Nelson:engine:invalidCommandLine", L"invalid command line arguments."));
}
//=============================================================================
static bool
isOptionAvailable(const ProgramOptionDefinition& definition, NELSON_ENGINE_MODE mode)
{
    switch (definition.mode) {
    case ProgramOptionMode::All:
        return true;
    case ProgramOptionMode::Gui:
        return mode == NELSON_ENGINE_MODE::GUI;
    case ProgramOptionMode::Web:
        return mode == NELSON_ENGINE_MODE::WEB_GUI;
    case ProgramOptionMode::AdvTerminal:
        return mode == NELSON_ENGINE_MODE::ADVANCED_TERMINAL;
    }
    return false;
}
//=============================================================================
static std::wstring
optionDescription(ProgramOptionId id)
{
    switch (id) {
    case ProgramOptionId::Help:
        return _W("display this help message");
    case ProgramOptionId::Version:
        return _W("display the version number");
    case ProgramOptionId::NoStartup:
        return _W("no main startup file");
    case ProgramOptionId::NoUserStartup:
        return _W("no user startup file");
    case ProgramOptionId::NoUserModules:
        return _W("no user modules loaded");
    case ProgramOptionId::NoIpc:
        return _W("no ipc features");
    case ProgramOptionId::Minimize:
        return _W("minimize main window (GUI only)");
    case ProgramOptionId::Execute:
        return _W("command to execute");
    case ProgramOptionId::File:
        return _W("file to execute in an new process");
    case ProgramOptionId::FileIpc:
        return _W("file to execute in an existing process");
    case ProgramOptionId::Language:
        return _W("language used in current session");
    case ProgramOptionId::Quiet:
        return _W("does not print banner and version at startup");
    case ProgramOptionId::Timeout:
        return _W("kill nelson process after n seconds");
    case ProgramOptionId::Open:
        return _W("opens files in text editor");
    case ProgramOptionId::Mat:
        return _W("load .nh5, .mat files in Nelson");
    case ProgramOptionId::AssociatedFiles:
        return _W("open associated files according to their type (GUI only)");
    case ProgramOptionId::WebUrl:
        return _W("web server host or HTTP URL");
    case ProgramOptionId::WebPort:
        return _W("web server port (1..65535)");
    case ProgramOptionId::WebServer:
        return _W("start in web server mode");
    case ProgramOptionId::WebGraphics:
        return _W("render figures with the web backend (no Qt figure windows)");
    case ProgramOptionId::WithoutFileWatcher:
        return _W("without file watcher");
    }
    return L"";
}
//=============================================================================
static std::wstring
optionNamesForCli11(const ProgramOptionDefinition& definition)
{
    std::wstring names = L"--" + std::wstring(definition.fullOption);
    if (definition.shortOption[0] != L'\0') {
        names = L"-" + std::wstring(definition.shortOption) + L"," + names;
    }
    return names;
}
//=============================================================================
static bool
validateNelsonOptions(const wstringVector& args, NELSON_ENGINE_MODE mode, std::wstring& error)
{
    wstringVector seenOptions;
    for (size_t k = 0; k < args.size(); ++k) {
        const ProgramOptionDefinition* definition = findOptionDefinition(args[k], mode);
        if (definition == nullptr) {
            continue;
        }
        std::wstring optionName = definition->fullOption;
        if (std::find(seenOptions.begin(), seenOptions.end(), optionName) != seenOptions.end()) {
            error = commandLineError(ProgramOptionErrorId::DuplicateOption,
                formatI18nMessage(Nelson::resolveMessageTemplate(
                                      L"Nelson:engine:multipleOption", L"multiple option %s"),
                    optionName));
            return false;
        }
        seenOptions.push_back(optionName);
        if (!validateOptionValue(args, mode, *definition, k, error)) {
            return false;
        }
    }
    error.clear();
    return true;
}
//=============================================================================
static bool
validateOptionValue(const wstringVector& args, NELSON_ENGINE_MODE mode,
    const ProgramOptionDefinition& definition, size_t& index, std::wstring& error)
{
    const std::wstring& arg = args[index];
    if (!definition.withValue) {
        if (optionHasInlineValue(arg)) {
            error = commandLineError(ProgramOptionErrorId::UnexpectedValue,
                formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:engine:optionDoesTake",
                                      L"option does not take a value %s"),
                    optionNameWithoutInlineValue(arg)));
            return false;
        }
        return true;
    }
    if (optionHasEmptyInlineValue(arg)) {
        error = commandLineError(ProgramOptionErrorId::MissingValue,
            formatI18nMessage(
                Nelson::resolveMessageTemplate(L"Nelson:engine:noValueAfter", L"no value after %s"),
                optionNameWithoutInlineValue(arg)));
        return false;
    }
    if (optionHasInlineValue(arg)) {
        return true;
    }
    if (index + 1 >= args.size() || findOptionDefinition(args[index + 1], mode) != nullptr) {
        error = commandLineError(ProgramOptionErrorId::MissingValue,
            formatI18nMessage(
                Nelson::resolveMessageTemplate(L"Nelson:engine:noValueAfter", L"no value after %s"),
                arg));
        return false;
    }
    if (definition.withMultipleValues) {
        while (index + 1 < args.size() && findOptionDefinition(args[index + 1], mode) == nullptr) {
            ++index;
        }
    } else {
        ++index;
    }
    return true;
}
//=============================================================================
static bool
optionHasInlineValue(const std::wstring& arg)
{
    return arg.find(L'=') != std::wstring::npos;
}
//=============================================================================
static bool
optionHasEmptyInlineValue(const std::wstring& arg)
{
    return arg.empty() == false && arg.back() == L'=' && optionHasInlineValue(arg);
}
//=============================================================================
static std::wstring
optionNameWithoutInlineValue(const std::wstring& arg)
{
    size_t separatorPosition = arg.find(L'=');
    if (separatorPosition == std::wstring::npos) {
        return arg;
    }
    return arg.substr(0, separatorPosition);
}
//=============================================================================
static const ProgramOptionDefinition*
findOptionDefinition(const std::wstring& arg, NELSON_ENGINE_MODE mode)
{
    for (const auto& definition : programOptionDefinitions()) {
        if (isOptionAvailable(definition, mode) && isOptionAlias(arg, definition)) {
            return &definition;
        }
    }
    return nullptr;
}
//=============================================================================
static bool
isOptionAlias(const std::wstring& arg, const ProgramOptionDefinition& definition)
{
    std::wstring fullOption = definition.fullOption;
    if (arg == L"--" + fullOption || arg.rfind(L"--" + fullOption + L"=", 0) == 0) {
        return true;
    }
    if (definition.shortOption[0] == L'\0') {
        return false;
    }
    std::wstring shortOption = definition.shortOption;
    if (arg == L"-" + shortOption || arg.rfind(L"-" + shortOption + L"=", 0) == 0) {
        return true;
    }
#ifdef _MSC_VER
    return arg == L"/" + shortOption || arg.rfind(L"/" + shortOption + L"=", 0) == 0;
#else
    return false;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
