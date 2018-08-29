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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include "AddGateway.hpp"
#include "EvaluateCommand.hpp"
#include "EvaluateScriptFile.hpp"
#include "Evaluator.hpp"
#include "FinishNelsonMainScript.hpp"
#include "FinishNelsonUserScript.hpp"
#include "GetNelsonPath.hpp"
#include "Localization.hpp"
#include "MainEvaluator.hpp"
#include "MaxOpenedFiles.hpp"
#include "ModulesHelpers.hpp"
#include "NelsonNamedMutex.hpp"
#include "Nelson_VERSION.h"
#include "OpenFilesAssociated.hpp"
#include "ProgramOptions.hpp"
#include "RecursionStack.hpp"
#include "SetNelSonEnvironmentVariables.hpp"
#include "StartNelson.h"
#include "StartNelsonMainScript.hpp"
#include "StartNelsonUserScript.hpp"
#include "TimeoutThread.hpp"
#include "characters_encoding.hpp"
#include "WarningIds.hpp"
#include "WarningEmitter.h"
#include "ErrorEmitter.h"
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <locale.h>
#include <sstream>
//=============================================================================
static void
ErrorCommandLineMessage_startup_exclusive(NELSON_ENGINE_MODE _mode)
{
    std::stringstream msg;
    msg << _("'nostartup' and  'startup' are mutual exclusive.\n");
#ifdef _MSC_VER
    if (_mode == GUI) {
        MessageBox(
            NULL, utf8_to_wstring(msg.str()).c_str(), L"Nelson version:", MB_ICONINFORMATION);
    } else {
        std::cout << msg.str();
    }
#else
    std::cout << msg.str();
#endif
}
//=============================================================================
static void
ErrorCommandLineMessage_file_commmand(NELSON_ENGINE_MODE _mode)
{
#ifdef _MSC_BUILD
    if (_mode == GUI) {
        MessageBox(NULL, _W("too many arguments -f and -e are exclusive.").c_str(),
            _W("Error").c_str(), MB_ICONERROR);
    } else {
        std::cerr << _("ERROR: too many arguments -f and -e are exclusive.") << std::endl;
    }
#else
    std::cerr << _("ERROR: too many arguments -f and -e are exclusive.") << std::endl;
#endif
}
//=============================================================================
static void
ErrorPathDetection(NELSON_ENGINE_MODE _mode)
{
#ifdef _MSC_BUILD
    if (_mode == GUI) {
        MessageBox(
            NULL, _W("Nelson paths not initialized.").c_str(), _W("Error").c_str(), MB_ICONERROR);
    } else {
        fwprintf(stderr, L"%ls", _W("Nelson paths not initialized.\n").c_str());
    }
#else
    fwprintf(stderr, L"%ls", _W("Nelson paths not initialized.\n").c_str());
#endif
}
//=============================================================================
static void
ErrorInterpreter(NELSON_ENGINE_MODE _mode)
{
#ifdef _MSC_BUILD
    if (_mode == GUI) {
        MessageBox(NULL, _W("Nelson interpreter not initialized.").c_str(), _W("Error").c_str(),
            MB_ICONERROR);
    } else {
        fwprintf(stderr, L"%ls", _W("Nelson interpreter not initialized.\n").c_str());
    }
#else
    fwprintf(stderr, _W("Nelson interpreter not initialized.\n").c_str());
#endif
}

//=============================================================================
static void
displayVersion(NELSON_ENGINE_MODE _mode)
{
    std::stringstream msg;
    msg << NELSON_PRODUCT_NAME << " version: \"" << NELSON_VERSION_STRING << "\"\n";
#ifdef _MSC_VER
    if (_mode == GUI) {
        MessageBox(
            NULL, utf8_to_wstring(msg.str()).c_str(), L"Nelson version:", MB_ICONINFORMATION);
    } else {
        std::cout << msg.str();
    }
#else
    std::cout << msg.str();
#endif
}
//=============================================================================
static void
displayHelp(std::wstring description, NELSON_ENGINE_MODE _mode)
{
#ifdef _MSC_VER
    if (_mode == GUI) {
        MessageBox(NULL, description.c_str(), L"Nelson options:", MB_ICONINFORMATION);
    } else {
        std::wcout << L"Nelson options:\n";
        std::wcout << description;
    }
#else
    std::wcout << L"Nelson options:\n";
    std::wcout << description.c_str();
#endif
}
//=============================================================================
static void
ErrorCommandLine(std::wstring str, NELSON_ENGINE_MODE _mode)
{
#ifdef _MSC_VER
    if (_mode == GUI) {
        MessageBox(NULL, str.c_str(), L"Error:", MB_ICONINFORMATION);
    } else {
        std::wcout << L"Error:\n";
        std::wcout << str;
    }
#else
    std::wcout << L"Error:\n";
    std::wcout << str.c_str();
#endif
}
//=============================================================================
static int
NelsonMainStates(Evaluator* eval, bool haveNoStartup, bool haveNoUserStartup,
    std::wstring commandToExecute, std::wstring fileToExecute, wstringVector filesToOpen)
{
    eval->resetState();
    if (!haveNoStartup) {
        StartNelsonMainScript(eval);
        eval->clearStacks();
        if (eval->getState() == NLS_STATE_QUIT) {
            goto FINISH;
        }
        eval->resetState();
        if (!haveNoUserStartup) {
            StartNelsonUserScript(eval);
            eval->clearStacks();
            if (eval->getState() == NLS_STATE_QUIT) {
                goto FINISH;
            }
            eval->resetState();
        }
    }
    try {
        if (!commandToExecute.empty()) {
            EvaluateCommand(eval, commandToExecute.c_str(), false);
        }
        if (!fileToExecute.empty()) {
            EvaluateScriptFile(eval, fileToExecute.c_str());
        }
    } catch (Exception& e) {
        Interface* io = eval->getInterface();
        io->errorMessage(e.getMessage());
    }
    OpenFilesAssociated(eval, filesToOpen);
    while (eval->getState() != NLS_STATE_QUIT) {
        if (eval->getState() == NLS_STATE_ABORT) {
            eval->clearStacks();
        }
        eval->resetState();
        eval->evalCLI();
    }
    eval->resetState();
FINISH:
    if (!haveNoStartup) {
        if (!haveNoUserStartup) {
            FinishNelsonUserScript(eval);
            eval->resetState();
        }
        FinishNelsonMainScript(eval);
        if (eval->getState() == NLS_STATE_QUIT) {
            goto EXIT;
        }
    }
EXIT:
    int exitCode = eval->getExitCode();
    ::destroyMainEvaluator();
    clearWarningIdsList();
    return exitCode;
}
//=============================================================================
static int
StartNelsonInternal(wstringVector args, NELSON_ENGINE_MODE _mode)
{
    int exitCode = -1;
    if (!SetNelSonEnvironmentVariables()) {
        ErrorPathDetection(_mode);
        return exitCode;
    }
    setMaxOpenedFiles();
    initializeDefaultWarningIdsList();
#ifdef _MSC_VER
#if _MSC_VER < 1900
    _set_output_format(_TWO_DIGIT_EXPONENT);
#endif
#else
    setRecursionStacksize(SIZE_MAX_RECURSION_CALL);
#endif
    setlocale(LC_NUMERIC, "C");
    if (getRecursionStacksize() < SIZE_MAX_RECURSION_CALL) {
        std::string msg;
        msg = _("Recursion stack not enough.\nPlease set C recursion stack to ");
        msg = msg + std::to_string(SIZE_MAX_RECURSION_CALL);
        msg.append("\n");
        msg = msg + _("Current C stack is: ") + std::to_string(getRecursionStacksize());
#ifdef _MSC_VER
        if (_mode == GUI) {
            MessageBox(NULL, utf8_to_wstring(msg).c_str(), L"Nelson error:", MB_ICONERROR);
        } else {
            std::cout << msg << std::endl;
        }
#else
        std::cout << msg << std::endl;
#endif
        return exitCode;
    }
    std::wstring fileToExecute;
    std::wstring commandToExecute;
    wstringVector filesToOpen;
    std::wstring lang;
    bool bQuietMode = false;
    ProgramOptions po(args);
    if (!po.isValid()) {
        ErrorCommandLine(po.getErrorMessage(), _mode);
        return exitCode;
    }
    if (po.haveOptionsHelp()) {
        displayHelp(po.getOptionsHelp(), _mode);
        return 0;
    }
    if (po.haveVersion()) {
        displayVersion(_mode);
        return 0;
    }
    if (po.haveTimeout()) {
        TimeoutThread(po.getTimeout());
    }
    filesToOpen = po.getFilesToOpen();
    commandToExecute = po.getCommandToExecute();
    fileToExecute = po.getFileToExecute();
    lang = po.getLanguage();
    if (!commandToExecute.empty() && !fileToExecute.empty()) {
        ErrorCommandLineMessage_file_commmand(_mode);
        return exitCode;
    }
    if (po.haveNoStartup() && po.haveNoUserStartup()) {
        ErrorCommandLineMessage_startup_exclusive(_mode);
        return exitCode;
    }
    bQuietMode = po.haveQuietMode();
    if (!fileToExecute.empty()) {
        // expand filename required for shebang
        boost::filesystem::path p(fileToExecute);
        boost::filesystem::path full_p = boost::filesystem::complete(p);
        fileToExecute = full_p.generic_wstring();
    }
    Evaluator* eval = createMainEvaluator(_mode, lang);
    if (eval) {
        setWarningEvaluator(eval);
        setErrorEvaluator(eval);
        eval->setQuietMode(bQuietMode);
        eval->setCommandLineArguments(args);
        if (lang != Localization::Instance()->getCurrentLanguage() && !lang.empty()) {
            Interface* io = eval->getInterface();
            Exception e(L"Wrong language.");
            eval->setLastErrorException(e);
            io->errorMessage(e.getMessage());
        }
        try {
            AddGateway(eval, ConstructDynamicLibraryFullname(Nelson::GetRootPath(), L"core"));
            AddGateway(
                eval, ConstructDynamicLibraryFullname(Nelson::GetRootPath(), L"modules_manager"));
            AddGateway(
                eval, ConstructDynamicLibraryFullname(Nelson::GetRootPath(), L"dynamic_link"));
            AddGateway(eval, ConstructDynamicLibraryFullname(Nelson::GetRootPath(), L"string"));
        } catch (const Exception& e) {
            Interface* io = eval->getInterface();
            eval->setLastErrorException(e);
            io->errorMessage(_W("Nelson cannot load base modules.\n"));
        }
        exitCode = NelsonMainStates(eval, po.haveNoStartup(), po.haveNoUserStartup(),
            commandToExecute, fileToExecute, filesToOpen);
        ::destroyMainEvaluator();
        clearWarningIdsList();
    } else {
        ErrorInterpreter(_mode);
    }
    return exitCode;
}
//=============================================================================
static int
StartNelsonInternalWithMutex(wstringVector args, NELSON_ENGINE_MODE _mode)
{
    openNelsonMutex();
    int exitCode = StartNelsonInternal(args, _mode);
    closeNelsonMutex();
    return exitCode;
}
//=============================================================================
int
StartNelson(int argc, wchar_t* argv[], NELSON_ENGINE_MODE _mode)
{
    wstringVector args;
    for (size_t l = 0; l < (size_t)argc; l++) {
        args.push_back(std::wstring(argv[l]));
    }
    return StartNelsonInternalWithMutex(args, _mode);
}
//=============================================================================
int
StartNelson(int argc, char* argv[], NELSON_ENGINE_MODE _mode)
{
    wstringVector args;
    for (size_t l = 0; l < (size_t)argc; l++) {
        args.push_back(utf8_to_wstring(argv[l]));
    }
    return StartNelsonInternalWithMutex(args, _mode);
}
//=============================================================================
