//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN
#endif
//=============================================================================
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include <boost/interprocess/managed_shared_memory.hpp>
#include <clocale>
#include <sstream>
#include "FileSystemWrapper.hpp"
#include "StartNelson.h"
#include "StartNelsonMainScript.hpp"
#include "StartNelsonUserScript.hpp"
#include "StartNelsonUserModules.hpp"
#include "FinishNelsonMainScript.hpp"
#include "FinishNelsonUserScript.hpp"
#include "AddGateway.hpp"
#include "EvaluateCommand.hpp"
#include "EvaluateScriptFile.hpp"
#include "Evaluator.hpp"
#include "GetNelsonPath.hpp"
#include "Localization.hpp"
#include "MainEvaluator.hpp"
#include "MaxOpenedFiles.hpp"
#include "ModulesHelpers.hpp"
#include "NelsonNamedMutex.hpp"
#include "Nelson_VERSION.h"
#include "ProgramOptions.hpp"
#include "RecursionStack.hpp"
#include "SetNelSonEnvironmentVariables.hpp"
#include "TimeoutThread.hpp"
#include "characters_encoding.hpp"
#include "SioClientCommand.hpp"
#include "WarningIds.hpp"
#include "WarningEmitter.h"
#include "ErrorEmitter.h"
#include "NelsonPrint.hpp"
#include "MxCall.h"
#include "NelsonConfiguration.hpp"
#include "FilesAssociation.hpp"
#include "NelsonReadyNamedMutex.hpp"
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
    std::wstring msg = _W("Too many arguments -f and -e are exclusive.");
#ifdef _MSC_BUILD
    if (_mode == GUI) {
        MessageBox(nullptr, msg.c_str(), _W("Error").c_str(), MB_ICONERROR);
    } else {
        std::cerr << wstring_to_utf8(msg) << std::endl;
    }
#else
    std::cerr << wstring_to_utf8(msg) << std::endl;
#endif
}
//=============================================================================
static void
ErrorCommandLineMessage_file_execute(NELSON_ENGINE_MODE _mode)
{
    std::wstring msg = _W("Too many arguments -f and -F are exclusive.");
#ifdef _MSC_BUILD
    if (_mode == GUI) {
        MessageBox(nullptr, msg.c_str(), _W("Error").c_str(), MB_ICONERROR);
    } else {
        std::cerr << wstring_to_utf8(msg) << std::endl;
    }
#else
    std::cerr << wstring_to_utf8(msg) << std::endl;
#endif
}
//=============================================================================
static void
ErrorPathDetection(NELSON_ENGINE_MODE _mode)
{
    std::wstring msg = _W("Nelson paths not initialized.");
#ifdef _MSC_BUILD
    if (_mode == GUI) {
        MessageBox(nullptr, msg.c_str(), _W("Error").c_str(), MB_ICONERROR);
    } else {
        msg = msg + L"\n";
        fwprintf(stderr, L"%ls", msg.c_str());
    }
#else
    msg = msg + L"\n";
    fwprintf(stderr, L"%ls", msg.c_str());
#endif
}
//=============================================================================
static void
ErrorInterpreter(NELSON_ENGINE_MODE _mode)
{
    std::wstring msg = _W("Nelson interpreter not initialized.");
#ifdef _MSC_BUILD
    if (_mode == GUI) {
        MessageBox(nullptr, msg.c_str(), _W("Error").c_str(), MB_ICONERROR);
    } else {
        msg = msg + L"\n";
        fwprintf(stderr, L"%ls", msg.c_str());
    }
#else
    msg = msg + L"\n";
    fwprintf(stderr, L"%ls", msg.c_str());
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
    std::wstring msg = L"Nelson options:";
#ifdef _MSC_VER
    if (_mode == GUI) {
        MessageBox(nullptr, description.c_str(), msg.c_str(), MB_ICONINFORMATION);
    } else {
        std::wcout << msg.c_str() << L"\n";
        std::wcout << description.c_str();
    }
#else
    std::wcout << msg.c_str() << L"\n";
    std::wcout << description.c_str();
#endif
}
//=============================================================================
static void
ErrorCommandLine(const std::wstring& str, NELSON_ENGINE_MODE _mode)
{
    std::wstring msg = L"Error:";
#ifdef _MSC_VER
    if (_mode == GUI) {
        MessageBox(nullptr, str.c_str(), msg.c_str(), MB_ICONINFORMATION);
    } else {
        std::wcout << msg.c_str() << L"\n";
        std::wcout << str;
    }
#else
    std::wcout << msg.c_str() << L"\n";
    std::wcout << str.c_str();
#endif
}
//=============================================================================
static int
NelsonMainStates(Evaluator* eval, bool haveNoStartup, bool haveNoUserStartup,
    bool haveNoUserModules, const std::wstring& commandToExecute, const std::wstring& fileToExecute,
    const wstringVector& filesToOpen, const wstringVector& filesToLoad)
{
    eval->resetState();
    if (!haveNoStartup) {
        StartNelsonMainScript(eval);
        eval->clearStacks();
        if (eval->getState() == NLS_STATE_QUIT) {
            goto FINISH;
        }
        eval->resetState();
        bool loadUserModulesSucceeced = false;
        if (!haveNoUserModules) {
            loadUserModulesSucceeced = StartNelsonUserModules(eval);
            eval->clearStacks();
            if (eval->getState() == NLS_STATE_QUIT) {
                goto FINISH;
            }
            eval->resetState();
        }
        if (!haveNoUserStartup && loadUserModulesSucceeced) {
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
            EvaluateCommand(eval, commandToExecute, false);
        }
        if (!fileToExecute.empty()) {
            EvaluateScriptFile(eval, fileToExecute);
        }
    } catch (Exception& e) {
        Interface* io = eval->getInterface();
        io->errorMessage(e.getMessage());
    }
    eval->isReadyToUse = true;
    OpenFilesAssociated(
        (NELSON_ENGINE_MODE)NelsonConfiguration::getInstance()->getNelsonEngineMode(), filesToOpen,
        false);
    LoadFilesAssociated(
        (NELSON_ENGINE_MODE)NelsonConfiguration::getInstance()->getNelsonEngineMode(), filesToLoad,
        false);
    while (eval->getState() != NLS_STATE_QUIT) {
        if (eval->getState() == NLS_STATE_ABORT) {
            eval->clearStacks();
        }
        eval->resetState();
        eval->evalCLI();
    }
    closeIsReadyNelsonMutex((int)boost::interprocess::ipcdetail::get_current_process_id());
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
    NelsonConfiguration::getInstance()->setNelsonEngineMode(_mode);
    ProgramOptions po(args, _mode);
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
    if (_mode == NELSON_ENGINE_MODE::GUI && !po.haveNoIpc()) {
        bool wasSent = false;
        if (po.haveFileToExecuteIPC()) {
            std::wstring fileToExecuteIPC = po.getFileToExecuteIPC();
            wstringVector fileToExecuteAsVector;
            fileToExecuteAsVector.push_back(fileToExecuteIPC);
            wasSent = ExecuteFilesAssociated(_mode, fileToExecuteAsVector, true);
            if (wasSent) {
                return 0;
            }
        }
        if (po.haveOpenFiles()) {
            wasSent = OpenFilesAssociated(_mode, po.getFilesToOpen(), true);
            if (wasSent) {
                return 0;
            }
        }
        if (po.haveLoadFiles()) {
            wasSent = LoadFilesAssociated(_mode, po.getFilesToLoad(), true);
            if (wasSent) {
                return 0;
            }
        }
    }
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

#ifndef _MSC_VER
    if (_mode == NELSON_ENGINE_MODE::BASIC_TERMINAL) {
        setlocale(LC_CTYPE, "en_US.utf8");
    }
#endif
    if (getRecursionStacksize() < SIZE_MAX_RECURSION_CALL) {
        std::string msg;
        msg = _("Recursion stack not enough.\nPlease set C recursion stack to ");
        msg = msg + std::to_string(SIZE_MAX_RECURSION_CALL);
        msg.append("\n");
        msg = msg + _("Current C stack is: ") + std::to_string(getRecursionStacksize());
#ifdef _MSC_VER
        if (_mode == GUI) {
            MessageBox(nullptr, utf8_to_wstring(msg).c_str(), L"Nelson error:", MB_ICONERROR);
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
    wstringVector filesToLoad;
    std::wstring lang;
    bool bQuietMode = false;
    if (po.haveTimeout()) {
        createTimeoutThread(po.getTimeout());
    }
    lang = po.getLanguage();
    if (po.haveCommandToExecute() && po.haveFileToExecute()) {
        ErrorCommandLineMessage_file_commmand(_mode);
        return exitCode;
    }
    if (po.haveFileToExecute() && po.haveFileToExecuteIPC()) {
        ErrorCommandLineMessage_file_execute(_mode);
        return exitCode;
    }
    if (po.haveNoStartup() && po.haveNoUserStartup()) {
        ErrorCommandLineMessage_startup_exclusive(_mode);
        return exitCode;
    }
    commandToExecute = po.getCommandToExecute();
    filesToOpen = po.getFilesToOpen();
    filesToLoad = po.getFilesToLoad();
    if (po.haveFileToExecute()) {
        fileToExecute = po.getFileToExecute();
    }
    if (po.haveFileToExecuteIPC()) {
        fileToExecute = po.getFileToExecuteIPC();
    }
    std::wstring socketIoURI;
    if (_mode == BASIC_SIO_CLIENT) {
        if (po.haveSocketIoUri()) {
            socketIoURI = po.getSocketIoUri();
        }
    }
    bQuietMode = po.haveQuietMode();
    if (!fileToExecute.empty()) {
        // expand filename required for shebang
        FileSystemWrapper::Path p(fileToExecute);
        FileSystemWrapper::Path full_p = FileSystemWrapper::Path::absolute(p);
        fileToExecute = full_p.generic_wstring();
    }

    Evaluator* eval = createMainEvaluator(_mode, lang, po.haveOptionsMinimize());
    if (eval != nullptr) {
        setWarningEvaluator(eval);
        setErrorEvaluator(eval);
        setPrintInterface(eval->getInterface());
        mexSetEvaluator(eval);
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
            AddGateway(eval, ConstructDynamicLibraryFullname(Nelson::GetRootPath(), L"string"));
        } catch (const Exception& e) {
            Interface* io = eval->getInterface();
            eval->setLastErrorException(e);
            io->errorMessage(_W("Nelson cannot load base modules.\n"));
        }
        if (!socketIoURI.empty()) {
            SioClientCommand::getInstance()->create(wstring_to_utf8(socketIoURI));
        }
        exitCode = NelsonMainStates(eval, po.haveNoStartup(), po.haveNoUserStartup(),
            po.haveNoUserModules(), commandToExecute, fileToExecute, filesToOpen, filesToLoad);
        ::destroyMainEvaluator();
        clearWarningIdsList();
        destroyTimeoutThread();
    } else {
        ErrorInterpreter(_mode);
    }
    return exitCode;
}
//=============================================================================
static int
StartNelsonInternalWithMutex(const wstringVector& args, NELSON_ENGINE_MODE _mode)
{
    if (!haveNelsonMutex()) {
        openNelsonMutex();
    }
    int exitCode = StartNelsonInternal(args, _mode);
    if (haveNelsonMutex()) {
        closeNelsonMutex();
    }
    return exitCode;
}
//=============================================================================
int
StartNelson(int argc, wchar_t* argv[], NELSON_ENGINE_MODE _mode)
{
    wstringVector args;
    for (size_t l = 0; l < static_cast<size_t>(argc); l++) {
        args.push_back(std::wstring(argv[l]));
    }
    return StartNelsonInternalWithMutex(args, _mode);
}
//=============================================================================
int
StartNelson(int argc, char* argv[], NELSON_ENGINE_MODE _mode)
{
    wstringVector args;
    for (size_t l = 0; l < static_cast<size_t>(argc); l++) {
        args.push_back(utf8_to_wstring(argv[l]));
    }
    return StartNelsonInternalWithMutex(args, _mode);
}
//=============================================================================
