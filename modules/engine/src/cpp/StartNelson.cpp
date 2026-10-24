//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#if defined(__clang__) && (__clang_major__ >= 17)
#pragma clang diagnostic ignored "-Wmissing-template-arg-list-after-template-kw"
#endif
#ifdef _MSC_VER
#include <Windows.h>
#else
#include <pthread.h>
#include <unistd.h>
#endif
#include <clocale>
#include <atomic>
#include <chrono>
#include <exception>
#include <functional>
#include <sstream>
#include <system_error>
#include <thread>
#include "FileSystemWrapper.hpp"
#include "StartNelson.h"
#include "StartNelsonMainScript.hpp"
#include "RunStartupScripts.hpp"
#include "StartNelsonUserModules.hpp"
#include "FinishNelsonMainScript.hpp"
#include "RunFinishScripts.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "Clear.hpp"
#include "ClearGlobal.hpp"
#include "EngineGatewayLifecycle.hpp"
#include "EvaluateCommand.hpp"
#include "EvaluateScriptFile.hpp"
#include "Evaluator.hpp"
#include "Localization.hpp"
#include "MainEvaluator.hpp"
#include "MaxOpenedFiles.hpp"
#include "ModulesHelpers.hpp"
#include "NelsonNamedMutex.hpp"
#include "Nelson_VERSION.h"
#include "ProgramOptions.hpp"
#include "RecursionStack.hpp"
#include "TimeoutThread.hpp"
#include "characters_encoding.hpp"
#include "WarningIds.hpp"
#include "ErrorID.hpp"
#include "NelsonPrint.hpp"
#include "NelsonConfiguration.hpp"
#include "RecentDirectories.hpp"
#include "FilesAssociation.hpp"
#include "NelsonReadyNamedMutex.hpp"
#include "ComputeNelsonPaths.hpp"
#include "i18n.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "RuntimeDispatch.hpp"
#ifdef NELSON_WEB_GUI_ENABLED
#include "MainWebGuiDynamic.hpp"
#endif
//=============================================================================
static bool
isWindowedStartupMode(NELSON_ENGINE_MODE mode)
{
    return mode == GUI || mode == WEB_GUI;
}
//=============================================================================
// nelson-gui and nelson-webview own no console: a MessageBox startup diagnostic
// would wait forever for a click in a --execute batch run.
static std::atomic_bool startupIsBatch(false);
//=============================================================================
static bool
isBatchStartupOption(const std::wstring& arg)
{
    static const wchar_t* batchOptions[]
        = { L"--execute", L"-e", L"--file", L"-f", L"--file-ipc", L"-F" };
    std::wstring name = arg;
    const size_t inlineValuePosition = name.find(L'=');
    if (inlineValuePosition != std::wstring::npos) {
        name = name.substr(0, inlineValuePosition);
    }
#ifdef _MSC_VER
    if (!name.empty() && name[0] == L'/') {
        name = L"-" + name.substr(1);
    }
#endif
    for (const auto* batchOption : batchOptions) {
        if (name == batchOption) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
isBatchStartupRequested(const wstringVector& args)
{
    for (const auto& arg : args) {
        if (arg == L"--") {
            break;
        }
        if (isBatchStartupOption(arg)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
#ifdef _MSC_VER
static bool
canReportOnStandardError()
{
    const HANDLE standardError = GetStdHandle(STD_ERROR_HANDLE);
    return standardError != nullptr && standardError != INVALID_HANDLE_VALUE;
}
//=============================================================================
// A modal dialog is used only for an interactive desktop launch. A batch launch
// (-e, -f, -F) or a caller that reads stderr gets the text there and a non-zero
// exit code instead.
static bool
useStartupDialog(NELSON_ENGINE_MODE mode)
{
    if (!isWindowedStartupMode(mode)) {
        return false;
    }
    if (startupIsBatch.load()) {
        return false;
    }
    return !canReportOnStandardError();
}
#endif
//=============================================================================
static void
ErrorCommandLineMessage_startup_exclusive(NELSON_ENGINE_MODE _mode)
{
    std::stringstream msg;
    msg << Nelson::resolveMessageTemplate("Nelson:engine:nostartupStartupMutualExclusive",
        "'nostartup' and 'startup' are mutual exclusive.")
        << "\n";
#ifdef _MSC_VER
    if (useStartupDialog(_mode)) {
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
ErrorCommandLineMessage_file_command(NELSON_ENGINE_MODE _mode)
{
    std::wstring msg = resolveMessageTemplate(
        L"Nelson:engine:tooManyFE", L"Too many arguments -f and -e are exclusive.");
#ifdef _MSC_BUILD
    if (useStartupDialog(_mode)) {
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
    std::wstring msg = resolveMessageTemplate(
        L"Nelson:engine:tooManyFF", L"Too many arguments -f and -F are exclusive.");
#ifdef _MSC_BUILD
    if (useStartupDialog(_mode)) {
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
    std::wstring msg = Nelson::resolveMessageTemplate(
        L"Nelson:engine:nelsonPathsInitialized", L"Nelson paths not initialized.");
#ifdef _MSC_BUILD
    if (useStartupDialog(_mode)) {
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
    std::wstring msg = Nelson::resolveMessageTemplate(
        L"Nelson:engine:nelsonInterpreterInitialized", L"Nelson interpreter not initialized.");
#ifdef _MSC_BUILD
    if (useStartupDialog(_mode)) {
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
    if (useStartupDialog(_mode)) {
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
displayHelp(const std::wstring& description, NELSON_ENGINE_MODE _mode)
{
    std::wstring msg = L"Nelson options:";
#ifdef _MSC_VER
    if (useStartupDialog(_mode)) {
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
    if (useStartupDialog(_mode)) {
        MessageBox(nullptr, str.c_str(), msg.c_str(), MB_ICONINFORMATION);
    } else {
        std::wcerr << msg.c_str() << L"\n";
        std::wcerr << str;
    }
#else
    std::wcerr << msg.c_str() << L"\n";
    std::wcerr << str.c_str();
#endif
}
//=============================================================================
static void
restoreDesktopCurrentDirectory()
{
    const NELSON_ENGINE_MODE mode = static_cast<NELSON_ENGINE_MODE>(
        NelsonConfiguration::getInstance()->getNelsonEngineMode());
    if (mode != NELSON_ENGINE_MODE::GUI && mode != NELSON_ENGINE_MODE::WEB_GUI) {
        return;
    }

    std::wstring directory = Nelson::getFirstValidRecentDirectory();
    if (directory.empty()) {
        directory = PathFunctionIndexerManager::getInstance()->getUserPath();
    }
    if (directory.empty()) {
        return;
    }

    std::string errorMessage;
    if (FileSystemWrapper::Path::current_path(directory, errorMessage)) {
        PathFunctionIndexerManager::getInstance()->setCurrentUserPath(
            FileSystemWrapper::Path::current_path().generic_wstring());
    }
}
//=============================================================================
// An error left uncaught by the -e command sets the exit code to 1. A later
// exit(code) overrides it; dbquit and quit leave it untouched. A command with
// nothing to run (a comment alone) is not an error: the evaluator records it
// silently as validScript.
static void
EvaluateCommandLineCommand(Evaluator* eval, const std::wstring& command)
{
    eval->resetLastUncaughtException();
    if (EvaluateCommand(eval, command, false)) {
        return;
    }
    NelsonException uncaught = eval->getLastUncaughtException();
    if (!uncaught.isEmpty() && uncaught.identifier() != L"Nelson:interpreter:validScript") {
        eval->setExitCode(1);
    }
}
//=============================================================================
static int
NelsonMainStates(Evaluator* eval, bool haveNoStartup, bool haveNoUserStartup,
    bool haveNoUserModules, const std::wstring& commandToExecute, const std::wstring& fileToExecute,
    const wstringVector& filesToOpen, const wstringVector& filesToLoad,
    const wstringVector& associatedFiles, bool destroyEvaluatorAtExit)
{
    bool isForceQuit = false;
    bool mainStartupSucceeded = true;
    eval->resetState();
    if (!haveNoStartup) {
        mainStartupSucceeded = StartNelsonMainScript(eval);
        eval->clearStacks();
        if (!mainStartupSucceeded) {
            eval->setExitCode(1);
            goto FINISH;
        }
        if (eval->isQuitOrForceQuitState()) {
            goto FINISH;
        }
        eval->resetState();
        bool loadUserModulesSucceeced = false;
        if (!haveNoUserModules) {
            loadUserModulesSucceeced = StartNelsonUserModules(eval);
            eval->clearStacks();
            if (eval->isQuitOrForceQuitState()) {
                goto FINISH;
            }
            eval->resetState();
        }
        restoreDesktopCurrentDirectory();
        if (!haveNoUserStartup && loadUserModulesSucceeced) {
            RunStartupScripts(eval);
            eval->clearStacks();
            if (eval->isQuitOrForceQuitState()) {
                goto FINISH;
            }
            eval->resetState();
        }
    }
    PathFunctionIndexerManager::getInstance()->startFileWatcher();
#ifdef NELSON_WEB_GUI_ENABLED
    if ((NELSON_ENGINE_MODE)NelsonConfiguration::getInstance()->getNelsonEngineMode()
        == NELSON_ENGINE_MODE::WEB_GUI) {
        std::string launchError;
        if (!WaitWebGuiLaunchedDynamic(launchError)) {
            eval->getInterface()->errorMessage(utf8_to_wstring(launchError));
            eval->setExitCode(1);
            goto FINISH;
        }
    }
#endif
    try {
        if (!commandToExecute.empty()) {
            EvaluateCommandLineCommand(eval, commandToExecute);
        }
        if (!fileToExecute.empty()) {
            EvaluateScriptFile(eval, fileToExecute);
        }
    } catch (NelsonException& e) {
        Interface* io = eval->getInterface();
        io->errorMessage(e.message());
        // An error left uncaught by the -f script fails the session, as for -e;
        // quit/exit(code) and dbquit (abort) keep their own exit code.
        if (!eval->isQuitOrForceQuitState() && eval->getState() != NLS_STATE_ABORT) {
            eval->setExitCode(1);
        }
    } catch (const std::exception& e) {
        // A stray C++ exception from -e/-f must fail the session cleanly, never
        // abort the process through std::terminate.
        eval->getInterface()->errorMessage(utf8_to_wstring(e.what()));
        eval->setExitCode(1);
    } catch (...) {
        eval->getInterface()->errorMessage(
            _W("An unexpected error occurred while executing the command."));
        eval->setExitCode(1);
    }

    eval->isReadyToUse = true;
    OpenAssociatedFiles(
        (NELSON_ENGINE_MODE)NelsonConfiguration::getInstance()->getNelsonEngineMode(),
        associatedFiles, false);
    OpenFilesAssociated(
        (NELSON_ENGINE_MODE)NelsonConfiguration::getInstance()->getNelsonEngineMode(), filesToOpen,
        false);
    LoadFilesAssociated(
        (NELSON_ENGINE_MODE)NelsonConfiguration::getInstance()->getNelsonEngineMode(), filesToLoad,
        false);
    if ((!commandToExecute.empty() || !fileToExecute.empty()) && filesToOpen.empty()
        && filesToLoad.empty() && associatedFiles.empty()) {
        goto FINISH;
    }
LOOP_EVAL:
    while (!eval->isQuitOrForceQuitState()) {
        if (eval->getState() == NLS_STATE_ABORT) {
            eval->clearStacks();
        }
        eval->resetState();
        eval->evalCLI();
    }
FINISH:
    // exit/quit leave a quit state that makes the evaluator stop on its first
    // instruction, so the finish.m hooks below would silently do nothing. Record
    // the "force" flavour (which skips them), then clear the state for every path.
    isForceQuit = eval->getState() == NLS_STATE_FORCE_QUIT;
    eval->resetState();
    if (!haveNoStartup) {
        if (mainStartupSucceeded && !haveNoUserStartup && !isForceQuit) {
            RunFinishScripts(eval);

            if (eval->getState() == NLS_STATE_CANCEL_QUIT) {
                eval->resetState();
                goto LOOP_EVAL;
            }
            eval->resetState();
        }
        auto get_current_process_id = []() -> int {
#ifdef _MSC_VER
            return static_cast<int>(GetCurrentProcessId());
#else
            return static_cast<int>(getpid());
#endif
        };
        ClearAllVariables(eval);
        ClearAllGlobalVariables(eval);
        DrainPendingHandleFinalizers(eval);
        closeIsReadyNelsonMutex(get_current_process_id());
        NelsonConfiguration::getInstance()->setAsShuttingDown(true);
        FinishNelsonMainScript(eval);
        if (eval->isQuitOrForceQuitState()) {
            goto EXIT;
        }
    }
EXIT:
    // Partial initialization cannot be made successful by shutdown scripts.
    int exitCode = mainStartupSucceeded ? eval->getExitCode() : 1;
    if (destroyEvaluatorAtExit) {
        ::destroyMainEvaluator();
    }
    clearWarningIdsList();
    return exitCode;
}
//=============================================================================
static bool
runEvaluatorInWorker(NELSON_ENGINE_MODE mode)
{
    return mode == NELSON_ENGINE_MODE::GUI || mode == NELSON_ENGINE_MODE::WEB_GUI;
}
//=============================================================================
#ifndef _MSC_VER
static void*
runEvaluatorThread(void* context)
{
    (*static_cast<std::function<void()>*>(context))();
    return nullptr;
}
#endif
//=============================================================================
static int
NelsonMainStatesWithMainThreadEvents(Evaluator* eval, NELSON_ENGINE_MODE mode, bool haveNoStartup,
    bool haveNoUserStartup, bool haveNoUserModules, const std::wstring& commandToExecute,
    const std::wstring& fileToExecute, const wstringVector& filesToOpen,
    const wstringVector& filesToLoad, const wstringVector& associatedFiles)
{
    int exitCode = -1;
    std::exception_ptr workerException;
    std::atomic_bool evaluatorDone(false);

    std::function<void()> evaluatorTask = [&]() {
        try {
            exitCode = NelsonMainStates(eval, haveNoStartup, haveNoUserStartup, haveNoUserModules,
                commandToExecute, fileToExecute, filesToOpen, filesToLoad, associatedFiles, false);
        } catch (...) {
            workerException = std::current_exception();
        }
        evaluatorDone.store(true, std::memory_order_release);
        if (mode == NELSON_ENGINE_MODE::GUI) {
            postToGui([]() { });
        }
    };

#ifdef _MSC_VER
    std::thread evaluatorThread(evaluatorTask);
#else
    pthread_attr_t evaluatorAttributes;
    int threadError = pthread_attr_init(&evaluatorAttributes);
    const bool attributesInitialized = threadError == 0;
    if (threadError == 0) {
        threadError = pthread_attr_setstacksize(&evaluatorAttributes, SIZE_MAX_RECURSION_CALL);
    }
    pthread_t evaluatorThread;
    if (threadError == 0) {
        threadError = pthread_create(
            &evaluatorThread, &evaluatorAttributes, runEvaluatorThread, &evaluatorTask);
    }
    if (attributesInitialized) {
        pthread_attr_destroy(&evaluatorAttributes);
    }
    if (threadError != 0) {
        throw std::system_error(
            threadError, std::generic_category(), "Cannot create the evaluator thread");
    }
#endif

    while (!evaluatorDone.load(std::memory_order_acquire)) {
        if (mode == NELSON_ENGINE_MODE::GUI) {
            // The blocking pump wakes on posted events, so evaluator-to-GUI calls
            // are not delayed by the sleep below (~15.6 ms OS timer tick on Windows).
            if (Nelson::pumpGuiEventsBlocking(50)) {
                continue;
            }
            Nelson::processGuiEvents(false);
        }
#ifdef NELSON_WEB_GUI_ENABLED
        else if (mode == NELSON_ENGINE_MODE::WEB_GUI) {
            ProcessWebGuiEventsDynamic();
        }
#endif
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
#ifdef _MSC_VER
    evaluatorThread.join();
#else
    const int joinError = pthread_join(evaluatorThread, nullptr);
    if (joinError != 0) {
        throw std::system_error(
            joinError, std::generic_category(), "Cannot join the evaluator thread");
    }
#endif
    if (mode == NELSON_ENGINE_MODE::GUI) {
        Nelson::processGuiEvents(false);
    }
#ifdef NELSON_WEB_GUI_ENABLED
    else if (mode == NELSON_ENGINE_MODE::WEB_GUI) {
        ShutdownWebGuiDynamic();
    }
#endif

    if (workerException) {
        std::rethrow_exception(workerException);
    }
    return exitCode;
}
//=============================================================================
static int
StartNelsonInternal(wstringVector args, NELSON_ENGINE_MODE _mode)
{
    int exitCode = -1;
    startupIsBatch.store(isBatchStartupRequested(args));
    std::wstring errorMessage;
    if (!ComputesNelsonPaths(errorMessage)) {
        ErrorCommandLine(errorMessage, _mode);
        return exitCode;
    }
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
    if (po.haveWithoutFileWatcher()) {
        NelsonConfiguration::getInstance()->disableFileWatcher();
    } else {
        NelsonConfiguration::getInstance()->enableFileWatcher();
    }
    // `nelson-adv-cli --webview`: keep the terminal REPL but render figures
    // through the headless RenderWeb host (Qt-free) instead of Qt windows.
    if (po.haveWebGraphics()) {
        NelsonConfiguration::getInstance()->setWebGraphicsMode(true);
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
        if (po.haveAssociatedFiles()) {
            wasSent = OpenAssociatedFiles(_mode, po.getAssociatedFiles(), true);
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
        // Initialize character classification from the process environment.
        // Locale aliases are platform-specific (macOS uses en_US.UTF-8), so a
        // single hard-coded spelling can silently leave LC_CTYPE at "C".
        if (setlocale(LC_CTYPE, "") == nullptr) {
            setlocale(LC_CTYPE, "en_US.UTF-8");
        }
    }
#endif
    if (getRecursionStacksize() < SIZE_MAX_RECURSION_CALL) {
        std::string msg = formatI18nMessage(
            Nelson::resolveMessageTemplate("Nelson:engine:recursionStackEnoughPlease",
                "Recursion stack not enough.\nPlease set C recursion stack to %d\nCurrent C stack "
                "is: %d"),
            SIZE_MAX_RECURSION_CALL, getRecursionStacksize());
#ifdef _MSC_VER
        if (useStartupDialog(_mode)) {
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
    wstringVector associatedFiles;
    std::wstring lang;
    bool bQuietMode = false;
    if (po.haveTimeout()) {
        createTimeoutThread(po.getTimeout());
    }
    lang = po.getLanguage();
    if (po.haveCommandToExecute() && po.haveFileToExecute()) {
        ErrorCommandLineMessage_file_command(_mode);
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
    associatedFiles = po.getAssociatedFiles();
    if (po.haveFileToExecute()) {
        fileToExecute = po.getFileToExecute();
    }
    if (po.haveFileToExecuteIPC()) {
        fileToExecute = po.getFileToExecuteIPC();
    }
    bQuietMode = po.haveQuietMode();
    if (!fileToExecute.empty()) {
        // expand filename required for shebang
        FileSystemWrapper::Path p(fileToExecute);
        FileSystemWrapper::Path full_p = FileSystemWrapper::Path::absolute(p);
        fileToExecute = full_p.generic_wstring();
    }

    Evaluator* eval = createMainEvaluator(_mode, lang, po.haveOptionsMinimize(), po.getWebUrl(),
        static_cast<uint16_t>(po.getWebPort()), po.haveWebUrl(), po.haveWebPort(),
        po.haveWebServerMode());
    if (eval != nullptr) {
        setPrintInterface(eval->getInterface());
        eval->setQuietMode(bQuietMode);
        eval->setCommandLineArguments(args);
        eval->setUserCommandLineArguments(po.getUserArguments());
        if (lang != Localization::Instance()->getCurrentLanguage() && !lang.empty()) {
            Interface* io = eval->getInterface();
            NelsonException e(L"Wrong language.");
            eval->setLastErrorException(e);
            io->errorMessage(e.message());
        }
        try {
            if (!registerEngineGateways(eval)) {
                throw NelsonException(L"Cannot register static modules.");
            }
        } catch (const NelsonException& e) {
            Interface* io = eval->getInterface();
            eval->setLastErrorException(e);
            io->errorMessage(resolveMessageTemplate(L"Nelson:engine:nelsonCannotLoadBase",
                                 L"Nelson cannot load base modules.")
                + L"\n");
        }
        const bool useMainThreadEvents = runEvaluatorInWorker(_mode)
            && (_mode == NELSON_ENGINE_MODE::WEB_GUI || hasGuiServices());
        prepareEngineEvaluator(eval);
        if (useMainThreadEvents) {
            exitCode = NelsonMainStatesWithMainThreadEvents(eval, _mode, po.haveNoStartup(),
                po.haveNoUserStartup(), po.haveNoUserModules(), commandToExecute, fileToExecute,
                filesToOpen, filesToLoad, associatedFiles);
        } else {
            exitCode = NelsonMainStates(eval, po.haveNoStartup(), po.haveNoUserStartup(),
                po.haveNoUserModules(), commandToExecute, fileToExecute, filesToOpen, filesToLoad,
                associatedFiles, false);
        }
        releaseEngineEvaluator(eval);
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
StartNelsonW(int argc, wchar_t* argv[], NELSON_ENGINE_MODE _mode)
{
    wstringVector args;
    for (size_t l = 0; l < static_cast<size_t>(argc); l++) {
        args.push_back(std::wstring(argv[l]));
    }
    return StartNelsonInternalWithMutex(args, _mode);
}
//=============================================================================
int
StartNelsonA(int argc, char* argv[], NELSON_ENGINE_MODE _mode)
{
    wstringVector args;
    for (size_t l = 0; l < static_cast<size_t>(argc); l++) {
        args.push_back(utf8_to_wstring(argv[l]));
    }
    return StartNelsonInternalWithMutex(args, _mode);
}
//=============================================================================
