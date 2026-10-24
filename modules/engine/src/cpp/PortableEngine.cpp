//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PortableEngine.hpp"
#include "PortableEngineC.h"
#include <array>
#include <clocale>
#include <cstdlib>
#include <mutex>
#include "AnsiSgr.hpp"
#include "AddGateway.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "Clear.hpp"
#include "ClearGlobal.hpp"
#include "CodeAnalyzer.hpp"
#include "ComputeNelsonPaths.hpp"
#include "EvaluateCommand.hpp"
#include "Evaluator.hpp"
#include "Interface.hpp"
#include "MainEvaluator.hpp"
#include "MaxOpenedFiles.hpp"
#include "ModulesHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "QueueProcessing.hpp"
#include "NelsonPrint.hpp"
#include "RecursionStack.hpp"
#include "StartNelsonUserModules.hpp"
#include "WarningIds.hpp"
#include "WebDesktopBridge.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
//=============================================================================
#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#define NELSON_WASM_EXPORT EMSCRIPTEN_KEEPALIVE
#if defined(NELSON_WASM_JSPI)
EM_ASYNC_JS(int, nelson_wasm_read_line_prepare, (const char* prompt), {
    const callback = Module['onNelsonInput'];
    if (typeof callback != 'function') {
        return 0;
    }
    const value = await callback(UTF8ToString(prompt));
    Module['__nelsonInputText'] = value == null ? "" : String(value);
    return lengthBytesUTF8(Module['__nelsonInputText']) + 1;
});
//=============================================================================
EM_JS(void, nelson_wasm_read_line_copy, (char* output, int length), {
    const text = Module['__nelsonInputText'] || "";
    stringToUTF8(text, output, length);
    delete Module['__nelsonInputText'];
});
#endif
#else
#define NELSON_WASM_EXPORT
#endif
//=============================================================================
#ifdef NELSON_STATIC_GATEWAYS
namespace Nelson {
bool
RegisterStaticGateways(Evaluator* eval);
bool
UnregisterStaticGateways(Evaluator* eval);
//=============================================================================
} // namespace Nelson
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
class PortableInterface final : public Interface
{
public:
    explicit PortableInterface(const PortableConfiguration& configuration)
        : outputCallback(configuration.stdoutCallback)
        , errorCallback(configuration.stderrCallback)
        , clearTerminalCallback(configuration.clearTerminalCallback)
        , htmlCallback(configuration.htmlCallback)
        , expandableTextCallback(configuration.expandableTextCallback)
    {
    }

    std::string
    getLine(const std::string& prompt) override
    {
#if defined(__EMSCRIPTEN__) && defined(NELSON_WASM_JSPI)
        return readLineFromBrowser(prompt);
#else
        return {};
#endif
    }
    std::wstring
    getLine(const std::wstring& prompt) override
    {
#if defined(__EMSCRIPTEN__) && defined(NELSON_WASM_JSPI)
        return utf8_to_wstring(readLineFromBrowser(wstring_to_utf8(prompt)));
#else
        return {};
#endif
    }
    std::wstring
    getInput(const std::wstring& prompt) override
    {
#if defined(__EMSCRIPTEN__) && defined(NELSON_WASM_JSPI)
        return utf8_to_wstring(readLineFromBrowser(wstring_to_utf8(prompt)));
#else
        return {};
#endif
    }
    size_t
    getTerminalWidth() override
    {
        return DEFAULT_CONSOLE_WIDTH;
    }
    size_t
    getTerminalHeight() override
    {
        return DEFAULT_CONSOLE_HEIGHT;
    }
    void
    outputMessage(const std::wstring& message) override
    {
        outputMessage(wstring_to_utf8(message));
    }
    void
    outputMessage(const std::string& message) override
    {
        standardOutput.append(message);
        if (outputCallback) {
            outputCallback(message);
        }
    }
    void
    errorMessage(const std::wstring& message) override
    {
        errorMessage(wstring_to_utf8(message));
    }
    void
    errorMessage(const std::string& message) override
    {
        const std::string line = message + "\n";
        standardError.append(line);
        if (errorCallback) {
            errorCallback(line);
        }
    }
    void
    warningMessage(const std::wstring& message) override
    {
        warningMessage(wstring_to_utf8(message));
    }
    void
    warningMessage(const std::string& message) override
    {
        outputMessage(message + "\n");
    }
    void
    clearTerminal() override
    {
        if (clearTerminalCallback) {
            clearTerminalCallback();
        }
    }
    bool
    outputStyledText(const std::wstring& message, const TextStyle& style) override
    {
        outputMessage(buildAnsiStyledMessage(wstring_to_utf8(message), style));
        return !style.isPlain();
    }
    bool
    outputExpandableText(const std::wstring& label, const std::wstring& expandedText,
        const std::wstring& plainFallback, const std::wstring& contextName = L"",
        long long contextValue = 0) override
    {
        (void)contextName;
        (void)contextValue;
        if (!expandableTextCallback) {
            return Interface::outputExpandableText(label, expandedText, plainFallback);
        }
        const long long id = ++expandableTextId;
        expandableTextCallback(id, wstring_to_utf8(expandedText));
        outputMessage(buildExpandableTextLink(id, wstring_to_utf8(label)) + "\n");
        return true;
    }
    bool
    supportsTruncatedMatrixDisplay() override
    {
        return expandableTextCallback != nullptr;
    }
    bool
    outputTruncatedMatrix(const std::wstring& truncatedText, const std::wstring& label,
        const std::wstring& expandedText, const std::wstring& variableName) override
    {
        outputMessage(truncatedText);
        return outputExpandableText(label, expandedText, expandedText, variableName, 0);
    }
    bool
    insertHtml(const std::wstring& html) override
    {
        if (!htmlCallback) {
            return false;
        }
        htmlCallback(wstring_to_utf8(html));
        return true;
    }
    bool
    supportsHyperlinks() override
    {
        return expandableTextCallback != nullptr;
    }
    bool
    isAtPrompt() override
    {
        return false;
    }
    void
    interruptGetLineByEvent() override
    {
    }

    void
    clear()
    {
        standardOutput.clear();
        standardError.clear();
    }
    const std::string&
    output() const
    {
        return standardOutput;
    }
    const std::string&
    error() const
    {
        return standardError;
    }

private:
#if defined(__EMSCRIPTEN__) && defined(NELSON_WASM_JSPI)
    static std::string
    readLineFromBrowser(const std::string& prompt)
    {
        int length = nelson_wasm_read_line_prepare(prompt.c_str());
        if (length <= 1) {
            return {};
        }
        std::string text(static_cast<size_t>(length), '\0');
        nelson_wasm_read_line_copy(text.data(), length);
        text.resize(static_cast<size_t>(length - 1));
        return text;
    }
#endif

    std::function<void(std::string_view)> outputCallback;
    std::function<void(std::string_view)> errorCallback;
    std::function<void()> clearTerminalCallback;
    std::function<void(std::string_view)> htmlCallback;
    std::function<void(long long, std::string_view)> expandableTextCallback;
    long long expandableTextId = 0;
    std::string standardOutput;
    std::string standardError;
};
//=============================================================================
class PortableEngine::Impl
{
public:
    PortableConfiguration configuration;
    Evaluator* evaluator = nullptr;
    std::unique_ptr<PortableInterface> interface;
    bool gatewaysRegistered = false;
};
//=============================================================================
static std::recursive_mutex engineOwnerMutex;
static PortableEngine* engineOwner = nullptr;
//=============================================================================
static void
applyResourceOverrides(const PortableResources& resources)
{
    auto* configuration = NelsonConfiguration::getInstance();
    if (!resources.rootDirectory.empty()) {
        configuration->setNelsonRootDirectory(resources.rootDirectory);
    }
    if (!resources.modulesDirectory.empty()) {
        configuration->setNelsonModulesDirectory(resources.modulesDirectory);
    }
    if (!resources.libraryDirectory.empty()) {
        configuration->setNelsonLibraryDirectory(resources.libraryDirectory);
    }
    if (!resources.binaryDirectory.empty()) {
        configuration->setNelsonBinaryDirectory(resources.binaryDirectory);
    }
}
//=============================================================================
static bool
registerBaseGateways(Evaluator* evaluator)
{
#ifdef NELSON_STATIC_GATEWAYS
    return RegisterStaticGateways(evaluator);
#else
    auto* configuration = NelsonConfiguration::getInstance();
    const auto& libraryDirectory = configuration->getNelsonLibraryDirectory();
    AddGateway(evaluator, ConstructDynamicLibraryFullname(libraryDirectory, L"core", true));
    AddGateway(
        evaluator, ConstructDynamicLibraryFullname(libraryDirectory, L"modules_manager", true));
    AddGateway(evaluator, ConstructDynamicLibraryFullname(libraryDirectory, L"string", true));
    return true;
#endif
}
//=============================================================================
static nlohmann::json
debuggerStateToJson(Evaluator* evaluator)
{
    nlohmann::json result = { { "running", false }, { "currentLine", nullptr },
        { "currentFile", nullptr }, { "stack", nlohmann::json::array() } };
    if (evaluator == nullptr || !evaluator->isBreakpointActive()) {
        return result;
    }

    result["running"] = true;
    if (evaluator->stepBreakpoint.has_value()) {
        const Breakpoint& bp = evaluator->stepBreakpoint.value();
        const auto file = wstring_to_utf8(bp.filename);
        const auto functionName = bp.functionName;
        result["currentFile"] = file;
        result["currentLine"] = static_cast<long long>(bp.line);
        result["stack"].push_back({ { "file", file }, { "line", static_cast<long long>(bp.line) },
            { "function", functionName } });
    }
    return result;
}
//=============================================================================
static nlohmann::json
breakpointsToJson(Evaluator* evaluator, const std::string& file)
{
    nlohmann::json lines = nlohmann::json::array();
    if (evaluator != nullptr) {
        for (size_t line : evaluator->getBreakpointLines(utf8_to_wstring(file))) {
            lines.push_back(static_cast<long long>(line));
        }
    }
    return { { "file", file }, { "lines", lines } };
}
//=============================================================================
static bool
isPriorityDebuggerCommand(const std::wstring& command)
{
    return command == L"dbstep" || command == L"dbstep in" || command == L"dbstep out"
        || command == L"dbcont" || command == L"dbquit" || command == L"dbquit all";
}
//=============================================================================
PortableEngine::PortableEngine() : impl(std::make_unique<Impl>()) { }
//=============================================================================
PortableEngine::~PortableEngine() { stop(); }
//=============================================================================
bool
PortableEngine::start(const PortableConfiguration& configuration)
{
    if (impl->evaluator != nullptr) {
        return true;
    }
    std::lock_guard<std::recursive_mutex> lock(engineOwnerMutex);
    if (engineOwner != nullptr && engineOwner != this) {
        return false;
    }
    if (NelsonConfiguration::getInstance()->getMainEvaluator() != nullptr) {
        return false;
    }
    applyResourceOverrides(configuration.resources);
    std::wstring pathError;
    if (!ComputesNelsonPaths(pathError)) {
        if (configuration.stderrCallback) {
            configuration.stderrCallback(wstring_to_utf8(pathError));
        }
        return false;
    }
    auto* runtimeConfiguration = NelsonConfiguration::getInstance();
#ifdef __EMSCRIPTEN__
    constexpr auto portableMode = NELSON_ENGINE_MODE::WASM_TERMINAL;
#else
    constexpr auto portableMode = NELSON_ENGINE_MODE::BASIC_TERMINAL;
#endif
    runtimeConfiguration->setNelsonEngineMode(portableMode);
    runtimeConfiguration->setAsShuttingDown(false);
    runtimeConfiguration->disableFileWatcher();
    setMaxOpenedFiles();
    initializeDefaultWarningIdsList();
#ifndef _MSC_VER
    setRecursionStacksize(SIZE_MAX_RECURSION_CALL);
#endif
    setlocale(LC_NUMERIC, "C");
    impl->configuration = configuration;
    impl->evaluator = createMainEvaluator(portableMode, configuration.language, false);
    if (impl->evaluator == nullptr) {
        clearWarningIdsList();
        return false;
    }
    impl->interface = std::make_unique<PortableInterface>(configuration);
    impl->evaluator->setInterface(impl->interface.get());
    setPrintInterface(impl->interface.get());
#ifdef __EMSCRIPTEN__
    // Makes getwebmode() consistent with the "browser" launch mode of the frontend.
    WebDesktopBridge::getInstance()->setWebLaunchMode("browser");
#endif
    WebDesktopBridge::getInstance()->setWebEditorOpener(
        [](void* context, const wchar_t* path, int line) {
            auto* portableInterface = static_cast<PortableInterface*>(context);
            const std::wstring editorPath = path == nullptr ? std::wstring() : std::wstring(path);
            nlohmann::json action { { "type", "open-editor" },
                { "path", wstring_to_utf8(editorPath) } };
            if (line > 0) {
                action["line"] = line;
            }
            portableInterface->outputMessage(
                "__NELSON_UI_ACTION_BEGIN__" + action.dump() + "__NELSON_UI_ACTION_END__\n");
        },
        impl->interface.get());
#ifdef __EMSCRIPTEN__
    WebDesktopBridge::getInstance()->setWebNotebookOpener(
        [](void* context, const wchar_t* path) {
            auto* portableInterface = static_cast<PortableInterface*>(context);
            const std::wstring scriptPath = path == nullptr ? std::wstring() : std::wstring(path);
            nlohmann::json action { { "type", "open-notebook" },
                { "path", wstring_to_utf8(scriptPath) } };
            portableInterface->outputMessage(
                "__NELSON_UI_ACTION_BEGIN__" + action.dump() + "__NELSON_UI_ACTION_END__\n");
        },
        impl->interface.get());
    WebDesktopBridge::getInstance()->setWebNFlowOpener(
        [](void* context, Evaluator*, const wchar_t* modelPath) {
            auto* portableInterface = static_cast<PortableInterface*>(context);
            nlohmann::json action { { "type", "open-nflow" } };
            if (modelPath != nullptr && modelPath[0] != L'\0') {
                action["path"] = wstring_to_utf8(std::wstring(modelPath));
            }
            portableInterface->outputMessage(
                "__NELSON_UI_ACTION_BEGIN__" + action.dump() + "__NELSON_UI_ACTION_END__\n");
        },
        impl->interface.get());
    static_assert(sizeof(WebFigureFrameAttachment) == 12,
        "PortableEngine assumes wasm32 WebFigureFrameAttachment layout in EM_ASM.");
    if (EM_ASM_INT({ return typeof Module['onNelsonFigureFrameAttachments'] == 'function'; })) {
        WebDesktopBridge::getInstance()->setWebFigureFrameAttachmentHook(
            [](void*, long long handle, const char* json, int length,
                const WebFigureFrameAttachment* attachments, int attachmentCount) {
                // clang-format off
                EM_ASM(
                    {
                        const callback = Module['onNelsonFigureFrameAttachments'];
                        if (typeof callback != 'function') {
                            return;
                        }
                        const jsonBytes = HEAPU8.slice($1, $1 + $2).buffer;
                        const attachmentList = [];
                        const stride = 12;
                        for (let i = 0; i < $4; i++) {
                            const base = $3 + i * stride;
                            const idPtr = HEAPU32[base >> 2];
                            const dataPtr = HEAPU32[(base + 4) >> 2];
                            const byteLength = HEAP32[(base + 8) >> 2];
                            if (idPtr && dataPtr && byteLength > 0) {
                                attachmentList.push({
                                    id: UTF8ToString(idPtr),
                                    data: HEAPU8.slice(dataPtr, dataPtr + byteLength).buffer,
                                });
                            }
                        }
                        callback($0, jsonBytes, attachmentList);
                    },
                    handle, json, length, attachments, attachmentCount);
                // clang-format on
            },
            nullptr);
    }
    if (EM_ASM_INT({
            return typeof Module['onNelsonFigureFrameBytes'] == 'function'
                || typeof Module['onNelsonFigureFrame'] == 'function';
        })) {
        WebDesktopBridge::getInstance()->setWebFigureFrameHook(
            [](void*, long long handle, const char* json, int length) {
                // clang-format off
                EM_ASM(
                    {
                        const bytesCallback = Module['onNelsonFigureFrameBytes'];
                        if (typeof bytesCallback == 'function') {
                            const bytes = HEAPU8.slice($1, $1 + $2);
                            bytesCallback($0, bytes.buffer);
                            return;
                        }
                        Module['onNelsonFigureFrame']($0, UTF8ToString($1, $2));
                    },
                    handle, json, length);
                // clang-format on
            },
            nullptr);
    }
    if (EM_ASM_INT({ return typeof Module['onNelsonPollCommand'] == 'function'; })) {
        WebDesktopBridge::getInstance()->setWebEventPumpHook(
            [](void* context) {
                auto* evaluator = static_cast<Evaluator*>(context);
                const std::string debugState = debuggerStateToJson(evaluator).dump();
                // clang-format off
                EM_ASM(
                    {
                        const callback = Module['onNelsonDebuggerState'];
                        if (typeof callback == 'function') {
                            callback(UTF8ToString($0, $1));
                        }
                    },
                    debugState.data(), debugState.size());
                // clang-format on
                constexpr size_t commandCapacity = 64 * 1024 + 1;
                static std::array<char, commandCapacity> commandBuffer;
                // clang-format off
                const int length = EM_ASM_INT(
                    {
                        const poll = Module['onNelsonPollCommand'];
                        if (typeof poll != 'function') {
                            return 0;
                        }
                        const command = poll();
                        if (typeof command != 'string' || command.length == 0) {
                            return 0;
                        }
                        const length = lengthBytesUTF8(command);
                        if (length >= $1) {
                            return -1;
                        }
                        stringToUTF8(command, $0, $1);
                        return length;
                    },
                    commandBuffer.data(), commandBuffer.size());
                // clang-format on
                if (length > 0) {
                    const std::wstring command
                        = utf8_to_wstring(std::string(commandBuffer.data(), length));
                    evaluator->addCommandToQueue(command, isPriorityDebuggerCommand(command));
                }
            },
            impl->evaluator);
    }
#endif
    impl->evaluator->setQuietMode(true);
    impl->evaluator->setCommandLineArguments(configuration.commandLineArguments.empty()
            ? std::vector<std::wstring> { L"nelson-portable" }
            : configuration.commandLineArguments);
    try {
        impl->gatewaysRegistered = registerBaseGateways(impl->evaluator);
    } catch (const NelsonException& exception) {
        impl->interface->errorMessage(exception.message());
        impl->gatewaysRegistered = false;
    }
    if (!impl->gatewaysRegistered) {
        stop();
        return false;
    }
    impl->evaluator->isReadyToUse = true;
    engineOwner = this;
    return true;
}
//=============================================================================
bool
PortableEngine::loadUserModules()
{
    if (impl->evaluator == nullptr) {
        return false;
    }
    return StartNelsonUserModules(impl->evaluator);
}
//=============================================================================
EvaluationResult
PortableEngine::evaluate(std::string_view command)
{
    EvaluationResult result;
    if (impl->evaluator == nullptr || impl->interface == nullptr) {
        result.errorIdentifier = "Nelson:engine:notStarted";
        result.errorMessage = "Portable engine is not started.";
        return result;
    }
    impl->interface->clear();
    impl->evaluator->resetState();
    impl->evaluator->resetLastUncaughtException();
    impl->evaluator->setExitCode(0);
    bool evaluated = false;
    try {
        evaluated = EvaluateCommand(impl->evaluator, std::string(command), false);
    } catch (const NelsonException& exception) {
        impl->evaluator->setLastUncaughtException(exception);
        if (!exception.isEmpty()) {
            impl->interface->errorMessage(exception.formattedErrorMessage());
        }
    }
    const NelsonException exception = impl->evaluator->getLastUncaughtException();
    const bool emptyScript
        = !exception.isEmpty() && exception.identifier() == L"Nelson:interpreter:validScript";
    result.succeeded = evaluated || emptyScript;
    result.interrupted = impl->evaluator->getState() == NLS_STATE_ABORT;
    result.stdoutText = impl->interface->output();
    result.stderrText = impl->interface->error();
    if (!exception.isEmpty() && !emptyScript) {
        result.errorIdentifier = wstring_to_utf8(exception.identifier());
        result.errorMessage = wstring_to_utf8(exception.message());
    }
    impl->evaluator->clearStacks();
    impl->evaluator->resetState();
    if (impl->configuration.figureCallback) {
        impl->configuration.figureCallback();
    }
    return result;
}
//=============================================================================
std::string
PortableEngine::debuggerStateJson() const
{
    return debuggerStateToJson(impl->evaluator).dump();
}
//=============================================================================
std::string
PortableEngine::debuggerGetBreakpointsJson(std::string_view file) const
{
    return breakpointsToJson(impl->evaluator, std::string(file)).dump();
}
//=============================================================================
std::string
PortableEngine::debuggerToggleBreakpointJson(std::string_view file, size_t line)
{
    if (impl->evaluator == nullptr) {
        return breakpointsToJson(nullptr, std::string(file)).dump();
    }
    const std::string fileUtf8(file);
    const std::wstring filename = utf8_to_wstring(fileUtf8);
    if (impl->evaluator->hasBreakpoint(filename, line)) {
        impl->evaluator->removeBreakpoint(filename, line);
    } else {
        Breakpoint bp;
        bp.filename = filename;
        bp.line = line;
        bp.enabled = true;
        impl->evaluator->addBreakpoint(bp);
    }
    return breakpointsToJson(impl->evaluator, fileUtf8).dump();
}
//=============================================================================
void
PortableEngine::requestInterrupt()
{
    if (impl->evaluator != nullptr) {
        NelsonConfiguration::getInstance()->setInterruptPending(true, impl->evaluator->getID());
        impl->evaluator->getInterface()->interruptGetLineByEvent();
    }
}
//=============================================================================
void
PortableEngine::pump()
{
    if (impl->evaluator == nullptr) {
        return;
    }
    processPendingCallbacksAndTimers(impl->evaluator);
}
//=============================================================================
void
PortableEngine::reset()
{
    if (impl->evaluator == nullptr) {
        return;
    }
    ClearAllVariables(impl->evaluator);
    ClearAllGlobalVariables(impl->evaluator);
    ClearAllPersistentVariables(impl->evaluator);
    DrainPendingHandleFinalizers(impl->evaluator);
    impl->evaluator->clearStacks();
    impl->evaluator->resetLastErrorException();
    impl->evaluator->resetLastUncaughtException();
    impl->evaluator->resetState();
    NelsonConfiguration::getInstance()->setInterruptPending(false, impl->evaluator->getID());
}
//=============================================================================
void
PortableEngine::stop()
{
    std::lock_guard<std::recursive_mutex> lock(engineOwnerMutex);
    if (impl->evaluator == nullptr) {
        if (engineOwner == this) {
            engineOwner = nullptr;
        }
        return;
    }
#ifdef NELSON_STATIC_GATEWAYS
    if (impl->gatewaysRegistered) {
        UnregisterStaticGateways(impl->evaluator);
    }
#endif
    impl->evaluator->setInterface(impl->evaluator->getDefaultInterface());
    setPrintInterface(impl->evaluator->getDefaultInterface());
    WebDesktopBridge::getInstance()->setWebEditorOpener(nullptr, nullptr);
#ifdef __EMSCRIPTEN__
    WebDesktopBridge::getInstance()->setWebNotebookOpener(nullptr, nullptr);
#endif
    WebDesktopBridge::getInstance()->setWebNFlowOpener(nullptr, nullptr);
#ifdef __EMSCRIPTEN__
    WebDesktopBridge::getInstance()->setWebLaunchMode("none");
#endif
    WebDesktopBridge::getInstance()->setWebFigureFrameHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureFrameAttachmentHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebEventPumpHook(nullptr, nullptr);
    impl->interface.reset();
    impl->evaluator = nullptr;
    impl->gatewaysRegistered = false;
    destroyMainEvaluator();
    clearWarningIdsList();
    if (engineOwner == this) {
        engineOwner = nullptr;
    }
}
//=============================================================================
bool
PortableEngine::isStarted() const
{
    return impl->evaluator != nullptr;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
namespace {
std::mutex cEngineMutex;
Nelson::PortableEngine cEngine;
Nelson::EvaluationResult cResult;
std::string cAnalysisResult = "[]";
std::string cDebuggerResult = "{}";
#ifdef __EMSCRIPTEN__
void
publishWasmOutput(bool error, std::string_view text)
{
    // clang-format off
    EM_ASM(
        {
            const callback = Module['onNelsonOutput'];
            if (typeof callback == 'function') {
                callback(Boolean($0), UTF8ToString($1, $2));
            }
        },
        error ? 1 : 0, text.data(), text.size());
    // clang-format on
}
//=============================================================================
void
publishWasmClearTerminal()
{
    // clang-format off
    EM_ASM(
        {
            const callback = Module['onNelsonClearTerminal'];
            if (typeof callback == 'function') {
                callback();
            }
        });
    // clang-format on
}
//=============================================================================
void
publishWasmHtml(std::string_view html)
{
    // clang-format off
    EM_ASM(
        {
            const callback = Module['onNelsonHtml'];
            if (typeof callback == 'function') {
                callback(UTF8ToString($0, $1));
            }
        },
        html.data(), html.size());
    // clang-format on
}
//=============================================================================
void
publishWasmExpandableText(long long id, std::string_view text)
{
    // clang-format off
    EM_ASM(
        {
            const callback = Module['onNelsonExpandableText'];
            if (typeof callback == 'function') {
                callback(Number($0), UTF8ToString($1, $2));
            }
        },
        id, text.data(), text.size());
    // clang-format on
}
#endif
bool
startCEngine()
{
#ifdef __EMSCRIPTEN__
    Nelson::PortableConfiguration configuration;
    configuration.stdoutCallback = [](std::string_view text) { publishWasmOutput(false, text); };
    configuration.stderrCallback = [](std::string_view text) { publishWasmOutput(true, text); };
    configuration.clearTerminalCallback = []() { publishWasmClearTerminal(); };
    configuration.htmlCallback = [](std::string_view html) { publishWasmHtml(html); };
    configuration.expandableTextCallback
        = [](long long id, std::string_view text) { publishWasmExpandableText(id, text); };
    return cEngine.start(configuration);
#else
    return cEngine.start();
#endif
}
//=============================================================================
} // namespace
//=============================================================================
extern "C" NELSON_WASM_EXPORT int
nlsPortableStart()
{
    std::lock_guard<std::mutex> lock(cEngineMutex);
    return startCEngine() ? 0 : 1;
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT int
nlsPortableLoadUserModules()
{
    std::lock_guard<std::mutex> lock(cEngineMutex);
    if (!cEngine.isStarted() && !startCEngine()) {
        return 1;
    }
    return cEngine.loadUserModules() ? 0 : 1;
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT int
nlsPortableEvaluate(const char* command)
{
    std::lock_guard<std::mutex> lock(cEngineMutex);
    if (!cEngine.isStarted() && !startCEngine()) {
        return 1;
    }
    cResult = cEngine.evaluate(command == nullptr ? std::string_view() : std::string_view(command));
    return cResult.succeeded ? 0 : 1;
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT const char*
nlsPortableAnalyzeCode(const char* path, const char* source)
{
    std::lock_guard<std::mutex> lock(cEngineMutex);
    if (!cEngine.isStarted() && !startCEngine()) {
        cAnalysisResult = "[]";
        return cAnalysisResult.c_str();
    }
    try {
        Nelson::CodeAnalyzerOptions options;
        const auto diagnostics
            = Nelson::analyzeCodeBuffer(Nelson::utf8_to_wstring(path == nullptr ? "" : path),
                source == nullptr ? "" : source, options);
        cAnalysisResult = Nelson::codeAnalyzerDiagnosticsToJson(diagnostics);
    } catch (...) {
        cAnalysisResult = "[]";
    }
    return cAnalysisResult.c_str();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT const char*
nlsPortableDebuggerState()
{
    std::lock_guard<std::mutex> lock(cEngineMutex);
    if (!cEngine.isStarted() && !startCEngine()) {
        cDebuggerResult = R"({"running":false,"currentLine":null,"currentFile":null,"stack":[]})";
        return cDebuggerResult.c_str();
    }
    cDebuggerResult = cEngine.debuggerStateJson();
    return cDebuggerResult.c_str();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT const char*
nlsPortableDebuggerGetBreakpoints(const char* file)
{
    std::lock_guard<std::mutex> lock(cEngineMutex);
    const std::string fileUtf8 = file == nullptr ? std::string() : std::string(file);
    if (!cEngine.isStarted() && !startCEngine()) {
        cDebuggerResult
            = std::string(R"({"file":)") + nlohmann::json(fileUtf8).dump() + R"(,"lines":[]})";
        return cDebuggerResult.c_str();
    }
    cDebuggerResult = cEngine.debuggerGetBreakpointsJson(fileUtf8);
    return cDebuggerResult.c_str();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT const char*
nlsPortableDebuggerToggleBreakpoint(const char* file, int line)
{
    std::lock_guard<std::mutex> lock(cEngineMutex);
    const std::string fileUtf8 = file == nullptr ? std::string() : std::string(file);
    if (!cEngine.isStarted() && !startCEngine()) {
        cDebuggerResult
            = std::string(R"({"file":)") + nlohmann::json(fileUtf8).dump() + R"(,"lines":[]})";
        return cDebuggerResult.c_str();
    }
    cDebuggerResult
        = cEngine.debuggerToggleBreakpointJson(fileUtf8, line > 0 ? static_cast<size_t>(line) : 0);
    return cDebuggerResult.c_str();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT const char*
nlsPortableStdout()
{
    return cResult.stdoutText.c_str();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT const char*
nlsPortableStderr()
{
    return cResult.stderrText.c_str();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT const char*
nlsPortableErrorIdentifier()
{
    return cResult.errorIdentifier.c_str();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT const char*
nlsPortableErrorMessage()
{
    return cResult.errorMessage.c_str();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT int
nlsPortableInterrupted()
{
    return cResult.interrupted ? 1 : 0;
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT void
nlsPortableRequestInterrupt()
{
    cEngine.requestInterrupt();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT void
nlsPortablePump()
{
    // Cooperative clock for the single-threaded runtime: advance due timers and
    // drain the callback/event queues. Driven by a JS setInterval in the worker.
    std::lock_guard<std::mutex> lock(cEngineMutex);
    if (!cEngine.isStarted()) {
        return;
    }
    cEngine.pump();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT void
nlsPortableReset()
{
    std::lock_guard<std::mutex> lock(cEngineMutex);
    cEngine.reset();
    cResult = {};
    cAnalysisResult = "[]";
    cDebuggerResult = "{}";
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT void
nlsPortableStop()
{
    std::lock_guard<std::mutex> lock(cEngineMutex);
    cEngine.stop();
    cResult = {};
    cAnalysisResult = "[]";
}
//=============================================================================
