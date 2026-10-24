//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <atomic>
#include <chrono>
#include <thread>
#include "StringHelpers.hpp"
#include "BinaryStream.hpp"
#include "IpcChannel.hpp"
#include "NelsonInterprocess.hpp"
#include "NelsonPIDs.hpp"
#include "characters_encoding.hpp"
#include "PostCommandDynamicFunction.hpp"
#include "Warning.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "Sleep.hpp"
#include "nlsBuildConfig.h"
#include "DataInterProcessToExchange.hpp"
#include "CompressedStringHelpers.hpp"
#include "StringZLib.hpp"
#include "IpcReadyReceiverNamedMutex.hpp"
#include "FilesAssociation.hpp"
#include "NelsonMinimizedDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr std::size_t KILOBYTE = 1024;
constexpr std::size_t MEGABYTE = 1024 * KILOBYTE;
#if (defined(_LP64) || defined(_WIN64))
constexpr std::size_t OFF_MSG_SIZE = sizeof(double) * 16 * KILOBYTE;
constexpr std::size_t MAX_MSG_SIZE = sizeof(double) * (4 * MEGABYTE) + OFF_MSG_SIZE;
#else
constexpr auto MAX_MSG_SIZE = sizeof(double) * MEGABYTE;
#endif
constexpr auto TIMEOUT_COUNT = 20;
//=============================================================================
// Wire header for an interprocess message. Bump the version on any layout change
// so that a mismatched build rejects the message instead of misreading it.
static constexpr uint32_t MESSAGE_WIRE_MAGIC = 0x53534C4Eu; // 'N','L','S','S'
static constexpr uint16_t MESSAGE_WIRE_VERSION = 1;
//=============================================================================
static std::string
serializeMessage(const dataInterProcessToExchange& msg);
//=============================================================================
static bool
deserializeMessage(const std::string& bytes, dataInterProcessToExchange& msg);
//=============================================================================
static std::atomic<bool> receiverLoopRunning { false };
//=============================================================================
static std::thread* receiver_thread = nullptr;
//=============================================================================
static volatile bool isMessageQueueFails = false;
static volatile bool isMessageQueueReady = false;
static volatile bool loopTerminated = false;
//=============================================================================
static volatile bool isVarAnswer = false;
static volatile bool isVarAnswerAvailable = false;
//=============================================================================
static volatile bool isMinimizedAnswer = false;
static volatile bool isMinimizedAnswerAvailable = false;
//=============================================================================
static volatile bool evalAnswer = false;
static volatile bool evalAnswerAvailable = false;
static std::string evalResult;
//=============================================================================
static ArrayOf getVarAnswer;
static volatile bool getVarAnswerAvailable = false;
//=============================================================================
static bool
sendIsVarAnswerToNelsonInterprocessReceiver(int pidDestination, bool isVar);
//=============================================================================
static bool
sendGetVarAnswerToNelsonInterprocessReceiver(int pidDestination, const ArrayOf& data);
//=============================================================================
static bool
sendIsMinimizedAnswerToNelsonInterprocessReceiver(int pidDestination, bool isMinimized);
//=============================================================================
static Scope*
getScopeFromName(Evaluator* eval, const std::string& name)
{
    Scope* scope = nullptr;
    if (eval != nullptr) {
        Context* context = eval->getContext();
        if (name == "global") {
            scope = context->getGlobalScope();
        }
        if (name == "base") {
            scope = context->getBaseScope();
        }
        if (name == "caller") {
            scope = context->getCallerScope();
        }
        if (name == "local") {
            scope = context->getCurrentScope();
        }
    }
    return scope;
}
//=============================================================================
static bool
processMessageData(const dataInterProcessToExchange& messageData)
{
    bool res = false;
    switch (messageData.commandType) {
    case OPEN_FILES: {
        res = OpenFilesAssociated(
            NELSON_ENGINE_MODE::GUI, utf8_to_wstring(messageData.filenames), false);
    } break;
    case LOAD_FILES: {
        res = LoadFilesAssociated(
            NELSON_ENGINE_MODE::GUI, utf8_to_wstring(messageData.filenames), false);
    } break;
    case RUN_FILES: {
        res = ExecuteFilesAssociated(
            NELSON_ENGINE_MODE::GUI, utf8_to_wstring(messageData.filenames), false);
    } break;
    case OPEN_NFLOW_FILES: {
        res = OpenAssociatedFiles(
            NELSON_ENGINE_MODE::GUI, utf8_to_wstring(messageData.filenames), false);
    } break;
    case POST_COMMAND: {
        std::wstring line = utf8_to_wstring(messageData.content);
        StringHelpers::replace_all(line, L"'", L"''");
        std::wstring command
            = L"evalin('" + utf8_to_wstring(messageData.scope) + L"',' " + line + L"');";
        res = PostCommandDynamicFunction(command);
    } break;
    case EVAL: {
        std::wstring line = utf8_to_wstring(messageData.content);
        std::wstring pidStr = std::to_wstring(messageData.pid);
        StringHelpers::replace_all(line, L"'", L"''");
        std::wstring command = L"ipc(" + pidStr + L", 'eval_answer', '" + line + L"');";
        res = PostCommandDynamicFunction(command);
    } break;
    case EVAL_ANSWER: {
        evalResult = messageData.content;
        evalAnswerAvailable = true;
    } break;
    case PUT: {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        Scope* scope = getScopeFromName(eval, messageData.scope);
        if (scope != nullptr) {
            bool success;
            ArrayOf var
                = CompressedStringToArrayOf(messageData.serializedCompressedVariable, success);
            if (success) {
                res = scope->insertVariable(messageData.variableName, var);
            }
        }
    } break;
    case GET: {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        Scope* scope = getScopeFromName(eval, messageData.scope);
        if (scope != nullptr) {
            ArrayOf result;
            bool isVar = scope->lookupVariable(messageData.variableName, result);
            if (isVar) {
                res = sendGetVarAnswerToNelsonInterprocessReceiver(messageData.pid, result);
            }
        }
    } break;
    case GET_ANSWER: {
        bool success;
        getVarAnswer = CompressedStringToArrayOf(messageData.serializedCompressedVariable, success);
        getVarAnswerAvailable = true;
        res = true;
    } break;
    case IS_VAR: {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        Scope* scope = getScopeFromName(eval, messageData.scope);
        if (scope != nullptr) {
            bool isVar = scope->isVariable(messageData.variableName);
            res = sendIsVarAnswerToNelsonInterprocessReceiver(messageData.pid, isVar);
        }
    } break;
    case IS_VAR_ANSWER: {
        isVarAnswer = messageData.valueAnswer;
        isVarAnswerAvailable = true;
        res = true;
    } break;
    case SET_MINIMIZE: {
        if (NELSON_ENGINE_MODE::GUI == NelsonConfiguration::getInstance()->getNelsonEngineMode()) {
            setNelsonMinimizedDynamicFunction(messageData.valueAnswer);
        }
    } break;
    case IS_MINIMIZED: {
        bool minimized = true;
        if (NELSON_ENGINE_MODE::GUI == NelsonConfiguration::getInstance()->getNelsonEngineMode()) {
            minimized = getNelsonMinimizedDynamicFunction();
        }
        res = sendIsMinimizedAnswerToNelsonInterprocessReceiver(messageData.pid, minimized);

    } break;
    case IS_MINIMIZED_ANSWER: {
        isMinimizedAnswer = messageData.valueAnswer;
        isMinimizedAnswerAvailable = true;
        res = true;
    } break;
    default: {
    } break;
    }
    return res;
}
//=============================================================================
static IpcChannelServer* ipcServer = nullptr;
//=============================================================================
static void
createNelsonInterprocessReceiverThread(int currentPID, bool withEventsLoop)
{
    (void)withEventsLoop;
    receiverLoopRunning = true;
    isMessageQueueFails = false;
    ipcServer = new (std::nothrow) IpcChannelServer();
    if (ipcServer == nullptr || !ipcServer->start(currentPID)) {
        delete ipcServer;
        ipcServer = nullptr;
        receiverLoopRunning = false;
        isMessageQueueFails = true;
        loopTerminated = true;
        return;
    }
    openIpcReceiverIsReadyMutex(currentPID);
    dataInterProcessToExchange msg(currentPID, NELSON_INTERPROCESS_COMMAND::POST_COMMAND, true);
    isMessageQueueReady = true;
    loopTerminated = false;
    std::string received;
    // Blocking receive: the loop ends when requestStop() sends an empty frame.
    while (receiverLoopRunning && ipcServer->receive(received)) {
        if (!received.empty()) {
            // A malformed or unsupported payload (e.g. a sparse variable whose
            // deserialization raises) must not escape this thread: an exception
            // crossing the thread entry function calls std::terminate and aborts
            // the whole process. Drop the offending message and keep serving.
            std::wstring reason;
            try {
                bool failed = false;
                std::string decompressed_string = decompressString(received, failed);
                if (!failed && deserializeMessage(decompressed_string, msg)) {
                    processMessageData(msg);
                }
            } catch (const NelsonException& e) {
                reason = e.what();
            } catch (const std::exception& e) {
                reason = utf8_to_wstring(e.what());
            } catch (...) {
                reason
                    = _W("An unexpected error occurred while processing an interprocess message.");
            }
            if (!reason.empty()) {
                // Emit under its own guard: with warnings promoted to errors,
                // Warning() itself throws, which would again escape the thread.
                try {
                    Warning(L"Nelson:IPC:messageProcessingFailed", reason);
                } catch (...) {
                }
            }
        }
    }
    loopTerminated = true;
}
//=============================================================================
bool
createNelsonInterprocessReceiver(int pid, bool withEventsLoop)
{
    try {
        receiver_thread
            = new std::thread(createNelsonInterprocessReceiverThread, pid, withEventsLoop);
    } catch (const std::system_error&) {
        Warning(L"Nelson:IPC:resourcesNotAvailable", L"IPC resources not available.");
        receiver_thread = nullptr;
        receiverLoopRunning = false;
        isMessageQueueFails = true;
        isMessageQueueReady = false;
        return false;
    } catch (const std::bad_alloc&) {
        receiver_thread = nullptr;
        receiverLoopRunning = false;
        isMessageQueueFails = true;
        isMessageQueueReady = false;
        return false;
    }
    return true;
}
//=============================================================================
bool
removeNelsonInterprocessReceiver(int pid, bool withEventsLoop)
{
    waitMessageQueueUntilReady(withEventsLoop);
    receiverLoopRunning = false;
    if (ipcServer != nullptr) {
        // Wake the blocked receive() so the loop can end.
        ipcServer->requestStop(pid);
    }
    int l = 0;
    if (withEventsLoop) {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        while (!loopTerminated && l < TIMEOUT_COUNT) {
            Sleep(eval, .5);
            l++;
        }
    } else {
        while (!loopTerminated && l < TIMEOUT_COUNT) {
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
            l++;
        }
    }
    if (receiver_thread != nullptr) {
        if (loopTerminated && receiver_thread->joinable()) {
            receiver_thread->join();
            delete receiver_thread;
            receiver_thread = nullptr;
            if (ipcServer != nullptr) {
                ipcServer->close(pid);
                delete ipcServer;
                ipcServer = nullptr;
            }
        } else {
            // Never block shutdown: detach and leak the still-running thread and
            // its endpoint; the OS reclaims both when the process exits.
            if (receiver_thread->joinable()) {
                receiver_thread->detach();
            }
            delete receiver_thread;
            receiver_thread = nullptr;
            ipcServer = nullptr;
        }
    } else if (ipcServer != nullptr) {
        ipcServer->close(pid);
        delete ipcServer;
        ipcServer = nullptr;
    }
    closeIpcReceiverIsReadyMutex(pid);
    return true;
}
//=============================================================================
static bool
sendMessage(int pid, const std::string& message)
{
    return ipcChannelSend(pid, message);
}
//=============================================================================
bool
sendEvalAnswerToNelsonInterprocessReceiver(int pidDestination, const std::wstring& content)
{
    dataInterProcessToExchange msg(
        pidDestination, NELSON_INTERPROCESS_COMMAND::EVAL_ANSWER, wstring_to_utf8(content));
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized_message, failed);
    if (failed) {
        return false;
    }
    return sendMessage(pidDestination, serialized_compressed_string);
}
//=============================================================================
bool
sendIsMinimizedAnswerToNelsonInterprocessReceiver(int pidDestination, bool isMinimized)
{
    if (pidDestination == getCurrentPID()) {
        // Same process: deliver the answer locally. Routing it through the
        // transport would deadlock, since the receiver thread cannot accept its
        // own connection while it is busy sending this reply.
        isMinimizedAnswer = isMinimized;
        isMinimizedAnswerAvailable = true;
        return true;
    }
    dataInterProcessToExchange msg(
        pidDestination, NELSON_INTERPROCESS_COMMAND::IS_MINIMIZED_ANSWER, isMinimized);
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized_message, failed);
    if (failed) {
        return false;
    }
    return sendMessage(pidDestination, serialized_compressed_string);
}
//=============================================================================
bool
sendIsVarAnswerToNelsonInterprocessReceiver(int pidDestination, bool isVar)
{
    if (pidDestination == getCurrentPID()) {
        isVarAnswer = isVar;
        isVarAnswerAvailable = true;
        return true;
    }
    dataInterProcessToExchange msg(
        pidDestination, NELSON_INTERPROCESS_COMMAND::IS_VAR_ANSWER, isVar);
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized_message, failed);
    if (failed) {
        return false;
    }
    return sendMessage(pidDestination, serialized_compressed_string);
}
//=============================================================================
bool
sendGetVarAnswerToNelsonInterprocessReceiver(int pidDestination, const ArrayOf& data)
{
    if (pidDestination == getCurrentPID()) {
        getVarAnswer = data;
        getVarAnswerAvailable = true;
        return true;
    }
    bool isFullySerialized = false;
    std::string compressedData = ArrayOfToCompressedString(data, isFullySerialized);
    dataInterProcessToExchange msg(
        pidDestination, NELSON_INTERPROCESS_COMMAND::GET_ANSWER, compressedData, isFullySerialized);
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized_message, failed);
    if (failed) {
        return false;
    }
    return sendMessage(pidDestination, serialized_compressed_string);
}
//=============================================================================
bool
postCommandToNelsonInterprocessReceiver(int pidDestination, const std::wstring& command,
    const std::wstring& scope, bool withEventsLoop, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (isMessageQueueFails) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:impossibleInitializeIpc", L"Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    dataInterProcessToExchange msg(pidDestination, NELSON_INTERPROCESS_COMMAND::POST_COMMAND,
        wstring_to_utf8(command), wstring_to_utf8(scope));
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized_message, failed);
    if (failed) {
        return false;
    }
    return sendMessage(pidDestination, serialized_compressed_string);
}
//=============================================================================
bool
evalCommandToNelsonInterprocessReceiver(int pidDestination, const std::wstring& command,
    bool withEventsLoop, std::wstring& result, std::wstring& errorMessage)
{
    result.clear();
    evalAnswer = false;
    evalAnswerAvailable = false;
    if (isMessageQueueFails) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:impossibleInitializeIpc", L"Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    dataInterProcessToExchange msg(
        getCurrentPID(), NELSON_INTERPROCESS_COMMAND::EVAL, wstring_to_utf8(command));
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized_message, failed);
    if (failed) {
        errorMessage
            = resolveMessageTemplate(L"Nelson:ipc:cannotCompressData", L"Cannot compress data.");
        return false;
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:serializedDataTooBig", L"Serialized data too big.");
        return false;
    }
    if (!sendMessage(pidDestination, serialized_compressed_string)) {
        errorMessage = resolveMessageTemplate(
            L"Nelson:ipc:cannotSendSerializedData", L"Cannot send serialized data.");
        return false;
    }
    if (withEventsLoop) {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        while (true) {
            if (evalAnswerAvailable || !isPIDRunning(pidDestination)) {
                break;
            }
            Sleep(eval, .5);
        }
    } else {
        while (true) {
            if (evalAnswerAvailable || !isPIDRunning(pidDestination)) {
                break;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
        }
    }
    if (!isPIDRunning(pidDestination)) {
        errorMessage = Nelson::resolveMessageTemplate(L"Nelson:ipc:impossibleGetPidDestination",
            L"Impossible to get value (PID destination no more valid).");
        return false;
    }
    std::string utf8Result = evalResult;
    evalResult.clear();
    result = Nelson::utf8_to_wstring(utf8Result);
    evalAnswerAvailable = false;
    return true;
}
//=============================================================================
bool
sendVariableToNelsonInterprocessReceiver(int pidDestination, const ArrayOf& var,
    const std::wstring& name, const std::wstring& scope, bool withEventsLoop,
    std::wstring& errorMessage)
{
    if (isMessageQueueFails) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:impossibleInitializeIpc", L"Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    bool isFullySerialized = false;
    std::string compressedData = ArrayOfToCompressedString(var, isFullySerialized);
    dataInterProcessToExchange msg(
        wstring_to_utf8(name), wstring_to_utf8(scope), compressedData, isFullySerialized);
    if (!msg.isFullySerialized()) {
        Warning(WARNING_NOT_FULLY_SERIALIZED, L"Variable not fully serialized.");
    }
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_string = serialized_message;
    std::string serialized_compressed_string = compressString(serialized_string, failed);
    bool bSend = false;
    if (failed) {
        return bSend;
    }
    if (serialized_compressed_string.size() < MAX_MSG_SIZE) {
        bSend = sendMessage(pidDestination, serialized_compressed_string);
    }
    return bSend;
}
//=============================================================================
bool
sendMinimizeToNelsonInterprocessReceiver(
    int pidDestination, bool minimize, bool withEventsLoop, std::wstring& errorMessage)
{
    if (isMessageQueueFails) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:impossibleInitializeIpc", L"Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    bool isFullySerialized = false;
    dataInterProcessToExchange msg(pidDestination, SET_MINIMIZE, minimize);
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_string = serialized_message;
    std::string serialized_compressed_string = compressString(serialized_string, failed);
    bool bSend = false;
    if (failed) {
        return bSend;
    }
    if (serialized_compressed_string.size() < MAX_MSG_SIZE) {
        bSend = sendMessage(pidDestination, serialized_compressed_string);
    }
    return bSend;
}
//=============================================================================
bool
isMinimizedFromNelsonInterprocessReceiver(
    int pidDestination, bool withEventsLoop, std::wstring& errorMessage)
{
    isMinimizedAnswer = false;
    isMinimizedAnswerAvailable = false;
    if (isMessageQueueFails) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:impossibleInitializeIpc", L"Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    dataInterProcessToExchange msg(getCurrentPID(), NELSON_INTERPROCESS_COMMAND::IS_MINIMIZED);
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized_message, failed);
    if (failed) {
        errorMessage
            = resolveMessageTemplate(L"Nelson:ipc:cannotCompressData", L"Cannot compress data.");
        return false;
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:serializedDataTooBig", L"Serialized data too big.");
        return false;
    }
    if (!sendMessage(pidDestination, serialized_compressed_string)) {
        errorMessage = resolveMessageTemplate(
            L"Nelson:ipc:cannotSendSerializedData", L"Cannot send serialized data.");
        return false;
    }
    if (withEventsLoop) {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        while (true) {
            if (isMinimizedAnswerAvailable || !isPIDRunning(pidDestination)) {
                break;
            }
            Sleep(eval, .5);
        }
    } else {
        while (true) {
            if (isMinimizedAnswerAvailable || !isPIDRunning(pidDestination)) {
                break;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
        }
    }
    if (!isPIDRunning(pidDestination)) {
        errorMessage = Nelson::resolveMessageTemplate(L"Nelson:ipc:impossibleGetPidDestination",
            L"Impossible to get value (PID destination no more valid).");
        return false;
    }
    bool isMinimized = isMinimizedAnswer;
    isMinimizedAnswer = false;
    isMinimizedAnswerAvailable = false;
    return isMinimized;
}
//=============================================================================
bool
isVariableFromNelsonInterprocessReceiver(int pidDestination, const std::wstring& name,
    const std::wstring& scope, bool withEventsLoop, std::wstring& errorMessage)
{
    isVarAnswer = false;
    isVarAnswerAvailable = false;
    if (isMessageQueueFails) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:impossibleInitializeIpc", L"Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    dataInterProcessToExchange msg(getCurrentPID(), NELSON_INTERPROCESS_COMMAND::IS_VAR,
        wstring_to_utf8(name), wstring_to_utf8(scope));
    if (!msg.isFullySerialized()) {
        Warning(WARNING_NOT_FULLY_SERIALIZED, L"Variable not fully serialized.");
    }
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized_message, failed);
    if (failed) {
        errorMessage
            = resolveMessageTemplate(L"Nelson:ipc:cannotCompressData", L"Cannot compress data.");
        return false;
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:serializedDataTooBig", L"Serialized data too big.");
        return false;
    }
    if (!sendMessage(pidDestination, serialized_compressed_string)) {
        errorMessage = resolveMessageTemplate(
            L"Nelson:ipc:cannotSendSerializedData", L"Cannot send serialized data.");
        return false;
    }
    if (withEventsLoop) {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        while (true) {
            if (isVarAnswerAvailable || !isPIDRunning(pidDestination)) {
                break;
            }
            Sleep(eval, .5);
        }
    } else {
        while (true) {
            if (isVarAnswerAvailable || !isPIDRunning(pidDestination)) {
                break;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
        }
    }
    if (!isPIDRunning(pidDestination)) {
        errorMessage = Nelson::resolveMessageTemplate(L"Nelson:ipc:impossibleGetPidDestination",
            L"Impossible to get value (PID destination no more valid).");
        return false;
    }
    bool isVarExist = isVarAnswer;
    isVarAnswer = false;
    isVarAnswerAvailable = false;
    return isVarExist;
}
//=============================================================================
ArrayOf
getVariableFromNelsonInterprocessReceiver(int pidDestination, const std::wstring& name,
    const std::wstring& scope, bool withEventsLoop, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (isMessageQueueFails) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:impossibleInitializeIpc", L"Impossible to initialize IPC.");
        return {};
    }
    waitMessageQueueUntilReady(withEventsLoop);

    dataInterProcessToExchange msg(getCurrentPID(), NELSON_INTERPROCESS_COMMAND::GET,
        wstring_to_utf8(name), wstring_to_utf8(scope));
    if (!msg.isFullySerialized()) {
        Warning(WARNING_NOT_FULLY_SERIALIZED, L"Variable not fully serialized.");
    }
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized_message, failed);
    if (failed) {
        errorMessage
            = resolveMessageTemplate(L"Nelson:ipc:cannotSerializeData", L"Cannot serialize data.");
        return {};
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:serializedDataTooBig", L"Serialized data too big.");
        return {};
    }
    if (!sendMessage(pidDestination, serialized_compressed_string)) {
        errorMessage = resolveMessageTemplate(
            L"Nelson:ipc:cannotSendSerializedData", L"Cannot send serialized data.");
        return {};
    }
    int l = 0;
    if (withEventsLoop) {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        while (true) {
            if (getVarAnswerAvailable || l >= TIMEOUT_COUNT) {
                break;
            }
            Sleep(eval, .5);
            l++;
        }
    } else {
        while (true) {
            if (getVarAnswerAvailable || l >= TIMEOUT_COUNT) {
                break;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
            l++;
        }
    }
    ArrayOf result;
    if (l >= 20) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:ipc:impossibleGetTimeout", L"Impossible to get value (Timeout).");
        return {};
    }
    result = getVarAnswer;
    getVarAnswerAvailable = false;
    getVarAnswer = ArrayOf();

    return result;
}
//=============================================================================
void
waitMessageQueueUntilReady(bool withEventsLoop)
{
    if (withEventsLoop) {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        while (true) {
            if (isMessageQueueReady || isMessageQueueFails) {
                break;
            }
            Sleep(eval, .5);
        }
    } else {
        while (true) {
            if (isMessageQueueReady || isMessageQueueFails) {
                break;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
        }
    }
}
//=============================================================================
bool
sendCommandFileExtensionToNelsonInterprocessReceiver(int pidDestination,
    NELSON_INTERPROCESS_COMMAND commandType, const std::vector<std::wstring>& filenames)
{
    dataInterProcessToExchange msg(commandType, wstring_to_utf8(filenames));
    std::string serialized_message = serializeMessage(msg);
    bool failed = false;
    std::string serialized_string = serialized_message;
    std::string serialized_compressed_string = compressString(serialized_string, failed);
    bool bSend = false;
    if (failed) {
        return bSend;
    }
    if (serialized_compressed_string.size() < MAX_MSG_SIZE) {
        bSend = sendMessage(pidDestination, serialized_compressed_string);
    }
    return bSend;
}
//=============================================================================
static std::string
serializeMessage(const dataInterProcessToExchange& msg)
{
    std::string serialized;
    BinaryWriter writer(serialized);
    writer.writePod<uint32_t>(MESSAGE_WIRE_MAGIC);
    writer.writePod<uint16_t>(MESSAGE_WIRE_VERSION);
    msg.write(writer);
    return serialized;
}
//=============================================================================
static bool
deserializeMessage(const std::string& bytes, dataInterProcessToExchange& msg)
{
    BinaryReader reader(bytes);
    uint32_t magic = 0;
    uint16_t version = 0;
    if (!reader.readPod(magic) || !reader.readPod(version) || magic != MESSAGE_WIRE_MAGIC
        || version != MESSAGE_WIRE_VERSION) {
        return false;
    }
    return msg.read(reader);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
