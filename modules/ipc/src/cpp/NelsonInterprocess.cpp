//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/thread/thread.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include "StringHelpers.hpp"
#include "NelsonInterprocess.hpp"
#include "NelsonPIDs.hpp"
#include "characters_encoding.hpp"
#include "PostCommandDynamicFunction.hpp"
#include "Warning.hpp"
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
constexpr std::size_t MAX_NB_MSG = 4;
#else
constexpr auto MAX_MSG_SIZE = sizeof(double) * MEGABYTE;
constexpr auto MAX_NB_MSG = 2;
#endif
constexpr auto TIMEOUT_COUNT = 20;
//=============================================================================
static bool receiverLoopRunning = false;
//=============================================================================
static boost::thread* receiver_thread = nullptr;
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
static std::string
getChannelName(int currentPID)
{
    return std::string(NELSON_COMMAND_INTERPROCESS) + "_" + std::to_string(currentPID);
}
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
static boost::interprocess::message_queue* messageQueue = nullptr;
//=============================================================================
static void
createNelsonInterprocessReceiverThread(int currentPID, bool withEventsLoop)
{
    receiverLoopRunning = true;
    isMessageQueueFails = false;
    try {
        std::string channelName = getChannelName(currentPID);
        messageQueue = new boost::interprocess::message_queue(
            boost::interprocess::open_or_create, channelName.c_str(), MAX_NB_MSG, MAX_MSG_SIZE);
    } catch (std::bad_alloc&) {
        messageQueue = nullptr;
    } catch (boost::interprocess::interprocess_exception&) {
        messageQueue = nullptr;
    }
    if (messageQueue == nullptr) {
        receiverLoopRunning = false;
        isMessageQueueFails = true;
        removeNelsonInterprocessReceiver(currentPID, withEventsLoop);
        return;
    }
    openIpcReceiverIsReadyMutex(currentPID);
    dataInterProcessToExchange msg(currentPID, NELSON_INTERPROCESS_COMMAND::POST_COMMAND, true);
    isMessageQueueReady = true;
    std::string serialized_compressed_string;
    unsigned int maxMessageSize = (unsigned int)messageQueue->get_max_msg_size();
    loopTerminated = false;
    while (receiverLoopRunning) {
        unsigned int priority = 0;
        size_t recvd_size = 0;
        std::stringstream iss;
        if (messageQueue == nullptr) {
            receiverLoopRunning = false;
            return;
        }
        serialized_compressed_string.resize(messageQueue->get_max_msg_size());
        bool readed = false;
        try {
            readed = messageQueue->try_receive(
                &serialized_compressed_string[0], maxMessageSize, recvd_size, priority);
        } catch (...) {
            readed = false;
        }
        if (readed && messageQueue) {
            if (recvd_size != 0) {
                serialized_compressed_string[recvd_size] = 0;
                bool failed = false;
                std::string decompressed_string
                    = decompressString(serialized_compressed_string, failed);
                serialized_compressed_string.clear();
                if (!failed) {
                    iss << decompressed_string;
                    try {
                        boost::archive::binary_iarchive ia(iss);
                        ia >> msg;
                        processMessageData(msg);
                    } catch (std::exception&) {
                    }
                }
            }
        }
        try {
            boost::this_thread::sleep_for(boost::chrono::milliseconds(uint64(50)));
        } catch (boost::thread_interrupted&) {
            receiverLoopRunning = false;
            isMessageQueueFails = false;
            loopTerminated = true;
            return;
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
            = new boost::thread(createNelsonInterprocessReceiverThread, pid, withEventsLoop);
    } catch (const std::bad_alloc&) {
        receiver_thread = nullptr;
        receiverLoopRunning = false;
        isMessageQueueFails = true;
        isMessageQueueReady = false;
        return false;
    } catch (const boost::thread_resource_error&) {
        Warning(L"Nelson::ipc:resourcesNotAvailable", _W("IPC resources not available."));
        receiver_thread = nullptr;
        receiverLoopRunning = false;
        isMessageQueueFails = true;
        isMessageQueueReady = false;
        return false;
    }

    if (receiver_thread) {
        receiver_thread->detach();
    }
    return true;
}
//=============================================================================
static void
terminateNelsonInterprocessReceiverThread()
{
    if (receiver_thread) {
        receiverLoopRunning = false;
        receiver_thread->interrupt();
        receiver_thread = nullptr;
    }
}
//=============================================================================
bool
removeNelsonInterprocessReceiver(int pid, bool withEventsLoop)
{
    waitMessageQueueUntilReady(withEventsLoop);
    terminateNelsonInterprocessReceiverThread();
    bool res = boost::interprocess::message_queue::remove(getChannelName(pid).c_str());
    closeIpcReceiverIsReadyMutex(pid);
    int l = 0;
    if (withEventsLoop) {
        auto* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        while (true) {
            if (loopTerminated || l >= TIMEOUT_COUNT) {
                break;
            }
            Sleep(eval, .5);
            l++;
        }
    } else {
        while (true) {
            if (loopTerminated || l >= TIMEOUT_COUNT) {
                break;
            }
            try {
                boost::this_thread::sleep(boost::posix_time::milliseconds(500));
            } catch (boost::thread_interrupted&) {
            }
            l++;
        }
    }
    if (messageQueue) {
        delete messageQueue;
        messageQueue = nullptr;
    }
    return res;
}
//=============================================================================
static bool
sendMessage(int pid, const std::string& message)
{
    bool sent = false;
    try {
        boost::interprocess::message_queue messages(
            boost::interprocess::open_only, getChannelName(pid).c_str());
        messages.send(message.data(), message.size(), 0);
        sent = true;
    } catch (boost::interprocess::interprocess_exception&) {
        sent = false;
    }
    return sent;
}
//=============================================================================
bool
sendEvalAnswerToNelsonInterprocessReceiver(int pidDestination, const std::wstring& content)
{
    dataInterProcessToExchange msg(
        pidDestination, NELSON_INTERPROCESS_COMMAND::EVAL_ANSWER, wstring_to_utf8(content));
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        return false;
    }
    return sendMessage(pidDestination, serialized_compressed_string);
}
//=============================================================================
bool
sendIsMinimizedAnswerToNelsonInterprocessReceiver(int pidDestination, bool isMinimized)
{
    dataInterProcessToExchange msg(
        pidDestination, NELSON_INTERPROCESS_COMMAND::IS_MINIMIZED_ANSWER, isMinimized);
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        return false;
    }
    return sendMessage(pidDestination, serialized_compressed_string);
}
//=============================================================================
bool
sendIsVarAnswerToNelsonInterprocessReceiver(int pidDestination, bool isVar)
{
    dataInterProcessToExchange msg(
        pidDestination, NELSON_INTERPROCESS_COMMAND::IS_VAR_ANSWER, isVar);
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        return false;
    }
    return sendMessage(pidDestination, serialized_compressed_string);
}
//=============================================================================
bool
sendGetVarAnswerToNelsonInterprocessReceiver(int pidDestination, const ArrayOf& data)
{
    bool isFullySerialized = false;
    std::string compressedData = ArrayOfToCompressedString(data, isFullySerialized);
    dataInterProcessToExchange msg(
        pidDestination, NELSON_INTERPROCESS_COMMAND::GET_ANSWER, compressedData, isFullySerialized);
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
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
        errorMessage = _W("Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    dataInterProcessToExchange msg(pidDestination, NELSON_INTERPROCESS_COMMAND::POST_COMMAND,
        wstring_to_utf8(command), wstring_to_utf8(scope));
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
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
        errorMessage = _W("Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    dataInterProcessToExchange msg(
        getCurrentPID(), NELSON_INTERPROCESS_COMMAND::EVAL, wstring_to_utf8(command));
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        errorMessage = _W("Cannot compress data.");
        return false;
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        errorMessage = _W("Serialized data too big.");
        return false;
    }
    if (!sendMessage(pidDestination, serialized_compressed_string)) {
        errorMessage = _W("Cannot send serialized data.");
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
            try {
                boost::this_thread::sleep(boost::posix_time::milliseconds(500));
            } catch (boost::thread_interrupted&) {
            }
        }
    }
    if (!isPIDRunning(pidDestination)) {
        errorMessage = _W("Impossible to get value (PID destination no more valid).");
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
        errorMessage = _W("Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    bool isFullySerialized = false;
    std::string compressedData = ArrayOfToCompressedString(var, isFullySerialized);
    dataInterProcessToExchange msg(
        wstring_to_utf8(name), wstring_to_utf8(scope), compressedData, isFullySerialized);
    if (!msg.isFullySerialized()) {
        Warning(WARNING_NOT_FULLY_SERIALIZED, _W("Variable not fully serialized."));
    }
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_string = oss.str();
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
        errorMessage = _W("Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    bool isFullySerialized = false;
    dataInterProcessToExchange msg(pidDestination, SET_MINIMIZE, minimize);
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_string = oss.str();
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
        errorMessage = _W("Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    dataInterProcessToExchange msg(getCurrentPID(), NELSON_INTERPROCESS_COMMAND::IS_MINIMIZED);
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        errorMessage = _W("Cannot compress data.");
        return false;
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        errorMessage = _W("Serialized data too big.");
        return false;
    }
    if (!sendMessage(pidDestination, serialized_compressed_string)) {
        errorMessage = _W("Cannot send serialized data.");
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
            try {
                boost::this_thread::sleep(boost::posix_time::milliseconds(500));
            } catch (boost::thread_interrupted&) {
            }
        }
    }
    if (!isPIDRunning(pidDestination)) {
        errorMessage = _W("Impossible to get value (PID destination no more valid).");
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
        errorMessage = _W("Impossible to initialize IPC.");
        return false;
    }
    waitMessageQueueUntilReady(withEventsLoop);
    dataInterProcessToExchange msg(getCurrentPID(), NELSON_INTERPROCESS_COMMAND::IS_VAR,
        wstring_to_utf8(name), wstring_to_utf8(scope));
    if (!msg.isFullySerialized()) {
        Warning(WARNING_NOT_FULLY_SERIALIZED, _W("Variable not fully serialized."));
    }
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        errorMessage = _W("Cannot compress data.");
        return false;
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        errorMessage = _W("Serialized data too big.");
        return false;
    }
    if (!sendMessage(pidDestination, serialized_compressed_string)) {
        errorMessage = _W("Cannot send serialized data.");
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
            try {
                boost::this_thread::sleep(boost::posix_time::milliseconds(500));
            } catch (boost::thread_interrupted&) {
            }
        }
    }
    if (!isPIDRunning(pidDestination)) {
        errorMessage = _W("Impossible to get value (PID destination no more valid).");
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
        errorMessage = _W("Impossible to initialize IPC.");
        return {};
    }
    waitMessageQueueUntilReady(withEventsLoop);

    dataInterProcessToExchange msg(getCurrentPID(), NELSON_INTERPROCESS_COMMAND::GET,
        wstring_to_utf8(name), wstring_to_utf8(scope));
    if (!msg.isFullySerialized()) {
        Warning(WARNING_NOT_FULLY_SERIALIZED, _W("Variable not fully serialized."));
    }
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        errorMessage = _W("Cannot serialize data.");
        return {};
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        errorMessage = _W("Serialized data too big.");
        return {};
    }
    if (!sendMessage(pidDestination, serialized_compressed_string)) {
        errorMessage = _W("Cannot send serialized data.");
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
            try {
                boost::this_thread::sleep(boost::posix_time::milliseconds(500));
            } catch (boost::thread_interrupted&) {
            }
            l++;
        }
    }
    ArrayOf result;
    if (l >= 20) {
        errorMessage = _W("Impossible to get value (Timeout).");
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
            try {
                boost::this_thread::sleep(boost::posix_time::milliseconds(500));
            } catch (boost::thread_interrupted&) {
            }
        }
    }
}
//=============================================================================
bool
sendCommandFileExtensionToNelsonInterprocessReceiver(int pidDestination,
    NELSON_INTERPROCESS_COMMAND commandType, const std::vector<std::wstring>& filenames)
{
    dataInterProcessToExchange msg(commandType, wstring_to_utf8(filenames));
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_string = oss.str();
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
}
//=============================================================================
