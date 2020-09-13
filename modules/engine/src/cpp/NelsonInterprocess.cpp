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
#define EIGEN_NO_DEBUG
//=============================================================================
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/thread/thread.hpp>
#include "NelsonInterprocess.hpp"
#include "NelsonPIDs.hpp"
#include "characters_encoding.hpp"
#include "PostCommand.hpp"
#include "MainEvaluator.hpp"
#include "StringZLib.hpp"
#include "SparseConstructors.hpp"
#include "ArrayOfSerialization.hpp"
#include "Warning.hpp"
#include "Sleep.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr auto NELSON_COMMAND_INTERPROCESS = "NELSON_COMMAND_INTERPROCESS";
constexpr auto OFF_MSG_SIZE = sizeof(double) * 16 * 1024;
constexpr auto MAX_MSG_SIZE = sizeof(double) * (4096 * 4096) + OFF_MSG_SIZE;
constexpr auto MAX_NB_MSG = 4;
//=============================================================================
static bool receiverLoopRunning = false;
//=============================================================================
static boost::thread* receiver_thread = nullptr;
//=============================================================================
static volatile bool isMessageQueueReady = false;
//=============================================================================
static volatile bool isVarAnswer = false;
static volatile bool isVarAnswerAvailable = false;
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
class dataInterProcessToExchange
{
    //=============================================================================
public:
    //=============================================================================
    dataInterProcessToExchange(int _pid, const std::string& _commandType, const ArrayOf& data)
        : pid(_pid), commandType(_commandType), variable(data) {};
    //=============================================================================
    dataInterProcessToExchange(int _pid, const std::string& _commandType, bool value)
        : pid(_pid), commandType(_commandType), valueAnswer(value) {};

    //=============================================================================
    dataInterProcessToExchange(const std::string& _lineToEvaluate)
        : commandType("eval")
        , lineToEvaluate(_lineToEvaluate)
        , variable(ArrayOf())
        , variableName("")
        , scope("") {};
    //=============================================================================
    dataInterProcessToExchange(
        const std::string& _variableName, const std::string& _scope, const ArrayOf& data)
        : commandType("put"), variable(data), variableName(_variableName), scope(_scope) {};
    //=============================================================================
    dataInterProcessToExchange(int _pid, const std::string& _commandType,
        const std::string& _variableName, const std::string& _scope)
        : pid(_pid), commandType(_commandType), variableName(_variableName), scope(_scope) {};
    //=============================================================================
    ArrayOfSerialization variable;
    int pid = 0;
    bool valueAnswer = false;
    std::string commandType;
    std::string lineToEvaluate;
    std::string variableName;
    std::string scope;
    //=============================================================================
    bool
    isFullySerialized()
    {
        if (commandType == "eval") {
            return true;
        } else if (commandType == "put") {
            return variable.isFullySerialized();
        } else if (commandType == "isvar") {
            return true;
        } else if (commandType == "isvar_answer") {
            return true;
        } else if (commandType == "get") {
            return true;
        } else if (commandType == "get_answer") {
            return variable.isFullySerialized();
        }
        return false;
    }
    //=============================================================================
    void
    clear()
    {
        valueAnswer = false;
        pid = 0;
        variable.clear();
        commandType.clear();
        lineToEvaluate.clear();
        variableName.clear();
        scope.clear();
    }
    //=============================================================================
private:
    //=============================================================================
    friend class boost::serialization::access;
    //=============================================================================
    template <class Archive>
    void
    serialize(Archive& ar, const unsigned int version)
    {
        ar& commandType;
        if (commandType == "eval") {
            ar& lineToEvaluate;
        }
        if (commandType == "isvar") {
            ar& pid;
            ar& variableName;
            ar& scope;
        }
        if (commandType == "put") {
            ar& variable;
            ar& variableName;
            ar& scope;
        }
        if (commandType == "isvar_answer") {
            ar& valueAnswer;
        }
        if (commandType == "get") {
            ar& pid;
            ar& variableName;
            ar& scope;
        }
        if (commandType == "get_answer") {
            ar& variable;
        }
    }
    //=============================================================================
};
//=============================================================================
static std::string
getChannelName(int currentPID)
{
    return std::string(NELSON_COMMAND_INTERPROCESS) + "_" + std::to_string(currentPID);
}
//=============================================================================
static void
createNelsonInterprocessReceiverThread(int currentPID)
{
    receiverLoopRunning = true;
    boost::interprocess::message_queue::remove(getChannelName(currentPID).c_str());
    try {
        boost::interprocess::message_queue messages(boost::interprocess::create_only,
            getChannelName(currentPID).c_str(), MAX_NB_MSG, MAX_MSG_SIZE);
        dataInterProcessToExchange msg("");
        isMessageQueueReady = true;
        while (receiverLoopRunning) {
            unsigned int priority = 0;
            size_t recvd_size = 0;
            std::string serialized_compressed_string;
            serialized_compressed_string.resize(MAX_MSG_SIZE);
            std::stringstream iss;
            if (messages.try_receive(
                    &serialized_compressed_string[0], MAX_MSG_SIZE, recvd_size, priority)) {
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
                            if (msg.commandType == "eval") {
                                postCommand(utf8_to_wstring(msg.lineToEvaluate));
                            } else if (msg.commandType == "put") {
                                Evaluator* eval = getMainEvaluator();
                                if (eval) {
                                    Context* context = eval->getContext();
                                    Scope* scope = nullptr;
                                    if (msg.scope == "global") {
                                        scope = context->getGlobalScope();
                                    }
                                    if (msg.scope == "base") {
                                        scope = context->getBaseScope();
                                    }
                                    if (msg.scope == "caller") {
                                        scope = context->getCallerScope();
                                    }
                                    if (msg.scope == "local") {
                                        scope = context->getCurrentScope();
                                    }
                                    if (scope != nullptr) {
                                        bool success;
                                        ArrayOf var = msg.variable.get(success);
                                        scope->insertVariable(msg.variableName, var);
                                        msg.clear();
                                    }
                                }
                            } else if (msg.commandType == "isvar") {
                                Evaluator* eval = getMainEvaluator();
                                if (eval) {
                                    Context* context = eval->getContext();
                                    Scope* scope = nullptr;
                                    if (msg.scope == "global") {
                                        scope = context->getGlobalScope();
                                    }
                                    if (msg.scope == "base") {
                                        scope = context->getBaseScope();
                                    }
                                    if (msg.scope == "caller") {
                                        scope = context->getCallerScope();
                                    }
                                    if (msg.scope == "local") {
                                        scope = context->getCurrentScope();
                                    }
                                    if (scope != nullptr) {
                                        bool isVar = scope->isVariable(msg.variableName);
                                        sendIsVarAnswerToNelsonInterprocessReceiver(msg.pid, isVar);
                                        msg.clear();
                                    }
                                }
                            } else if (msg.commandType == "isvar_answer") {
                                isVarAnswer = msg.valueAnswer;
                                isVarAnswerAvailable = true;
                                msg.clear();
                            } else if (msg.commandType == "get") {
                                Evaluator* eval = getMainEvaluator();
                                if (eval) {
                                    Context* context = eval->getContext();
                                    Scope* scope = nullptr;
                                    if (msg.scope == "global") {
                                        scope = context->getGlobalScope();
                                    }
                                    if (msg.scope == "base") {
                                        scope = context->getBaseScope();
                                    }
                                    if (msg.scope == "caller") {
                                        scope = context->getCallerScope();
                                    }
                                    if (msg.scope == "local") {
                                        scope = context->getCurrentScope();
                                    }
                                    if (scope != nullptr) {
                                        ArrayOf result;
                                        bool isVar
                                            = scope->lookupVariable(msg.variableName, result);
                                        sendGetVarAnswerToNelsonInterprocessReceiver(
                                            msg.pid, result);
                                        msg.clear();
                                    }
                                }

                            } else if (msg.commandType == "get_answer") {
                                bool success = false;
                                getVarAnswer = msg.variable.get(success);
                                getVarAnswerAvailable = true;
                                msg.clear();
                            }
                        } catch (boost::archive::archive_exception&) { }
                    }
                }
            }
            try {
                boost::this_thread::sleep(boost::posix_time::milliseconds(500));
            } catch (boost::thread_interrupted&) {
                return;
            }
        }

        receiverLoopRunning = false;
    } catch (boost::interprocess::interprocess_exception&) {
        removeNelsonInterprocessReceiver(currentPID);
        receiverLoopRunning = false;
    }
    removeNelsonInterprocessReceiver(currentPID);
}
//=============================================================================
void
createNelsonInterprocessReceiver(int pid)
{
    try {
        receiver_thread = new boost::thread(createNelsonInterprocessReceiverThread, pid);
    } catch (const std::bad_alloc&) {
        receiver_thread = nullptr;
        receiverLoopRunning = false;
    }
    if (receiver_thread) {
        receiver_thread->detach();
    }
}
//=============================================================================
bool
removeNelsonInterprocessReceiver(int pid)
{
    if (receiver_thread) {
        Evaluator* eval = getMainEvaluator();
        while (!isMessageQueueReady) {
            Sleep(eval, .5);
        }
        receiverLoopRunning = false;
        receiver_thread->interrupt();
        receiver_thread = nullptr;
    }
    return boost::interprocess::message_queue::remove(getChannelName(pid).c_str());
}
//=============================================================================
bool
sendIsVarAnswerToNelsonInterprocessReceiver(int pidDestination, bool isVar)
{
    dataInterProcessToExchange msg(pidDestination, "isvar_answer", isVar);
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        return false;
    }
    bool bSend = false;
    try {
        boost::interprocess::message_queue messages(
            boost::interprocess::open_only, getChannelName(pidDestination).c_str());
        messages.send(serialized_compressed_string.data(), serialized_compressed_string.size(), 0);
        bSend = true;
    } catch (boost::interprocess::interprocess_exception&) {
        bSend = false;
    }
    return bSend;
}
//=============================================================================
bool
sendGetVarAnswerToNelsonInterprocessReceiver(int pidDestination, const ArrayOf& data)
{
    dataInterProcessToExchange msg(pidDestination, "get_answer", data);
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        return false;
    }
    bool bSend = false;
    try {
        boost::interprocess::message_queue messages(
            boost::interprocess::open_only, getChannelName(pidDestination).c_str());
        messages.send(serialized_compressed_string.data(), serialized_compressed_string.size(), 0);
        bSend = true;
    } catch (boost::interprocess::interprocess_exception& e) {
        e;
        bSend = false;
    }
    return bSend;
}
//=============================================================================

bool
sendCommandToNelsonInterprocessReceiver(int pidDestination, const std::wstring& command)
{
    Evaluator* eval = getMainEvaluator();
    while (!isMessageQueueReady) {
        Sleep(eval, 0.5);
    }
    dataInterProcessToExchange msg(wstring_to_utf8(command));
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        return false;
    }
    bool bSend = false;
    try {
        boost::interprocess::message_queue messages(
            boost::interprocess::open_only, getChannelName(pidDestination).c_str());
        messages.send(serialized_compressed_string.data(), serialized_compressed_string.size(), 0);
        bSend = true;
    } catch (boost::interprocess::interprocess_exception&) {
        bSend = false;
    }
    return bSend;
}
//=============================================================================
bool
sendVariableToNelsonInterprocessReceiver(
    int pidDestination, const ArrayOf& var, const std::wstring& name, const std::wstring& scope)
{
    Evaluator* eval = getMainEvaluator();
    while (!isMessageQueueReady) {
        Sleep(eval, .5);
    }
    dataInterProcessToExchange msg(wstring_to_utf8(name), wstring_to_utf8(scope), var);
    if (!msg.isFullySerialized()) {
        Warning(WARNING_NOT_FULLY_SERIALIZED, _W("Variable not fully serialized."));
    }
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    bool bSend = false;
    if (failed) {
        return bSend;
    }
    if (serialized_compressed_string.size() < MAX_MSG_SIZE) {
        try {
            boost::interprocess::message_queue messages(
                boost::interprocess::open_only, getChannelName(pidDestination).c_str());
            messages.send(
                serialized_compressed_string.data(), serialized_compressed_string.size(), 0);
            bSend = true;
        } catch (boost::interprocess::interprocess_exception&) {
            bSend = false;
        }
    }
    return bSend;
}
//=============================================================================
bool
isVariableFromNelsonInterprocessReceiver(
    int pidDestination, const std::wstring& name, const std::wstring& scope)
{
    Evaluator* eval = getMainEvaluator();
    while (!isMessageQueueReady) {
        Sleep(eval, .5);
    }

    dataInterProcessToExchange msg(
        getCurrentPID(), "isvar", wstring_to_utf8(name), wstring_to_utf8(scope));
    if (!msg.isFullySerialized()) {
        Warning(WARNING_NOT_FULLY_SERIALIZED, _W("Variable not fully serialized."));
    }
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        Error(_W("Cannot serialize data."));
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        Error(_W("Serialized data too big."));
    }
    try {
        boost::interprocess::message_queue messages(
            boost::interprocess::open_only, getChannelName(pidDestination).c_str());
        messages.send(serialized_compressed_string.data(), serialized_compressed_string.size(), 0);
    } catch (boost::interprocess::interprocess_exception&) {
        Error(_W("Cannot send serialized data."));
    }
    int l = 0;
    while (!isVarAnswerAvailable && l < 20) {
        Sleep(eval, .5);
        l++;
    }
    if (l == 20) {
        Error("Impossible to get value (Timeout).");
    } else {
        bool isVarExist = isVarAnswer;
        isVarAnswer = false;
        isVarAnswerAvailable = false;
        return isVarExist;
    }
    return false;
}
//=============================================================================
ArrayOf
getVariableFromNelsonInterprocessReceiver(
    int pidDestination, const std::wstring& name, const std::wstring& scope)
{
    Evaluator* eval = getMainEvaluator();
    while (!isMessageQueueReady) {
        Sleep(eval, .5);
    }

    dataInterProcessToExchange msg(
        getCurrentPID(), "get", wstring_to_utf8(name), wstring_to_utf8(scope));
    if (!msg.isFullySerialized()) {
        Warning(WARNING_NOT_FULLY_SERIALIZED, _W("Variable not fully serialized."));
    }
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        Error(_W("Cannot serialize data."));
    }
    if (serialized_compressed_string.size() >= MAX_MSG_SIZE) {
        Error(_W("Serialized data too big."));
    }
    try {
        boost::interprocess::message_queue messages(
            boost::interprocess::open_only, getChannelName(pidDestination).c_str());
        messages.send(serialized_compressed_string.data(), serialized_compressed_string.size(), 0);
    } catch (boost::interprocess::interprocess_exception&) {
        Error(_W("Cannot send serialized data."));
    }
    int l = 0;
    while (!getVarAnswerAvailable && l < 20) {
        Sleep(eval, .5);
        l++;
    }
    ArrayOf result;
    if (l == 20) {
        Error("Impossible to get value (Timeout).");
    } else {
        result = getVarAnswer;
        getVarAnswerAvailable = false;
        getVarAnswer = ArrayOf();
    }
    return result;
}
//=============================================================================
}
//=============================================================================
