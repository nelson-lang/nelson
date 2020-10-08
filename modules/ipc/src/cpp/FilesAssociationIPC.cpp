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
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/thread/thread.hpp>
#include "FilesAssociationIPC.hpp"
#include "FilesAssociation.hpp"
#include "StringZLib.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "Sleep.hpp"
//=============================================================================
namespace Nelson {
constexpr auto NELSON_COMMAND_PID = "NELSON_COMMAND_PID";
constexpr auto NELSON_COMMAND_PID_DATA = "NELSON_COMMAND_PID_DATA";
constexpr auto NELSON_COMMAND_MODE_DATA = "NELSON_COMMAND_MODE_DATA";
constexpr auto NELSON_COMMAND_FILE_EXTENSION = "NELSON_COMMAND_FILE_EXTENSION";
//=============================================================================
#define MAX_MSG_SIZE 1000
#define MAX_NB_MSG 100
//=============================================================================
class command_file_extension
{
public:
    command_file_extension(
        const std::string& _commandType, const std::vector<std::wstring>& _filenames)
        : commandType(_commandType), filenames(_filenames){};

    std::string commandType;
    std::vector<std::wstring> filenames;

private:
    friend class boost::serialization::access;

    template <class Archive>
    void
    serialize(Archive& ar, const unsigned int version)
    {
        ar& commandType;
        ar& filenames;
    }
};
//=============================================================================
static bool receiverLoopRunning = false;
//=============================================================================
static boost::thread* server_thread = nullptr;
//=============================================================================
static std::string ipc_channel_name;
//=============================================================================
static volatile bool isMessageQueueReady = false;
static volatile bool isMessageQueueFails = false;
static volatile bool loopTerminated = false;
//=============================================================================
static boost::interprocess::message_queue* messageQueue = nullptr;
//=============================================================================
static std::string
getChannelName(int currentPID)
{
    if (ipc_channel_name.empty()) {
        ipc_channel_name
            = std::string(NELSON_COMMAND_FILE_EXTENSION) + "_" + std::to_string(currentPID);
    }
    return ipc_channel_name;
}
//=============================================================================
static bool
doFileExtensionCommand(const command_file_extension& msg)
{
    bool result = false;
    if (msg.commandType == "open") {
        result = OpenFilesAssociated(NELSON_ENGINE_MODE::GUI, msg.filenames, false);
    } else if (msg.commandType == "load") {
        result = LoadFilesAssociated(NELSON_ENGINE_MODE::GUI, msg.filenames, false);
    } else if (msg.commandType == "run") {
        result = ExecuteFilesAssociated(NELSON_ENGINE_MODE::GUI, msg.filenames, false);
    }
    return result;
}
//=============================================================================
void
createNelsonCommandFileExtensionReceiverThread(int currentPID)
{
    receiverLoopRunning = true;
    try {
        messageQueue = new boost::interprocess::message_queue(boost::interprocess::create_only,
            getChannelName(currentPID).c_str(), MAX_NB_MSG, MAX_MSG_SIZE);
    } catch (std::bad_alloc&) {
        messageQueue = nullptr;
    } catch (boost::interprocess::interprocess_exception&) {
        messageQueue = nullptr;
    }
    if (messageQueue == nullptr) {
        receiverLoopRunning = false;
        isMessageQueueFails = true;
        removeNelsonCommandFileExtensionReceiver(currentPID);
        return;
    }

    command_file_extension msg("", std::vector<std::wstring>());
    isMessageQueueReady = true;
    std::string serialized_compressed_string;
    unsigned int maxMessageSize = messageQueue->get_max_msg_size();
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
        if (messageQueue && readed) {
            if (recvd_size != 0) {
                serialized_compressed_string[recvd_size] = 0;
                bool failed = false;
                std::string decompressed_string
                    = decompressString(serialized_compressed_string, failed);
                serialized_compressed_string.clear();
                if (!failed) {
                    iss << decompressed_string;
                    try {
                        boost::archive::text_iarchive ia(iss);
                        ia >> msg;
                        doFileExtensionCommand(msg);
                    } catch (boost::archive::archive_exception&) {
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
    }
    loopTerminated = true;
}
//=============================================================================
void
createNelsonCommandFileExtensionReceiver(int pid)
{
    try {
        server_thread = new boost::thread(createNelsonCommandFileExtensionReceiverThread, pid);
    } catch (const std::bad_alloc&) {
        server_thread = nullptr;
        receiverLoopRunning = false;
        isMessageQueueFails = true;
        isMessageQueueReady = false;
    }
    if (server_thread) {
        server_thread->detach();
    }
}
//=============================================================================
static void
waitMessageQueueUntilReady()
{
    auto* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    while (!isMessageQueueReady && !isMessageQueueFails) {
        Sleep(eval, .5);
    }
}
//=============================================================================
static void
terminateNelsonCommandFileExtensionReceiver()
{
    if (server_thread) {
        receiverLoopRunning = false;
        server_thread->interrupt();
        server_thread = nullptr;
    }
}
//=============================================================================
bool
removeNelsonCommandFileExtensionReceiver(int pid)
{
    waitMessageQueueUntilReady();
    terminateNelsonCommandFileExtensionReceiver();
    bool res = boost::interprocess::message_queue::remove(getChannelName(pid).c_str());
    int l = 0;
    auto* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    while (!loopTerminated && l < 20) {
        Sleep(eval, .5);
        l++;
    }
    if (messageQueue) {
        delete messageQueue;
        messageQueue = nullptr;
    }
    return res;
}
//=============================================================================
bool
sendCommandToFileExtensionReceiver(
    int pidDestination, const std::string& commandType, const std::vector<std::wstring>& filenames)
{
    command_file_extension msg(commandType, filenames);
    std::stringstream oss;
    boost::archive::text_oarchive oa(oss);
    oa << msg;
    bool fails = false;
    std::string serialized_compressed_string(compressString(oss.str(), fails));
    if (fails) {
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
}
//=============================================================================
