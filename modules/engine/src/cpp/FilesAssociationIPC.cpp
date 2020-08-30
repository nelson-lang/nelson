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
#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/thread/thread.hpp>
#include "FilesAssociationIPC.hpp"
#include "FilesAssociation.hpp"
//=============================================================================
namespace Nelson {
#define NELSON_COMMAND_PID "NELSON_COMMAND_PID"
#define NELSON_COMMAND_PID_DATA "NELSON_COMMAND_PID_DATA"
#define NELSON_COMMAND_MODE_DATA "NELSON_COMMAND_MODE_DATA"
#define NELSON_COMMAND_FILE_EXTENSION "NELSON_COMMAND_FILE_EXTENSION"
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
        result = OpenFilesAssociated(NELSON_ENGINE_MODE::GUI, msg.filenames);
    } else if (msg.commandType == "load") {
        result = LoadFilesAssociated(NELSON_ENGINE_MODE::GUI, msg.filenames);
    } else if (msg.commandType == "run") {
        result = ExecuteFilesAssociated(NELSON_ENGINE_MODE::GUI, msg.filenames);
    }
    return result;
}
//=============================================================================
void
createNelsonCommandFileExtensionReceiverThread(int currentPID)
{
    receiverLoopRunning = true;
    try {
        boost::interprocess::message_queue messages(boost::interprocess::create_only,
            getChannelName(currentPID).c_str(), MAX_NB_MSG, MAX_MSG_SIZE);

        unsigned int priority = 0;
        size_t recvd_size = 0;

        while (receiverLoopRunning) {
            command_file_extension msg("", std::vector<std::wstring>());

            std::stringstream iss;
            std::string serialized_string;
            serialized_string.resize(MAX_MSG_SIZE);
            if (messages.try_receive(&serialized_string[0], MAX_MSG_SIZE, recvd_size, priority)) {
                iss << serialized_string;

                boost::archive::text_iarchive ia(iss);
                ia >> msg;
                doFileExtensionCommand(msg);
            }
            try {
                boost::this_thread::sleep(boost::posix_time::milliseconds(500));
            } catch (boost::thread_interrupted&) {
                return;
            }
        }

        receiverLoopRunning = false;

    } catch (boost::interprocess::interprocess_exception&) {
        removeNelsonCommandFileExtensionReceiver(currentPID);
        receiverLoopRunning = false;
    }
    removeNelsonCommandFileExtensionReceiver(currentPID);
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
    }
    if (server_thread) {
        server_thread->detach();
    }
}
//=============================================================================
bool
removeNelsonCommandFileExtensionReceiver(int pid)
{
    if (server_thread) {
        receiverLoopRunning = false;
        server_thread->interrupt();
        delete server_thread;
        server_thread = nullptr;
    }
    return boost::interprocess::message_queue::remove(getChannelName(pid).c_str());
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
    std::string serialized_string(oss.str());
    bool bSend = false;
    try {
        boost::interprocess::message_queue messages(
            boost::interprocess::open_only, getChannelName(pidDestination).c_str());
        messages.send(serialized_string.data(), serialized_string.size(), 0);
        bSend = true;
    } catch (boost::interprocess::interprocess_exception&) {
        bSend = false;
    }
    return bSend;
}
//=============================================================================
}
//=============================================================================
