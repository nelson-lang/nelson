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
#if _MSC_VER
#define _WIN32_WINNT 0x0550
#endif
//=============================================================================
#include <string>
#include <stdlib.h>
#include <boost/process.hpp>
#include <boost/thread/thread.hpp>
#include <boost/process/async.hpp>
#include <boost/filesystem.hpp>
#include <algorithm>
#include <map>
#include "engine.h"
#include "characters_encoding.hpp"
#include "NelsonPIDs.hpp"
#include "NelsonInterprocess.hpp"
#include "MxArrayOf.hpp"
#include "IpcReadyReceiverNamedMutex.hpp"
#include "NelsonReadyNamedMutex.hpp"
#include "SystemCommand.hpp"
//=============================================================================
#define NELSON_EXECUTABLE L"nelson-gui"
#define TIMEOUT_SECONDS 20
//=============================================================================
static std::map<int, char*> mapOutputBufferPointer;
static std::map<int, int> mapoutputBufferLength;
//=============================================================================
static int countEngine = 0;
//=============================================================================
static void
exit_handler(boost::process::child& process, int e, std::error_code ec)
{}
//=============================================================================
static boost::process::child*
attach_child(int pid)
{
    boost::process::child* child = nullptr;
    try {
        boost::process::pid_t _pid = (boost::process::pid_t)pid;
        child = new boost::process::child(_pid);
    } catch (const std::bad_alloc&) {
        child = nullptr;
    }
    return child;
}
//=============================================================================
static boost::process::child*
start_child(const std::wstring& executable_name, const std::wstring& arguments)
{
    boost::process::child* child = nullptr;
#ifdef _MSC_VER
    try {
        child = new boost::process::child(boost::process::search_path(executable_name), arguments,
            boost::process::std_out > stdout, boost::process::std_err > stderr,
            boost::process::std_in < stdin);
        child->detach();
    } catch (const std::bad_alloc&) {
        child = nullptr;
    }
#else
#if defined(__APPLE__) || defined(__MACH__)
    std::wstring command = L"open -a \""
        + boost::process::search_path(executable_name).generic_wstring() + L"\"" + L" --args "
        + arguments;
#else
    std::wstring command
        = boost::process::search_path(executable_name).generic_wstring() + L" " + arguments + L" &";
#endif
    int res = 0;
    Nelson::SystemCommand(command, res, false);
    if (res == -1) {
        child = nullptr;
    } else {
        int l = 0;
        int latestNelsonPID = 0;
        while (true) {
            latestNelsonPID = Nelson::getLatestPidWithModeInSharedMemory(NELSON_ENGINE_MODE::GUI);
            if (latestNelsonPID > 0) {
                break;
            }
            if (l >= TIMEOUT_SECONDS) {
                break;
            }
            try {
                boost::this_thread::sleep(boost::posix_time::seconds(1));
                l++;
            } catch (boost::thread_interrupted&) {
            }
        }
        child = attach_child(latestNelsonPID);
    }
#endif
    return child;
}
//=============================================================================
static bool
waitUntilNelsonIsReady(int pid, int n)
{
    int l = 0;
    while (true) {
        if (Nelson::haveIsReadyNelsonMutex(pid)) {
            return true;
        }
        if (l >= n || !Nelson::isPIDRunning(pid)) {
            return false;
        }
        try {
            boost::this_thread::sleep(boost::posix_time::seconds(1));
            l++;
        } catch (boost::thread_interrupted&) {
        }
    }
    return false;
}
//=============================================================================
static bool
waitUntilIpcReceiverIsReady(int pid, int n)
{
    int l = 0;
    while (true) {
        if (Nelson::haveIpcReceiverIsReadyMutex(pid)) {
            return true;
        }
        if (l >= n || !Nelson::isPIDRunning(pid)) {
            return false;
        }
        try {
            boost::this_thread::sleep(boost::posix_time::seconds(1));
            l++;
        } catch (boost::thread_interrupted&) {
        }
    }
    return false;
}
//=============================================================================
Engine*
engOpen(const char* startcmd)
{
    Engine* engine = nullptr;
    int parentPID = Nelson::getCurrentPID();
    if (!Nelson::createNelsonInterprocessReceiver(parentPID, false)) {
        return nullptr;
    }
    if (!waitUntilIpcReceiverIsReady(parentPID, TIMEOUT_SECONDS)) {
        return nullptr;
    }
    boost::process::pid_t latestNelsonPID
        = (boost::process::pid_t)Nelson::getLatestPidWithModeInSharedMemory(
            NELSON_ENGINE_MODE::GUI);

    boost::process::child* child = nullptr;
    bool createChild = false;
    if (latestNelsonPID < 1) {
        std::wstring args(L"--minimize");
        child = start_child(NELSON_EXECUTABLE, args);
        createChild = true;
    } else {
        child = attach_child(latestNelsonPID);
        createChild = false;
    }
    if (child == nullptr) {
        return nullptr;
    }
    if (!child->valid()) {
        if (createChild) {
            delete child;
            child = nullptr;
        }
        return nullptr;
    }
    int childPID = (int)child->id();
    if (!waitUntilNelsonIsReady(childPID, TIMEOUT_SECONDS)) {
        if (createChild) {
            delete child;
            child = nullptr;
        }
        return nullptr;
    }
    if (!waitUntilIpcReceiverIsReady(childPID, TIMEOUT_SECONDS)) {
        if (createChild) {
            delete child;
            child = nullptr;
        }
        return nullptr;
    }
    try {
        engine = new Engine;
        countEngine++;
    } catch (std::bad_alloc&) {
        engine = nullptr;
    }
    if (engine != nullptr) {
        engine->child = (void*)child;
        engine->isSingleUse = createChild;
    }
    return engine;
}
//=============================================================================
Engine*
engOpenSingleUse(const char* startcmd, void* reserved, int* retstatus)
{
    Engine* engine = nullptr;
    *retstatus = 0;
    if (reserved != nullptr) {
        *retstatus = -2;
    }
    int parentPID = Nelson::getCurrentPID();
    if (!Nelson::createNelsonInterprocessReceiver(parentPID, false)) {
        return nullptr;
    }
    if (!waitUntilIpcReceiverIsReady(parentPID, TIMEOUT_SECONDS)) {
        return nullptr;
    }
    std::wstring args;
    boost::process::child* child = start_child(NELSON_EXECUTABLE, args);
    if (child == nullptr) {
        return nullptr;
    }
    int childPID = (int)child->id();
    if (!child->valid()) {
        delete child;
        child = nullptr;
        *retstatus = -3;
        return nullptr;
    }

    if (!waitUntilNelsonIsReady(childPID, TIMEOUT_SECONDS)) {
#ifndef _MSC_VER
        kill(child->id(), SIGKILL);
#endif
        delete child;
        child = nullptr;
        *retstatus = -3;
        return nullptr;
    }
    if (!waitUntilIpcReceiverIsReady(childPID, TIMEOUT_SECONDS)) {
#ifndef _MSC_VER
        kill(child->id(), SIGKILL);
#endif
        delete child;
        child = nullptr;
        *retstatus = -3;
        return nullptr;
    }
    try {
        engine = new Engine;
        countEngine++;
    } catch (std::bad_alloc&) {
        *retstatus = -3;
        engine = nullptr;
    }
    if (engine != nullptr) {
        engine->child = (void*)child;
        engine->isSingleUse = true;
    }
    return engine;
}
//=============================================================================
int
engEvalString(Engine* ep, const char* string)
{
    if (ep == nullptr) {
        return 1;
    }
    boost::process::child* child = (boost::process::child*)(ep->child);
    int childPID = child->id();
    if (!child->valid()) {
        return 1;
    }
    std::wstring command = Nelson::utf8_to_wstring(string);
    std::wstring errorMessage;

    std::wstring result;
    if ((mapOutputBufferPointer[childPID] != nullptr) && (mapoutputBufferLength[childPID] > 0)) {
        mapOutputBufferPointer[childPID][0] = 0;
    }
    bool r = Nelson::evalCommandToNelsonInterprocessReceiver(
        childPID, command, false, result, errorMessage);
    if (errorMessage.empty()) {
        if ((mapOutputBufferPointer[childPID] != nullptr)
            && (mapoutputBufferLength[childPID] > 0)) {
            std::string engineOutputBuffer = Nelson::wstring_to_utf8(result);
            strncpy(mapOutputBufferPointer[childPID], engineOutputBuffer.c_str(),
                mapoutputBufferLength[childPID]);
            mapOutputBufferPointer[childPID][mapoutputBufferLength[childPID]] = 0;
        }
        return 0;
    }
    return 1;
}
//=============================================================================
int
engSetVisible(Engine* ep, bool newVal)
{
    if (ep == nullptr) {
        return 1;
    }
    boost::process::child* child = (boost::process::child*)(ep->child);
    int childPID = child->id();
    if (!child->valid()) {
        return 1;
    }
    std::wstring errorMessage;
    if (Nelson::sendMinimizeToNelsonInterprocessReceiver(childPID, !newVal, false, errorMessage)) {
        return 0;
    }
    return 1;
}
//=============================================================================
int
engGetVisible(Engine* ep, bool* bVal)
{
    if (ep == nullptr) {
        *bVal = false;
        return 1;
    }
    boost::process::child* child = (boost::process::child*)(ep->child);
    int childPID = child->id();
    if (!child->valid()) {
        *bVal = false;
        return 1;
    }
    std::wstring errorMessage;
    bool isminimized
        = Nelson::isMinimizedFromNelsonInterprocessReceiver(childPID, false, errorMessage);
    if (errorMessage.empty()) {
        *bVal = !isminimized;
        return 0;
    }
    *bVal = false;
    return 1;
}
//=============================================================================
int
engClose(Engine* ep)
{
    if (ep == nullptr) {
        return 1;
    }
    if (ep->isSingleUse) {
        boost::process::child* child = (boost::process::child*)ep->child;
        int res = engEvalString(ep, "quit;");
#ifndef _MSC_VER
        if (res) {
            kill(child->id(), SIGKILL);
        }
#endif
        delete child;
    }
    delete ep;
    countEngine--;
    if (countEngine == 0) {
        mapOutputBufferPointer.clear();
        mapoutputBufferLength.clear();
    }
    int parentPID = Nelson::getCurrentPID();
    if (Nelson::removeNelsonInterprocessReceiver(parentPID, false)) {
        return 0;
    }
    return 1;
}
//=============================================================================
mxArray*
engGetVariableCommon(Engine* ep, const char* name, bool interleavedComplex)
{
    if (ep == nullptr) {
        return nullptr;
    }
    std::wstring wname = Nelson::utf8_to_wstring(name);
    boost::process::child* child = (boost::process::child*)(ep->child);
    int childPID = child->id();
    if (!child->valid()) {
        return nullptr;
    }
    bool success = false;
    std::wstring errorMessage;
    Nelson::ArrayOf result = Nelson::getVariableFromNelsonInterprocessReceiver(
        childPID, wname, L"base", false, errorMessage);
    if (errorMessage.empty()) {
        return Nelson::ArrayOfToMxArray(result, interleavedComplex);
    }
    return nullptr;
}
//=============================================================================
mxArray*
engGetVariableInterleavedComplex(Engine* ep, const char* name)
{
    return engGetVariableCommon(ep, name, true);
}
//=============================================================================
mxArray*
engGetVariableSeparatedComplex(Engine* ep, const char* name)
{
    return engGetVariableCommon(ep, name, false);
}
//=============================================================================
int
engPutVariable(Engine* ep, const char* var_name, const mxArray* ap)
{
    if (ep == nullptr) {
        return 1;
    }
    Nelson::ArrayOf var = Nelson::MxArrayToArrayOf(ap);
    std::wstring name = Nelson::utf8_to_wstring(var_name);
    boost::process::child* child = (boost::process::child*)(ep->child);
    int childPID = child->id();
    if (!child->valid()) {
        return 1;
    }
    std::wstring errorMessage;
    if (Nelson::sendVariableToNelsonInterprocessReceiver(
            childPID, var, name, L"base", false, errorMessage)) {
        return 0;
    }
    return 1;
}
//=============================================================================
int
engOutputBuffer(Engine* ep, char* buffer, int buflen)
{
    if (ep == nullptr) {
        return 1;
    }
    boost::process::child* child = (boost::process::child*)(ep->child);
    int childPID = child->id();
    if (!child->valid()) {
        return 1;
    }
    mapOutputBufferPointer[childPID] = buffer;
    mapoutputBufferLength[childPID] = buflen;
    return 0;
}
//=============================================================================
