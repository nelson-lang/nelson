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
#include "engine.h"
#include "characters_encoding.hpp"
#include "NelsonPIDs.hpp"
#include "NelsonInterprocess.hpp"
#include "MxArrayOf.hpp"
#include "IpcReadyReceiverNamedMutex.hpp"
#include "NelsonReadyNamedMutex.hpp"
//=============================================================================
#ifdef _MSC_VER
#define NELSON_EXECUTABLE L"nelson-gui"
#else
#if defined(__APPLE__) || defined(__MACH__)
#define NELSON_EXECUTABLE L"nelson-gui"
#else
#define NELSON_EXECUTABLE L"nelson-gui-exec"
#endif
#endif
#define TIMEOUT_SECONDS 20
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
        return new boost::process::child(_pid);
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
    std::string command = "open -a \""
        + boost::process::search_path(executable_name).generic_string() + "\"" + " --args "
        + Nelson::wstring_to_utf8(arguments);
  #else
    std::string command = Nelson::wstring_to_utf8(executable_name) + " "
        + Nelson::wstring_to_utf8(arguments) + " &";
  #endif
    int res = system(command.c_str());
    if (res == -1) {
        child = nullptr;
    } else {
        int latestNelsonPID = Nelson::getLatestPidWithModeInSharedMemory(NELSON_ENGINE_MODE::GUI);
        int l = 0;
        while (latestNelsonPID < 1 && l < TIMEOUT_SECONDS) {
            latestNelsonPID = Nelson::getLatestPidWithModeInSharedMemory(NELSON_ENGINE_MODE::GUI);
            if (latestNelsonPID > 0) {
                break;
            }
            try {
                boost::this_thread::sleep(boost::posix_time::seconds(1));
                l++;
            } catch (boost::thread_interrupted&) { }
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
    while (!Nelson::haveIsReadyNelsonMutex(pid) && l < n) {
        try {
            boost::this_thread::sleep(boost::posix_time::seconds(1));
            l++;
        } catch (boost::thread_interrupted&) {
        }
    }
    if (l < n) {
        return true;
    }
    return false;
}
//=============================================================================
static bool
waitUntilIpcReceiverIsReady(int pid, int n)
{
    int l = 0;
    while (!Nelson::haveIpcReceiverIsReadyMutex(pid) && l < n) {
        try {
            boost::this_thread::sleep(boost::posix_time::seconds(1));
            l++;
        } catch (boost::thread_interrupted&) {
        }
    }
    if (l < n) {
        return true;
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
        return nullptr;
    }
    if (!waitUntilIpcReceiverIsReady(childPID, TIMEOUT_SECONDS)) {
#ifndef _MSC_VER
        kill(child->id(), SIGKILL);
#endif
        delete child;
        child = nullptr;
        return nullptr;
    }
    try {
        engine = new Engine;
    } catch (std::bad_alloc&) {
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
    std::wstring command = Nelson::utf8_to_wstring(string);
    std::wstring errorMessage;
    if (Nelson::sendCommandToNelsonInterprocessReceiver(childPID, command, false, errorMessage)) {
        return 0;
    }
    return 1;
}
//=============================================================================
int
engSetVisible(Engine* ep, bool newVal)
{
    return 0;
}
//=============================================================================
int
engGetVisible(Engine* ep, bool* bVal)
{
    return 0;
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
    return 0;
}
//=============================================================================
