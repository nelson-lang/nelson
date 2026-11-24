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
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <shellapi.h>
#include "StartNelsonDynamicFunction.hpp"
#else
#include "StartNelson.h"
#endif
#include "NelsonRunner.h"
#ifdef NELSON_PORTABLE_CLI
#include "PortableRunner.hpp"
#endif
#if defined(__linux__)
#include <cstdlib>
#include <cstring>
//=============================================================================
// A malformed DBUS_SESSION_BUS_ADDRESS (seen on some headless CI runners) makes
// the WebKit web process the webview spawns loop on failed D-Bus connections and
// never return, hanging the caller. A valid address starts with a known
// transport; when it does not, drop it and disable the accessibility bridge that
// relies on the session bus, so the webview degrades like a bus-less session
// instead of stalling. A valid address (a real desktop) is left untouched. This
// runs before any GTK, WebKit or D-Bus code, so every later subsystem and the
// web process it forks inherit the sanitized environment.
static void
sanitizeDBusEnvironment()
{
    const char* address = ::getenv("DBUS_SESSION_BUS_ADDRESS");
    if (address == nullptr || address[0] == '\0') {
        return;
    }
    if (std::strncmp(address, "unix:", 5) == 0 || std::strncmp(address, "tcp:", 4) == 0
        || std::strncmp(address, "nonce-tcp:", 10) == 0
        || std::strncmp(address, "autolaunch:", 11) == 0
        || std::strncmp(address, "unixexec:", 9) == 0) {
        return;
    }
    ::unsetenv("DBUS_SESSION_BUS_ADDRESS");
    if (::getenv("NO_AT_BRIDGE") == nullptr) {
        ::setenv("NO_AT_BRIDGE", "1", 1);
    }
}
#endif
//=============================================================================
int
runNelson(int argc, char* argv[], NELSON_ENGINE_MODE mode)
{
#if defined(__linux__)
    sanitizeDBusEnvironment();
#endif
#ifdef NELSON_PORTABLE_CLI
#ifdef _MSC_VER
    int argCount = 0;
    LPWSTR* argumentList = CommandLineToArgvW(GetCommandLineW(), &argCount);
    const int status = runPortableNelson(argCount, argumentList);
    LocalFree(argumentList);
    return status;
#else
    return runPortableNelson(argc, argv);
#endif
#else
#ifdef _MSC_VER
    int argCount = 0;
    LPWSTR* szArgList = CommandLineToArgvW(GetCommandLineW(), &argCount);
#ifndef _DEBUG
    /* Catch system error messages (release mode only) */
    UINT LastErrorMode
        = SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOALIGNMENTFAULTEXCEPT | SEM_NOGPFAULTERRORBOX);
#endif
    int ierr = Nelson::StartNelsonDynamicFunction(argCount, (wchar_t**)szArgList, mode);
    LocalFree(szArgList);
#else
    int ierr = StartNelsonA(argc, argv, mode);
#endif
    return ierr;
#endif
}
//=============================================================================
