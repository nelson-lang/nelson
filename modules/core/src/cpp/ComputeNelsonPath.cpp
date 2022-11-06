//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
#include <cstdio>
#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#include <MacTypes.h>
#else
#ifndef _MSC_VER
#include <sys/types.h>
#include <unistd.h>
#endif
#endif
#include "FileSystemWrapper.hpp"
#include "ComputeNelsonBinariesPath.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _MSC_VER
std::wstring
get_basepathW()
{
#define NELSON_ENGINE_DLL L"libnlsEngine.dll"
    std::wstring path;
    wchar_t buffer[MAX_PATH];
    GetModuleFileNameW(GetModuleHandleW(NELSON_ENGINE_DLL), buffer, MAX_PATH);
    path = buffer;
    return path;
}
//=============================================================================
#else
#ifdef __APPLE__
std::string
get_basepathU()
{
    char path[1024];
    CFBundleRef mainBundle = CFBundleGetMainBundle();
    if (!mainBundle) {
        return "";
    }
    CFURLRef mainBundleURL = CFBundleCopyBundleURL(mainBundle);
    if (!mainBundleURL) {
        return "";
    }
    CFStringRef cfStringRef = CFURLCopyFileSystemPath(mainBundleURL, kCFURLPOSIXPathStyle);
    if (!cfStringRef) {
        return "";
    }
    CFStringGetCString(cfStringRef, path, 1024, kCFStringEncodingASCII);
    CFRelease(mainBundleURL);
    CFRelease(cfStringRef);
    return std::string(path);
}
#else // LINUX
std::string
get_basepathU()
{
    std::string path = "";
    pid_t pid = getpid();
    char buf[13];
    sprintf(buf, "%d", pid);
    std::string _link = "/proc/";
    _link.append(buf);
    _link.append("/exe");
    char proc[512];
    int ch = readlink(_link.c_str(), proc, 512);
    if (ch != -1) {
        proc[ch] = 0;
        path = proc;
        std::string::size_type t = path.find_last_of('/');
        path = path.substr(0, t);
    }
    return (path);
}
#endif
#endif
//=============================================================================
bool
ComputeNelsonPath()
{
    std::wstring NelsonPath;
    std::wstring p;
#define NELSON_ROOT_PATH_ENV L"NELSON_ROOT_PATH"
    std::wstring penv = GetVariableEnvironment(NELSON_ROOT_PATH_ENV, L"");
    if (penv != L"") {
        FileSystemWrapper::Path path(penv);
        if (FileSystemWrapper::Path::is_directory(path)) {
            NelsonPath = path.generic_path().generic_wstring();
            NelsonConfiguration::getInstance()->setNelsonRootDirectory(NelsonPath);
            return true;
        }
    }
#ifdef _MSC_VER
    p = get_basepathW();
#else
    p = utf8_to_wstring(get_basepathU());
#endif
    FileSystemWrapper::Path path(p);
    FileSystemWrapper::Path nelsonpath;
#ifdef _MSC_VER
    nelsonpath = path.parent_path().parent_path().parent_path();
#else
    nelsonpath = path.parent_path().parent_path();
#endif
    if (FileSystemWrapper::Path::is_directory(nelsonpath)) {
        NelsonPath = nelsonpath.generic_path().getFinalPathname().generic_wstring();
        NelsonConfiguration::getInstance()->setNelsonRootDirectory(NelsonPath);
        return true;
    }
    fprintf(stderr, "%s\n", _("Error: we cannot find Nelson root path.").c_str());
    NelsonConfiguration::getInstance()->setNelsonRootDirectory(L"");
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
