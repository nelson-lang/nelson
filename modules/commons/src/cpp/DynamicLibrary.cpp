//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifndef _MSC_VER
#if defined(__APPLE__) || defined(__MACH__)
#import <mach-o/dyld.h>
#else
#include <link.h>
#endif
#endif
#include "DynamicLibrary.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifndef _MSC_VER
static int
get_dlopen_flag(const std::string& library_name)
{
    // MPI, Python needs to load library as DLD_GLOBAL
    if (library_name.find("nlsMpi") != std::string::npos
        || library_name.find("ython") != std::string::npos) {
        return RTLD_NOW | RTLD_GLOBAL;
    }
    return RTLD_NOW | RTLD_LOCAL;
}
#endif
//=============================================================================
library_handle
load_dynamic_library(const std::string& library_name)
{
    library_handle hl;
#ifdef _MSC_VER
    try {
        hl = LoadLibraryA(library_name.c_str());
    } catch (const std::runtime_error&) {
        hl = nullptr;
    }
    return hl;
#else
    try {
        hl = dlopen(library_name.c_str(), get_dlopen_flag(library_name));
    } catch (const std::runtime_error&) {
        hl = nullptr;
    }
    return hl;
#endif
}
//=============================================================================
library_handle
load_dynamic_libraryW(const std::wstring& library_name)
{
#ifdef _MSC_VER
    library_handle hl;
    try {
        hl = LoadLibraryW(library_name.c_str());
    } catch (const std::runtime_error&) {
        hl = nullptr;
    }
    return hl;
#else
    return load_dynamic_library(wstring_to_utf8(library_name));
#endif
}
//=============================================================================
generic_function_ptr
get_function(library_handle handle, const std::string& function_name)
{
#ifdef _MSC_VER
    return GetProcAddress(handle, function_name.c_str());
#else
    return dlsym(handle, function_name.c_str());
#endif
}
//=============================================================================
bool
close_dynamic_library(library_handle handle)
{
#ifdef _MSC_VER
    return FreeLibrary(handle) != 0;
#else
    return dlclose(handle) == 0;
#endif
}
//=============================================================================
std::string
get_dynamic_library_extension()
{
    return std::string(SHARED_LIBRARY_SUFFIX);
}
//=============================================================================
std::wstring
get_dynamic_library_extensionW()
{
    return utf8_to_wstring(get_dynamic_library_extension());
}
//=============================================================================
std::string
get_dynamic_library_error()
{
#ifdef _MSC_VER
    DWORD errorMessageID = ::GetLastError();
    if (errorMessageID == 0) {
        return {};
    }
    LPSTR messageBuffer = nullptr;
    size_t size = FormatMessageA(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        nullptr, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        reinterpret_cast<LPSTR>(&messageBuffer), 0, nullptr);
    std::string message(messageBuffer, size);
    LocalFree(messageBuffer);
    return message;
#else
    std::string res;
    const char* errmsg = dlerror();
    if (errmsg) {
        res = std::string(errmsg);
    }
    return res;
#endif
}
//=============================================================================
std::wstring
get_dynamic_library_errorW()
{
    return utf8_to_wstring(get_dynamic_library_error());
}
//=============================================================================
std::wstring
get_dynamic_library_pathW(const std::wstring& libraryNameWithExtension)
{
    std::wstring full_path;
    library_handle hModule = load_dynamic_libraryW(libraryNameWithExtension);
    if (hModule != NULL) {
#ifdef _MSC_VER
#define MAX_PATH_LEN 4096
        wchar_t szFileName[MAX_PATH_LEN];
        if (GetModuleFileName(hModule, szFileName, MAX_PATH_LEN) != 0) {
            Nelson::FileSystemWrapper::Path p(szFileName);
            std::string error;
            Nelson::FileSystemWrapper::Path _p
                = Nelson::FileSystemWrapper::Path::canonical(p, error);
            if (!error.empty()) {
                _p = p;
            }
            Nelson::FileSystemWrapper::Path path = _p.parent_path();
            full_path = path.generic_wstring();
        }
#else
#if defined(__APPLE__) || defined(__MACH__)
        uint32_t count = _dyld_image_count();
        for (uint32_t i = 1; i < count; i++) {
            const char* image_name = _dyld_get_image_name(i);
            void* handle = dlopen(image_name, RTLD_LAZY);
            if (handle == hModule) {
                std::string filename = image_name;
                Nelson::FileSystemWrapper::Path p(filename);
                std::string error;
                Nelson::FileSystemWrapper::Path _p
                    = Nelson::FileSystemWrapper::Path::canonical(p, error);
                if (!error.empty()) {
                    _p = p;
                }
                Nelson::FileSystemWrapper::Path path = _p.parent_path();
                full_path = path.generic_wstring();
                break;
            }
            dlclose(handle);
        }
#else
        struct link_map* lm;
        if (dlinfo(hModule, RTLD_DI_LINKMAP, &lm) == 0) {
            std::string filename = lm->l_name;
            Nelson::FileSystemWrapper::Path p(filename);
            std::string error;
            Nelson::FileSystemWrapper::Path _p
                = Nelson::FileSystemWrapper::Path::canonical(p, error);
            if (!error.empty()) {
                _p = p;
            }
            Nelson::FileSystemWrapper::Path path = _p.parent_path();
            full_path = path.generic_wstring();
        }
#endif
#endif
        close_dynamic_library(hModule);
    }
    return full_path;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
