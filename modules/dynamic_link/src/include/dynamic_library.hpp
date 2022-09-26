//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
#include <string>
#include <stdexcept>
#ifdef _MSC_VER
#include <Windows.h>
#pragma comment(lib, "kernel32.lib")
#else
#include <dlfcn.h>
#endif
//=============================================================================
namespace Nelson {
#ifdef _MSC_VER
using library_handle = HMODULE;
using generic_function_ptr = FARPROC;
//=============================================================================
inline library_handle
load_dynamic_library(const std::string& library_name)
{
    library_handle hl;
    try {
        hl = LoadLibraryA(library_name.c_str());
    } catch (const std::runtime_error&) {
        hl = nullptr;
    }
    return hl;
}
//=============================================================================
inline library_handle
load_dynamic_libraryW(const std::wstring& library_name)
{
    library_handle hl;
    try {
        hl = LoadLibraryW(library_name.c_str());
    } catch (const std::runtime_error&) {
        hl = nullptr;
    }
    return hl;
}
//=============================================================================
inline generic_function_ptr
get_function(library_handle handle, const std::string& function_name)
{
    return GetProcAddress(handle, function_name.c_str());
}
//=============================================================================
inline bool
close_dynamic_library(library_handle handle)
{
    return FreeLibrary(handle) != 0;
}
//=============================================================================
inline std::string
get_dynamic_library_extension()
{
    return std::string(".dll");
}
//=============================================================================
inline std::wstring
get_dynamic_library_extensionW()
{
    return std::wstring(L".dll");
}
//=============================================================================
inline std::string
get_dynamic_library_error()
{
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
}
//=============================================================================
#else
typedef void* library_handle;
using generic_function_ptr = void*;
//=============================================================================
inline int
get_dlopen_flag(const std::string& library_name)
{
    // MPI needs to load library as DLD_GLOBAL
    if (library_name.find("nlsMpi") != std::string::npos) {
        return RTLD_NOW | RTLD_GLOBAL;
    }
    return RTLD_NOW | RTLD_LOCAL;
}
//=============================================================================
inline library_handle
load_dynamic_library(const std::string& library_name)
{
    library_handle hl;
    try {
        hl = dlopen(library_name.c_str(), get_dlopen_flag(library_name));
    } catch (const std::runtime_error&) {
        hl = nullptr;
    }
    return hl;
}
//=============================================================================
inline generic_function_ptr
get_function(library_handle handle, const std::string& function_name)
{
    return dlsym(handle, function_name.c_str());
}
//=============================================================================
inline bool
close_dynamic_library(library_handle handle)
{
    return dlclose(handle) == 0;
}
//=============================================================================
inline std::string
get_dynamic_library_extension()
{
#ifdef __APPLE__
    return std::string(".dylib");
#else
    return std::string(".so");
#endif
}
//=============================================================================
inline std::string
get_dynamic_library_error()
{
    std::string res;
    const char* errmsg = dlerror();
    if (errmsg) {
        res = std::string(errmsg);
    }
    return res;
}
//=============================================================================
#endif
} // namespace Nelson
//=============================================================================
