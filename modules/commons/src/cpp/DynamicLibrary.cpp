//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DynamicLibrary.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifndef _MSC_VER
static int
get_dlopen_flag(const std::string& library_name)
{
    // MPI needs to load library as DLD_GLOBAL
    if (library_name.find("nlsMpi") != std::string::npos) {
        return RTLD_NOW | RTLD_GLOBAL;
    }
    return RTLD_NOW | RTLD_LOCAL;
}
#endif
//=============================================================================

library_handle
load_dynamic_library(const std::string& library_name)
{
#ifdef _MSC_VER
    library_handle hl;
    try {
        hl = LoadLibraryA(library_name.c_str());
    } catch (const std::runtime_error&) {
        hl = nullptr;
    }
    return hl;
#else
    library_handle hl;
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
#ifdef _MSC_VER
    return std::string(".dll");
#else
#ifdef __APPLE__
    return std::string(".dylib");
#else
    return std::string(".so");
#endif
#endif
}
//=============================================================================
std::wstring
get_dynamic_library_extensionW()
{
#ifdef _MSC_VER
    return std::wstring(L".dll");
#else
#ifdef __APPLE__
    return std::wstring(L".dylib");
#else
    return std::wstring(L".so");
#endif
#endif
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
} // namespace Nelson
//=============================================================================
