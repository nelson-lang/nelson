//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
typedef HMODULE library_handle;
typedef FARPROC generic_function_ptr;
//=============================================================================
inline library_handle
load_dynamic_library(std::string library_name)
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
load_dynamic_libraryW(std::wstring library_name)
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
get_function(library_handle handle, std::string function_name)
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
        return std::string();
    }
    LPSTR messageBuffer = nullptr;
    size_t size = FormatMessageA(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&messageBuffer, 0,
        NULL);
    std::string message(messageBuffer, size);
    LocalFree(messageBuffer);
    return message;
}
//=============================================================================
#else
typedef void* library_handle;
typedef void* generic_function_ptr;
//=============================================================================
inline library_handle
load_dynamic_library(std::string library_name)
{
    library_handle hl;
    try {
        hl = dlopen(library_name.c_str(), RTLD_NOW | RTLD_GLOBAL);
    } catch (const std::runtime_error&) {
        hl = nullptr;
    }
    return hl;
}
//=============================================================================
inline generic_function_ptr
get_function(library_handle handle, std::string function_name)
{
    return dlsym(handle, function_name.c_str());
}
//=============================================================================
inline bool
close_shared_library(library_handle handle)
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
