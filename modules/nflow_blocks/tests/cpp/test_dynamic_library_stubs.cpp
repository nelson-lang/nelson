//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DynamicLibrary.hpp"
//=============================================================================
namespace Nelson {
library_handle
load_dynamic_library(const std::string&)
{
    return nullptr;
}
//=============================================================================
library_handle
load_dynamic_libraryW(const std::wstring&)
{
    return nullptr;
}
//=============================================================================
generic_function_ptr
get_function(library_handle, const std::string&)
{
    return nullptr;
}
//=============================================================================
bool
close_dynamic_library(library_handle)
{
    return true;
}
//=============================================================================
std::string
get_dynamic_library_extension()
{
#ifdef _WIN32
    return ".dll";
#elif defined(__APPLE__)
    return ".dylib";
#else
    return ".so";
#endif
}
//=============================================================================
std::wstring
get_dynamic_library_extensionW()
{
#ifdef _WIN32
    return L".dll";
#elif defined(__APPLE__)
    return L".dylib";
#else
    return L".so";
#endif
}
//=============================================================================
std::string
get_dynamic_library_error()
{
    return {};
}
//=============================================================================
std::wstring
get_dynamic_library_errorW()
{
    return {};
}
//=============================================================================
std::wstring
get_dynamic_library_pathW(const std::wstring&)
{
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
