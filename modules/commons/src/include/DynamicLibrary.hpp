//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <stdexcept>
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <dlfcn.h>
#endif
#include "nlsCommons_exports.h"
//=============================================================================
namespace Nelson {
#ifdef _MSC_VER
using library_handle = HMODULE;
using generic_function_ptr = FARPROC;
#else
typedef void* library_handle;
using generic_function_ptr = void*;
#endif
//=============================================================================
NLSCOMMONS_IMPEXP library_handle
load_dynamic_library(const std::string& library_name);
//=============================================================================
NLSCOMMONS_IMPEXP library_handle
load_dynamic_libraryW(const std::wstring& library_name);
//=============================================================================
NLSCOMMONS_IMPEXP generic_function_ptr
get_function(library_handle handle, const std::string& function_name);
//=============================================================================
NLSCOMMONS_IMPEXP bool
close_dynamic_library(library_handle handle);
//=============================================================================
NLSCOMMONS_IMPEXP std::string
get_dynamic_library_extension();
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
get_dynamic_library_extensionW();
//=============================================================================
NLSCOMMONS_IMPEXP std::string
get_dynamic_library_error();
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
get_dynamic_library_errorW();
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
get_dynamic_library_pathW(const std::wstring& libraryNameWithExtension);
//=============================================================================
} // namespace Nelson
//=============================================================================
