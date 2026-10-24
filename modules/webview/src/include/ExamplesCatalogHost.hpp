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
#include "ExamplesCatalog.hpp"
#include "nlsWebview_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// An examples provider is a native library exporting
//   int symbol(const char* locale, void (*sink)(void* context, const char* groupsJson),
//              void* context)
// which hands the sink a JSON array of catalog groups. Modules register their
// provider from etc/startup.m through __webview__('registerExamplesProvider',
// libraryPath, symbol); the library is loaded on the first catalog build, so a
// lazily loaded module costs nothing until the gallery is opened.
NLSWEBVIEW_IMPEXP void
RegisterExamplesProvider(const std::wstring& libraryPath, const std::string& symbol);
NLSWEBVIEW_IMPEXP bool
UnregisterExamplesProvider(const std::wstring& libraryPath);
//=============================================================================
NLSWEBVIEW_IMPEXP std::string
CurrentExamplesLocale();
// Catalog of every loaded module manifest plus every registered provider.
NLSWEBVIEW_IMPEXP ExamplesCatalog
BuildLoadedModulesExamplesCatalog();
//=============================================================================
} // namespace Nelson
//=============================================================================
