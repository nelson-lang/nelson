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
#include "nlsNflow_gui_builtin_exports.h"
#include <string>
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// NFlow models and scripts as examples catalog groups (JSON array), in the
// schema of the webview examples catalog: one group per category, built-in
// categories first, external block libraries after.
NLSNFLOW_GUI_BUILTIN_IMPEXP std::string
nflowExampleGroupsJson();
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
// Examples provider entry point resolved by the webview module (registered
// from etc/startup.m), see ExamplesCatalogHost.hpp.
extern "C" NLSNFLOW_GUI_BUILTIN_IMPEXP int
nlsNFlowExampleGroupsJson(
    const char* locale, void (*sink)(void* context, const char* groupsJson), void* context);
//=============================================================================
