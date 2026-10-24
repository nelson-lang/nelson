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
#include <cstddef>
#include <string>
//=============================================================================
namespace Nelson {
struct WebUiLaunchOptions;
void*
CreateWebGuiEvaluatorDynamic(void* context, size_t id, const WebUiLaunchOptions& options);
void
StartWebGuiFiguresDynamic(void* evaluator);
bool
WaitWebGuiLaunchedDynamic(std::string& error);
void
ProcessWebGuiEventsDynamic();
void
ShutdownWebGuiDynamic();
//=============================================================================
} // namespace Nelson
//=============================================================================
