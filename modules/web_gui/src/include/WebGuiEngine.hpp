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
#include "nlsWeb_gui_exports.h"
#include <cstddef>
#include <string>
//=============================================================================
namespace Nelson {
struct WebUiLaunchOptions;
// The caller retains context ownership unless evaluator creation succeeds.
extern "C" NLSWEB_GUI_IMPEXP void*
NelsonWebGuiCreateEvaluator(void* context, size_t id, const WebUiLaunchOptions& options);
extern "C" NLSWEB_GUI_IMPEXP void
NelsonWebGuiStartFigures(void* evaluator);
extern "C" NLSWEB_GUI_IMPEXP bool
NelsonWebGuiWaitUntilLaunched(std::string& error);
extern "C" NLSWEB_GUI_IMPEXP void
NelsonWebGuiPumpEvents();
extern "C" NLSWEB_GUI_IMPEXP void
NelsonWebGuiShutdown();
//=============================================================================
} // namespace Nelson
//=============================================================================
