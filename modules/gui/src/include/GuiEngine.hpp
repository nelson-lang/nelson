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
#include "nlsGui_exports.h"
#include <cstddef>
#include <string>
//=============================================================================
namespace Nelson {
struct WebUiLaunchOptions;
// The caller retains context ownership unless evaluator creation succeeds.
extern "C" NLSGUI_IMPEXP void*
NelsonGuiCreateEvaluator(void* context, size_t id, const WebUiLaunchOptions& options);
extern "C" NLSGUI_IMPEXP void
NelsonGuiStartFigures(void* evaluator);
// Notebook kernel: creates an evaluator whose Interface is the stdio frame
// terminal, starts a headless figure RPC server routed to the kernel's channel,
// and starts the kernel reader thread. The caller runs the standard REPL, which
// pulls cell code from the terminal. Takes ownership of context on success.
extern "C" NLSGUI_IMPEXP void*
NelsonGuiCreateNotebookKernelEvaluator(void* context, size_t id);
// Debug Adapter server (nelson-adv-cli --dap): creates an evaluator whose
// Interface is the DAP terminal, starts a headless figure RPC server for the
// debugger.* dispatch, and starts the DAP reader thread over stdio. The caller
// runs the standard REPL, which pulls the launched program and debugger commands
// from the terminal. Takes ownership of context on success.
extern "C" NLSGUI_IMPEXP void*
NelsonGuiCreateDapEvaluator(void* context, size_t id);
extern "C" NLSGUI_IMPEXP bool
NelsonGuiWaitUntilLaunched(std::string& error);
extern "C" NLSGUI_IMPEXP void
NelsonGuiPumpEvents();
extern "C" NLSGUI_IMPEXP void
NelsonGuiShutdown();
//=============================================================================
// Open a Notebook document in the Qt Notebook window; an empty path is a new document.
extern "C" NLSGUI_IMPEXP bool
NelsonGuiOpenNotebook(void* evaluator, const wchar_t* path);
extern "C" NLSGUI_IMPEXP void
NelsonGuiCloseNotebookHost();
//=============================================================================
} // namespace Nelson
//=============================================================================
