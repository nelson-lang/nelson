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
#include <functional>
//=============================================================================
namespace webui {
class window;
//=============================================================================
} // namespace webui
//=============================================================================
namespace Nelson::NFlowUiBindings {
//=============================================================================
struct NFlowRpcCallbacks
{
    std::function<std::string(const std::string&)> runSimulation;
    std::function<std::string(const std::string&)> startSimulation;
    std::function<std::string(const std::string&)> pollSimulation;
    std::function<void(bool)> pauseSimulation;
    std::function<void()> stopSimulation;
    std::function<bool()> isSimulationRunning;
    std::function<void(const std::string&, const std::string&)> sendEvent;
    // Import an SSP archive (path) into a runnable .nflow; returns a JSON string
    // {success, path, error}. Marshals the Nelson import to the interpreter thread.
    std::function<std::string(const std::string&)> importSsp;
    // Export a diagram (given as .nflow JSON content) to an SSP at the target
    // path; returns {success, path, error}. Inverse of importSsp; marshals the
    // Nelson export (NFlow.sspExportDiagram) to the interpreter thread.
    std::function<std::string(const std::string&, const std::string&)> exportSsp;
    // Export a diagram (given as .nflow JSON content) to an FMI 3.0 CS FMU at the
    // target path; returns {success, path, error}. Marshals NFlow.exportfmu to
    // the interpreter thread.
    std::function<std::string(const std::string&, const std::string&)> exportFmu;
    // Report the resolved OpenModelica installation as modelicaInfo JSON.
    std::function<std::string()> modelicaInfo;
    std::function<bool(const std::string&, const std::string&, std::string&)> evaluateCallback;
    std::function<bool(const std::string&, const std::string&, std::string&)> openHelp;
    // Launch mode selected for this NFlow window. Kept apart from the live WebUI browser
    // query, which can still be unset while the embedded page loads its first RPC request.
    std::function<bool()> isEmbeddedWebView;
};
//=============================================================================
// modelName binds the window to a live NFlowModelDocument in the registry;
// empty means legacy file mode (read/write modelFilename directly).
void
bindRpc(webui::window* win, const std::string& modelFilename, const std::string& modelName,
    NFlowRpcCallbacks callbacks);
//=============================================================================
} // namespace Nelson::NFlowUiBindings
//=============================================================================
