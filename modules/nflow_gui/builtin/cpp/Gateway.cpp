//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "nflow_guiBuiltin.hpp"
#include "open_systemBuiltin.hpp"
#include "nflow_highlightBuiltin.hpp"
#include "WebDesktopBridge.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"nflow_gui";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__open_system__", (ptrBuiltin)Nelson::NFlowGateway::open_systemBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "nflow_highlight", (ptrBuiltin)Nelson::NFlowGateway::nflow_highlightBuiltin, 0, 1,
        CPP_BUILTIN },
};
//=============================================================================
static void
openNFlowFromBridge(void*, Nelson::Evaluator* eval, const wchar_t* modelPath)
{
    Nelson::ArrayOfVector arguments;
    if (modelPath != nullptr && modelPath[0] != L'\0') {
        arguments << Nelson::ArrayOf::characterArray(std::wstring(modelPath));
    }
    Nelson::NFlowGateway::nflow_guiBuiltin(eval, 0, arguments);
}
//=============================================================================
static bool
initializeNFlowGuiModule(Nelson::Evaluator*)
{
    Nelson::WebDesktopBridge::getInstance()->setWebNFlowOpener(&openNFlowFromBridge, nullptr);
    return true;
}
//=============================================================================
static bool
finishNFlowGuiModule(Nelson::Evaluator* eval)
{
    Nelson::WebDesktopBridge::getInstance()->setWebNFlowOpener(nullptr, nullptr);
    Nelson::NFlowGateway::closeAllNFlowWindows(eval);
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED(gateway, (void*)initializeNFlowGuiModule)
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishNFlowGuiModule)
//=============================================================================
