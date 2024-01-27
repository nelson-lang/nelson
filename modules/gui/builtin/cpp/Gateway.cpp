//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "bannerBuiltin.hpp"
#include "inserthtmlBuiltin.hpp"
#include "qt_verboseBuiltin.hpp"
#include "uigetdirBuiltin.hpp"
#include "lookandfeelBuiltin.hpp"
#include "msgboxBuiltin.hpp"
#include "questdlgBuiltin.hpp"
#include "historybrowserBuiltin.hpp"
#include "filebrowserBuiltin.hpp"
#include "workspacebrowserBuiltin.hpp"
//=============================================================================
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
#include "HistoryBrowser.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"gui";
//=============================================================================
static const nlsGateway gateway[] = {
    { "banner", (ptrBuiltin)Nelson::GuiGateway::bannerBuiltin, 0, 0, CPP_BUILTIN_WITH_EVALUATOR },
    { "inserthtml", (ptrBuiltin)Nelson::GuiGateway::inserthtmlBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lookandfeel", (ptrBuiltin)Nelson::GuiGateway::lookandfeelBuiltin, 1, 2 },
    { "uigetdir", (ptrBuiltin)Nelson::GuiGateway::uigetdirBuiltin, 1, 2 },
    { "qt_verbose", (ptrBuiltin)Nelson::GuiGateway::qt_verboseBuiltin, 1, 1 },
    { "msgbox", (ptrBuiltin)Nelson::GuiGateway::msgboxBuiltin, 1, 4 },
    { "questdlg", (ptrBuiltin)Nelson::GuiGateway::questdlgBuiltin, 1, 6 },
    { "commandhistory", (ptrBuiltin)Nelson::GuiGateway::historybrowserBuiltin, 0, 2 },
    { "filebrowser", (ptrBuiltin)Nelson::GuiGateway::filebrowserBuiltin, 0, 2 },
    { "workspace", (ptrBuiltin)Nelson::GuiGateway::workspacebrowserBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
static bool
initializeGuiModule(Nelson::Evaluator* eval)
{
    auto engineMode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    if (engineMode == GUI) {
        HistoryBrowser::synchronizeHistoryBrowser();
    }
    return true;
}
//=============================================================================
static bool
finishGuiModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeGuiModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishGuiModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
