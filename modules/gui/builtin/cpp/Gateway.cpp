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
#include "bannerBuiltin.hpp"
#include "inserthtmlBuiltin.hpp"
#include "qt_verboseBuiltin.hpp"
#include "uigetdirBuiltin.hpp"
#include "lookandfeelBuiltin.hpp"
#include "questdlgBuiltin.hpp"
#include "inputdlgBuiltin.hpp"
#include "listdlgBuiltin.hpp"
#include "uialertBuiltin.hpp"
#include "uiconfirmBuiltin.hpp"
#include "uigetfileBuiltin.hpp"
#include "uiopenBuiltin.hpp"
#include "uiprogressdlgBuiltin.hpp"
#include "uiputfileBuiltin.hpp"
#include "uisaveBuiltin.hpp"
#include "uisetcolorBuiltin.hpp"
#include "uisetfontBuiltin.hpp"
#include "historybrowserBuiltin.hpp"
#include "filebrowserBuiltin.hpp"
#include "workspacebrowserBuiltin.hpp"
#include "openvarBuiltin.hpp"
#include "terminalBuiltin.hpp"
#include "__native_screen_info__Builtin.hpp"
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
    { "uigetdir", (ptrBuiltin)Nelson::GuiGateway::uigetdirBuiltin, 1, -1 },
    { "qt_verbose", (ptrBuiltin)Nelson::GuiGateway::qt_verboseBuiltin, 1, 1 },
    { "questdlg", (ptrBuiltin)Nelson::GuiGateway::questdlgBuiltin, 1, 6 },
    { "inputdlg", (ptrBuiltin)Nelson::GuiGateway::inputdlgBuiltin, 1, 5 },
    { "listdlg", (ptrBuiltin)Nelson::GuiGateway::listdlgBuiltin, 2, -1 },
    { "uigetfile", (ptrBuiltin)Nelson::GuiGateway::uigetfileBuiltin, 3, -1 },
    { "uiputfile", (ptrBuiltin)Nelson::GuiGateway::uiputfileBuiltin, 3, -1 },
    { "uisetcolor", (ptrBuiltin)Nelson::GuiGateway::uisetcolorBuiltin, 1, -1 },
    { "uisetfont", (ptrBuiltin)Nelson::GuiGateway::uisetfontBuiltin, 1, -1 },
    { "uialert", (ptrBuiltin)Nelson::GuiGateway::uialertBuiltin, 0, -4 },
    { "uiconfirm", (ptrBuiltin)Nelson::GuiGateway::uiconfirmBuiltin, 1, -4 },
    { "uiprogressdlg", (ptrBuiltin)Nelson::GuiGateway::uiprogressdlgBuiltin, 1, -2 },
    { "uiopen", (ptrBuiltin)Nelson::GuiGateway::uiopenBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "uisave", (ptrBuiltin)Nelson::GuiGateway::uisaveBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "commandhistory", (ptrBuiltin)Nelson::GuiGateway::historybrowserBuiltin, 0, 2 },
    { "filebrowser", (ptrBuiltin)Nelson::GuiGateway::filebrowserBuiltin, 0, 2 },
    { "workspace", (ptrBuiltin)Nelson::GuiGateway::workspacebrowserBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "openvar", (ptrBuiltin)Nelson::GuiGateway::openvarBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "__terminal__", (ptrBuiltin)Nelson::GuiGateway::terminalBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__native_screen_info__", (ptrBuiltin)Nelson::GuiGateway::__native_screen_info__Builtin, 1,
        0 },

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
NLS_GATEWAY_FUNC_EXTENDED(gateway, (void*)initializeGuiModule)
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishGuiModule)
//=============================================================================
