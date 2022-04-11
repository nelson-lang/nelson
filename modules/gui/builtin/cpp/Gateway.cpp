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
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
