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
#include "nmm_guiBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"nmm_gui";
//=============================================================================
static const nlsGateway gateway[] = {
    { "nmm_gui", (ptrBuiltin)Nelson::NmmGuiGateway::nmm_guiBuiltin, 0, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__nmm_gui_notify__", (ptrBuiltin)Nelson::NmmGuiGateway::nmm_gui_notifyBuiltin, 0, 2 },
    { "__nmm_gui_i18n__", (ptrBuiltin)Nelson::NmmGuiGateway::nmm_gui_i18nBuiltin, 1, 1 },
    { "__nmm_gui_updates__", (ptrBuiltin)Nelson::NmmGuiGateway::nmm_gui_updatesBuiltin, 1, 1 },
    { "__nmm_gui_prefs__", (ptrBuiltin)Nelson::NmmGuiGateway::nmm_gui_prefsBuiltin, 1, 1 },
};
//=============================================================================
static bool
finishNmmGuiModule(Nelson::Evaluator* eval)
{
    Nelson::NmmGuiGateway::closeNmmGuiWindow();
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishNmmGuiModule)
//=============================================================================
