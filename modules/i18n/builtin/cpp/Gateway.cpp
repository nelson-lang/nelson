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
#include "gettextBuiltin.hpp"
#include "i18nHelpersBuiltin.hpp"
#include "message_getUnformattedStringBuiltin.hpp"
#include "message_getStringBuiltin.hpp"
#include "OverloadName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"i18n";
//=============================================================================
static const nlsGateway gateway[] = {
    { "gettext", (ptrBuiltin)Nelson::I18nGateway::gettextBuiltin, 1, 1 },
    { "_", (ptrBuiltin)Nelson::I18nGateway::gettextBuiltin, 1, 1 },
    { "i18nHelpers", (ptrBuiltin)Nelson::I18nGateway::i18nHelpersBuiltin, 1, -1 },
    //=============================================================================
    { OVERLOAD_FUNCTION_NAME("message", "getUnformattedString"),
        (ptrBuiltin)Nelson::I18nGateway::message_getUnformattedStringBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("message", "getString"),
        (ptrBuiltin)Nelson::I18nGateway::message_getStringBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },

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
