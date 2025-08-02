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
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"i18n";
//=============================================================================
static const nlsGateway gateway[] = {
    { "gettext", (ptrBuiltin)Nelson::I18nGateway::gettextBuiltin, 1, 1 },
    { "_", (ptrBuiltin)Nelson::I18nGateway::gettextBuiltin, 1, 1 },
    { "i18nHelpers", (ptrBuiltin)Nelson::I18nGateway::i18nHelpersBuiltin, 1, -1 },
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
