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
#include "getavailablelanguagesBuiltin.hpp"
#include "getdefaultlanguageBuiltin.hpp"
#include "getlanguageBuiltin.hpp"
#include "setlanguageBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"localization";
//=============================================================================
static const nlsGateway gateway[] = {
    { "getavailablelanguages",
        (ptrBuiltin)Nelson::LocalizationGateway::getavailablelanguagesBuiltin, 1, 0 },
    { "setlanguage", (ptrBuiltin)Nelson::LocalizationGateway::setlanguageBuiltin, 1, 1 },
    { "getlanguage", (ptrBuiltin)Nelson::LocalizationGateway::getlanguageBuiltin, 1, 0 },
    { "getdefaultlanguage", (ptrBuiltin)Nelson::LocalizationGateway::getdefaultlanguageBuiltin, 1,
        0 },
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
