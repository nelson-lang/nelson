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
#include "addprefBuiltin.hpp"
#include "getprefBuiltin.hpp"
#include "isprefBuiltin.hpp"
#include "preferencesBuiltin.hpp"
#include "rmprefBuiltin.hpp"
#include "setprefBuiltin.hpp"
#include "uigetprefBuiltin.hpp"
#include "uisetprefBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"preferences";
//=============================================================================
static const nlsGateway gateway[] = {
    { "getpref", (ptrBuiltin)Nelson::PreferencesGateway::getprefBuiltin, 1, 3 },
    { "setpref", (ptrBuiltin)Nelson::PreferencesGateway::setprefBuiltin, 0, 3 },
    { "addpref", (ptrBuiltin)Nelson::PreferencesGateway::addprefBuiltin, 0, 3 },
    { "ispref", (ptrBuiltin)Nelson::PreferencesGateway::isprefBuiltin, 1, 2 },
    { "rmpref", (ptrBuiltin)Nelson::PreferencesGateway::rmprefBuiltin, 0, 2 },
    { "uigetpref", (ptrBuiltin)Nelson::PreferencesGateway::uigetprefBuiltin, 2, -5 },
    { "uisetpref", (ptrBuiltin)Nelson::PreferencesGateway::uisetprefBuiltin, 1, 1 },
    { "preferences", (ptrBuiltin)Nelson::PreferencesGateway::preferencesBuiltin, 0, 1 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
