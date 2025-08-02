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
#include "sioemitBuiltin.hpp"
#include "sioregisterBuiltin.hpp"
#include "siounregisterBuiltin.hpp"
#include "SioClientQuit.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"sio_client";
//=============================================================================
static const nlsGateway gateway[]
    = { { "sioemit", (ptrBuiltin)Nelson::SioClientGateway::sioemitBuiltin, 0, -1 },
          { "sioregister", (ptrBuiltin)Nelson::SioClientGateway::sioregisterBuiltin, 0, 2 },
          { "siounregister", (ptrBuiltin)Nelson::SioClientGateway::siounregisterBuiltin, 0, 1 } };
//=============================================================================
static bool
finishSioClientModule(Nelson::Evaluator* eval)
{
    sioquit();
    return true;
}
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishSioClientModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
