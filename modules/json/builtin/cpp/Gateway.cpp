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
#include "jsondecodeBuiltin.hpp"
#include "jsonencodeBuiltin.hpp"
#include "jsonprettyprintBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"json";
//=============================================================================
static const nlsGateway gateway[] = {
    { "jsondecode", (ptrBuiltin)Nelson::JsonGateway::jsondecodeBuiltin, 1, 1 },
    { "jsonencode", (ptrBuiltin)Nelson::JsonGateway::jsonencodeBuiltin, 1, -1 },
    { "jsonprettyprint", (ptrBuiltin)Nelson::JsonGateway::jsonprettyprintBuiltin, 1, 1 },
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
