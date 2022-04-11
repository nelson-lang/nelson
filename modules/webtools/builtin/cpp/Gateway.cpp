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
#include "webRESTBuiltin.hpp"
#include "repoBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"webtools";
//=============================================================================
static const nlsGateway gateway[] = {
    { "webREST", (ptrBuiltin)Nelson::WebtoolsGateway::webRESTBuiltin, 1, 5,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "repo", (ptrBuiltin)Nelson::WebtoolsGateway::repoBuiltin, 1, 2 },
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
