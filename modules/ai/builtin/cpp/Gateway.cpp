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
#include "mcpinfoBuiltin.hpp"
#include "mcpserverBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"ai";
//=============================================================================
static const nlsGateway gateway[] = {
    { "mcpserver", (ptrBuiltin)Nelson::AiGateway::mcpserverBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mcpinfo", (ptrBuiltin)Nelson::AiGateway::mcpinfoBuiltin, 1, 0 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
