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
#include "argvBuiltin.hpp"
#include "getnelsonmodeBuiltin.hpp"
#include "getwebmodeBuiltin.hpp"
#include "getweburlBuiltin.hpp"
#include "isquietmodeBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"engine";
//=============================================================================
static const nlsGateway gateway[] = {
    { "getnelsonmode", (ptrBuiltin)Nelson::EngineGateway::getnelsonmodeBuiltin, 1, 0 },
    { "getwebmode", (ptrBuiltin)Nelson::EngineGateway::getwebmodeBuiltin, 1, 0 },
    { "getweburl", (ptrBuiltin)Nelson::EngineGateway::getweburlBuiltin, 2, 0 },
    { "argv", (ptrBuiltin)Nelson::EngineGateway::argvBuiltin, 1, -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isquietmode", (ptrBuiltin)Nelson::EngineGateway::isquietmodeBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR }
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
