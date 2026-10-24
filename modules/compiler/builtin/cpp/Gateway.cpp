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
#include "applicationArchiveBuiltin.hpp"
#include "applicationExecutableBuiltin.hpp"
#include "applicationPlanBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"compiler";
static const nlsGateway gateway[] = {
    { "__nelsonc_archive__", (ptrBuiltin)Nelson::CompilerGateway::applicationArchiveBuiltin, 0,
        -6 },
    { "__nelsonc_executable__", (ptrBuiltin)Nelson::CompilerGateway::applicationExecutableBuiltin,
        1, 4 },
    { "__nelsonc_plan__", (ptrBuiltin)Nelson::CompilerGateway::applicationPlanBuiltin, 1, 5 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
