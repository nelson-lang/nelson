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
#include "loadmatBuiltin.hpp"
#include "savematBuiltin.hpp"
#include "ismatfileBuiltin.hpp"
#include "whosmatBuiltin.hpp"
#include "whomatBuiltin.hpp"
#include "matfileBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"matio";
//=============================================================================
static const nlsGateway gateway[] = {
    { "savemat", (ptrBuiltin)Nelson::MatioGateway::savematBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "loadmat", (ptrBuiltin)Nelson::MatioGateway::loadmatBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR_CALLER_WORKSPACE_ACCESS },
    { "whosmat", (ptrBuiltin)Nelson::MatioGateway::whosmatBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "whomat", (ptrBuiltin)Nelson::MatioGateway::whomatBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "ismatfile", (ptrBuiltin)Nelson::MatioGateway::ismatfileBuiltin, 1, 1 },
    { "__matfile_read__", (ptrBuiltin)Nelson::MatioGateway::matfileReadBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__matfile_write__", (ptrBuiltin)Nelson::MatioGateway::matfileWriteBuiltin, 0, 4,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__matfile_size__", (ptrBuiltin)Nelson::MatioGateway::matfileSizeBuiltin, 1, 2 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
