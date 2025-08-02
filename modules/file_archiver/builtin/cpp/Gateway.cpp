//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "NelsonGateway.hpp"
#include "zipBuiltin.hpp"
#include "unzipBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"file_archiver";
//=============================================================================
static const nlsGateway gateway[] = {
    { "zip", (ptrBuiltin)Nelson::FileArchiverGateway::zipBuiltin, -1, -3 },
    { "unzip", (ptrBuiltin)Nelson::FileArchiverGateway::unzipBuiltin, -1, -2 },
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
