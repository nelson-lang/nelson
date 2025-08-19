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
#include "OverloadName.hpp"
#include "imrotateBuiltin.hpp"
#include "imresizeBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"image_processing";
//=============================================================================
static const nlsGateway gateway[] = {
    { "imrotate", (ptrBuiltin)Nelson::ImageProcessingGateway::imrotateBuiltin, 1, 2, CPP_BUILTIN },
    { "imresize", (ptrBuiltin)Nelson::ImageProcessingGateway::imresizeBuiltin, 1, 2, CPP_BUILTIN },
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
