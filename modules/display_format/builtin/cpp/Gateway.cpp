//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "NelsonGateway.hpp"
#include "dispBuiltin.hpp"
#include "displayBuiltin.hpp"
#include "formatBuiltin.hpp"
#include "echoBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"display_format";
//=============================================================================
static const nlsGateway gateway[] = {
    { "echo", (ptrBuiltin)Nelson::DisplayFormatGateway::echoBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "format", (ptrBuiltin)Nelson::DisplayFormatGateway::formatBuiltin, 1, 2 },
    { "disp", (ptrBuiltin)Nelson::DisplayFormatGateway::dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "display", (ptrBuiltin)Nelson::DisplayFormatGateway::displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },

};
//=============================================================================
static bool
initializeModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
static bool
finishModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
