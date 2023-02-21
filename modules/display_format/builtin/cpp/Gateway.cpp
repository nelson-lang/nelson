//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
    { "int8_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "int16_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "int32_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "int64_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint8_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint16_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint32_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint64_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "logical_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "char_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "double_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "single_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "struct_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "cell_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "handle_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "string_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sparsedouble_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0,
        2, CPP_BUILTIN_WITH_EVALUATOR },
    { "sparselogical_display", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0,
        2, CPP_BUILTIN_WITH_EVALUATOR },
    { "int8_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "int16_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "int32_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "int64_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint8_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint16_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint32_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint64_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "logical_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "char_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "double_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "single_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "struct_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "cell_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "handle_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "string_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sparsedouble_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sparselogical_disp", (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
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
