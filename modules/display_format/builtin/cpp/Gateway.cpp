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
    { OVERLOAD_UNARY_NAME(NLS_INT8_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_INT16_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_INT32_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_INT64_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_UINT8_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_UINT16_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_UINT32_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_UINT64_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_LOGICAL_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_CHAR_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_DOUBLE_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_SINGLE_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_STRUCT_ARRAY_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_CELL_ARRAY_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_HANDLE_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_STRING_ARRAY_STR, "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME("sparsedouble", "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME("sparselogical", "display"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_INT8_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_INT16_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_INT32_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_INT64_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_UINT8_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_UINT16_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_UINT32_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_UINT64_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_LOGICAL_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_CHAR_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_DOUBLE_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_SINGLE_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_STRUCT_ARRAY_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_CELL_ARRAY_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_HANDLE_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME(NLS_STRING_ARRAY_STR, "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME("sparsedouble", "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_UNARY_NAME("sparselogical", "disp"),
        (ptrBuiltin)Nelson::DisplayFormatGateway::generic_dispBuiltin, 0, 1,
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
