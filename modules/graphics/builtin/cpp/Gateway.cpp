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
#include "InitializeGraphics.hpp"
#include "figureBuiltin.hpp"
#include "grootBuiltin.hpp"
#include "gcfBuiltin.hpp"
#include "graphic_object_displayBuiltin.hpp"
#include "graphic_object_deleteBuiltin.hpp"
#include "graphic_object_getBuiltin.hpp"
#include "graphic_object_setBuiltin.hpp"
#include "graphic_object_classBuiltin.hpp"
#include "graphic_object_isvalidBuiltin.hpp"
#include "graphic_object_fieldnamesBuiltin.hpp"
#include "graphic_object_horzcat_graphic_objectBuiltin.hpp"
#include "graphic_object_vertcat_graphic_objectBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"graphics";
//=============================================================================
static const nlsGateway gateway[] = {
    { "graphic_object_display", (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_displayBuiltin,
        0, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "graphic_object_disp", (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_displayBuiltin, 0,
        1, CPP_BUILTIN_WITH_EVALUATOR },
    { "figure", (ptrBuiltin)Nelson::GraphicsGateway::figureBuiltin, 1, 2 },
    { "groot", (ptrBuiltin)Nelson::GraphicsGateway::grootBuiltin, 1, 0 },
    { "gcf", (ptrBuiltin)Nelson::GraphicsGateway::gcfBuiltin, 1, 0 },
    { "graphic_object_delete", (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_deleteBuiltin, 0,
        1 },
    { "graphic_object_get", (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_getBuiltin, 1, 2 },
    { "graphic_object_set", (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_setBuiltin, 1, 3 },
    { "graphic_object_class", (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_classBuiltin, 1,
        1 },
    { "graphic_object_isvalid", (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_isvalidBuiltin,
        1, 1 },
    { "graphic_object_fieldnames",
        (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_fieldnamesBuiltin, 1, 1 },
    { "graphic_object_horzcat_graphic_object",
        (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_horzcat_graphic_objectBuiltin, 1, 2 },
    { "graphic_object_vertcat_graphic_object",
        (ptrBuiltin)Nelson::GraphicsGateway::graphic_object_vertcat_graphic_objectBuiltin, 1, 2 },
};
//=============================================================================
static bool
initializeGraphicsModule(Nelson::Evaluator* eval)
{
    return initializeGraphics();
}
//=============================================================================
static bool
finishGraphicsModule(Nelson::Evaluator* eval)
{
    return finishGraphics();
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeGraphicsModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishGraphicsModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
