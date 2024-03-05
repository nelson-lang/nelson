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
#include "OverloadName.hpp"
#include "__line__Builtin.hpp"
#include "__text__Builtin.hpp"
#include "__surf__Builtin.hpp"
#include "__image__Builtin.hpp"
#include "__patch__Builtin.hpp"
#include "__contour__Builtin.hpp"
#include "__view__Builtin.hpp"
#include "__hggroup__Builtin.hpp"
#include "__zoom__Builtin.hpp"
#include "__pan__Builtin.hpp"
#include "__rotate3d__Builtin.hpp"
#include "figureBuiltin.hpp"
#include "graphics_object_displayBuiltin.hpp"
#include "graphics_object_setBuiltin.hpp"
#include "graphics_object_getBuiltin.hpp"
#include "graphics_object_isequalBuiltin.hpp"
#include "graphics_object_propertiesBuiltin.hpp"
#include "graphics_object_eqBuiltin.hpp"
#include "graphics_object_deleteBuiltin.hpp"
#include "isgraphicsBuiltin.hpp"
#include "saveasBuiltin.hpp"
#include "copygraphicsBuiltin.hpp"
#include "grootBuiltin.hpp"
#include "isValidGraphicsPropertyBuiltin.hpp"
#include "graphics_object_ispropBuiltin.hpp"
#include "GOFiguresManager.hpp"
#include "axesBuiltin.hpp"
#include "gcfBuiltin.hpp"
#include "gcaBuiltin.hpp"
#include "closeBuiltin.hpp"
#include "is2DBuiltin.hpp"
#include "refreshBuiltin.hpp"
#include "drawnowBuiltin.hpp"
#include "imreadBuiltin.hpp"
#include "imwriteBuiltin.hpp"
#include "validatecolorBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"graphics";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__line__", (ptrBuiltin)Nelson::GraphicsGateway::__line__Builtin, -1, 1, CPP_BUILTIN },
    { "__text__", (ptrBuiltin)Nelson::GraphicsGateway::__text__Builtin, -1, 1, CPP_BUILTIN },
    { "__surf__", (ptrBuiltin)Nelson::GraphicsGateway::__surf__Builtin, -1, 1, CPP_BUILTIN },
    { "__image__", (ptrBuiltin)Nelson::GraphicsGateway::__image__Builtin, -1, 1, CPP_BUILTIN },
    { "__patch__", (ptrBuiltin)Nelson::GraphicsGateway::__patch__Builtin, -1, 1, CPP_BUILTIN },
    { "__contour__", (ptrBuiltin)Nelson::GraphicsGateway::__contour__Builtin, -1, 1, CPP_BUILTIN },
    { "__hggroup__", (ptrBuiltin)Nelson::GraphicsGateway::__hggroup__Builtin, -1, 1, CPP_BUILTIN },
    { "__view__", (ptrBuiltin)Nelson::GraphicsGateway::__view__Builtin, -1, 4, CPP_BUILTIN },
    { "__zoom__", (ptrBuiltin)Nelson::GraphicsGateway::__zoom__Builtin, -1, 2, CPP_BUILTIN },
    { "__pan__", (ptrBuiltin)Nelson::GraphicsGateway::__pan__Builtin, -1, 2, CPP_BUILTIN },
    { "__rotate3d__", (ptrBuiltin)Nelson::GraphicsGateway::__rotate3d__Builtin, -1, 2,
        CPP_BUILTIN },
    //=============================================================================
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "display"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "disp"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "properties"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_propertiesBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "delete"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "set"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_setBuiltin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "get"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_getBuiltin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "isequal"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "isequaln"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "isequalto"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "isprop"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_GO_HANDLE_STR, "eq"),
        (ptrBuiltin)Nelson::GraphicsGateway::graphics_object_eqBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { "isgraphics", (ptrBuiltin)Nelson::GraphicsGateway::isgraphicsBuiltin, 1, 1, CPP_BUILTIN },
    { "saveas", (ptrBuiltin)Nelson::GraphicsGateway::saveasBuiltin, 0, 2, CPP_BUILTIN },
    { "copygraphics", (ptrBuiltin)Nelson::GraphicsGateway::copygraphicsBuiltin, 0, 1, CPP_BUILTIN },
    { "groot", (ptrBuiltin)Nelson::GraphicsGateway::grootBuiltin, 1, 0, CPP_BUILTIN },
    { "isValidGraphicsProperty",
        (ptrBuiltin)Nelson::GraphicsGateway::isValidGraphicsPropertyBuiltin, 1, 2, CPP_BUILTIN },
    { "axes", (ptrBuiltin)Nelson::GraphicsGateway::axesBuiltin, -1, 1, CPP_BUILTIN },
    { "gcf", (ptrBuiltin)Nelson::GraphicsGateway::gcfBuiltin, -1, 0, CPP_BUILTIN },
    { "gca", (ptrBuiltin)Nelson::GraphicsGateway::gcaBuiltin, -1, 0, CPP_BUILTIN },
    { "close", (ptrBuiltin)Nelson::GraphicsGateway::closeBuiltin, -1, 0, CPP_BUILTIN },
    { "is2D", (ptrBuiltin)Nelson::GraphicsGateway::is2DBuiltin, 1, 1, CPP_BUILTIN },
    { "refresh", (ptrBuiltin)Nelson::GraphicsGateway::refreshBuiltin, 0, 1, CPP_BUILTIN },
    { "drawnow", (ptrBuiltin)Nelson::GraphicsGateway::drawnowBuiltin, 0, 0, CPP_BUILTIN },
    { "imread", (ptrBuiltin)Nelson::GraphicsGateway::imreadBuiltin, -1, 1, CPP_BUILTIN },
    { "imwrite", (ptrBuiltin)Nelson::GraphicsGateway::imwriteBuiltin, 0, -2, CPP_BUILTIN },
    { "validatecolor", (ptrBuiltin)Nelson::GraphicsGateway::validatecolorBuiltin, 1, 2,
        CPP_BUILTIN },
    { "figure", (ptrBuiltin)Nelson::GraphicsGateway::figureBuiltin, 1, 1, CPP_BUILTIN },
};
//=============================================================================
static bool
initializeGraphicsModule(Nelson::Evaluator* eval)
{
    initializeGraphic();
    return true;
}
//=============================================================================
static bool
finishGraphicsModule(Nelson::Evaluator* eval)
{
    shutdownGraphic();
    return true;
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
