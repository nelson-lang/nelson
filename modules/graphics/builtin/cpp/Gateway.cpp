//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "InitializeGraphics.hpp"
#include "figureBuiltin.hpp"
#include "grootBuiltin.hpp"
#include "gcfBuiltin.hpp"
#include "graphic_object_dispBuiltin.hpp"
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
    { "graphic_object_display", (void*)Nelson::GraphicsGateway::graphic_object_dispBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "figure", (void*)Nelson::GraphicsGateway::figureBuiltin, 1, 2 },
    { "groot", (void*)Nelson::GraphicsGateway::grootBuiltin, 1, 0 },
    { "gcf", (void*)Nelson::GraphicsGateway::gcfBuiltin, 1, 0 },
    { "graphic_object_delete", (void*)Nelson::GraphicsGateway::graphic_object_deleteBuiltin, 0, 1 },
    { "graphic_object_get", (void*)Nelson::GraphicsGateway::graphic_object_getBuiltin, 1, 2 },
    { "graphic_object_set", (void*)Nelson::GraphicsGateway::graphic_object_setBuiltin, 1, 3 },
    { "graphic_object_class", (void*)Nelson::GraphicsGateway::graphic_object_classBuiltin, 1, 1 },
    { "graphic_object_isvalid", (void*)Nelson::GraphicsGateway::graphic_object_isvalidBuiltin, 1,
        1 },
    { "graphic_object_fieldnames", (void*)Nelson::GraphicsGateway::graphic_object_fieldnamesBuiltin,
        1, 1 },
    { "graphic_object_horzcat_graphic_object",
        (void*)Nelson::GraphicsGateway::graphic_object_horzcat_graphic_objectBuiltin, 1, 2 },
    { "graphic_object_vertcat_graphic_object",
        (void*)Nelson::GraphicsGateway::graphic_object_vertcat_graphic_objectBuiltin, 1, 2 },

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
