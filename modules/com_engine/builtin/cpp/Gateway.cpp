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
#include "COM_classBuiltin.hpp"
#include "COM_deleteBuiltin.hpp"
#include "COM_displayBuiltin.hpp"
#include "COM_fieldnamesBuiltin.hpp"
#include "COM_getBuiltin.hpp"
#include "COM_invokeBuiltin.hpp"
#include "COM_ismethodBuiltin.hpp"
#include "COM_ispropBuiltin.hpp"
#include "COM_isvalidBuiltin.hpp"
#include "COM_methodsBuiltin.hpp"
#include "COM_rangeBuiltin.hpp"
#include "COM_setBuiltin.hpp"
#include "COM_usedBuiltin.hpp"
#include "ComEngine.hpp"
#include "NelsonGateway.hpp"
#include "actxGetRunningServerBuiltin.hpp"
#include "actxcontrollistBuiltin.hpp"
#include "actxserverBuiltin.hpp"
#include "actxserverlistBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"com_engine";
//=============================================================================
static const nlsGateway gateway[] = {
    { "COM_isvalid", (void*)Nelson::ComEngineGateway::COM_isvalidBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "actxserver", (void*)Nelson::ComEngineGateway::actxserverBuiltin, 1, -2 },
    { "actxGetRunningServer", (void*)Nelson::ComEngineGateway::actxGetRunningServerBuiltin, 1, 1 },
    { "actxcontrollist", (void*)Nelson::ComEngineGateway::actxcontrollistBuiltin, 1, 0 },
    { "actxserverlist", (void*)Nelson::ComEngineGateway::actxserverlistBuiltin, 1, 0 },
    { "COM_display", (void*)Nelson::ComEngineGateway::COM_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "COM_fieldnames", (void*)Nelson::ComEngineGateway::COM_fieldnamesBuiltin, 1, 1 },
    { "COM_methods", (void*)Nelson::ComEngineGateway::COM_methodsBuiltin, 1, 1 },
    { "COM_used", (void*)Nelson::ComEngineGateway::COM_usedBuiltin, 1, 1 },
    { "COM_delete", (void*)Nelson::ComEngineGateway::COM_deleteBuiltin, 0, 1 },
    { "COM_isprop", (void*)Nelson::ComEngineGateway::COM_ispropBuiltin, 1, 1 },
    { "COM_ismethod", (void*)Nelson::ComEngineGateway::COM_ismethodBuiltin, 1, 1 },
    { "COM_class", (void*)Nelson::ComEngineGateway::COM_classBuiltin, 1, 1 },
    { "COM_get", (void*)Nelson::ComEngineGateway::COM_getBuiltin, 1, 2 },
    { "COM_set", (void*)Nelson::ComEngineGateway::COM_setBuiltin, 1, 3 },
    { "COM_invoke", (void*)Nelson::ComEngineGateway::COM_invokeBuiltin, 1, 2 },
    { "COM_range", (void*)Nelson::ComEngineGateway::COM_rangeBuiltin, 1, -1 },
};
//=============================================================================
static bool
initializeComModule(Nelson::Evaluator* eval)
{
    ComEngine::getInstance()->create();
    return true;
}
//=============================================================================
static bool
finishComModule(Nelson::Evaluator* eval)
{
    ComEngine::getInstance()->finish();
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeComModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishComModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
