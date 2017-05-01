//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "qml_loadfileBuiltin.hpp"
#include "handle_QML_deleteBuiltin.hpp"
#include "handle_QML_dispBuiltin.hpp"
#include "handle_QML_getBuiltin.hpp"
#include "handle_QML_setBuiltin.hpp"
#include "handle_QML_fieldnamesBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"qml_engine";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "qml_loadfile", Nelson::QmlEngineGateway::qml_loadfileBuiltin, 1, 1 },
	{ "handle_QML_delete", Nelson::QmlEngineGateway::handle_QML_deleteBuiltin, 0, 1 },
	{ "handle_QML_disp", Nelson::QmlEngineGateway::handle_QML_dispBuiltin, 0, 1 },
	{ "handle_QML_get", Nelson::QmlEngineGateway::handle_QML_getBuiltin, 1, 2 },
	{ "handle_QML_set", Nelson::QmlEngineGateway::handle_QML_setBuiltin, 1, 3 },
	{ "handle_QML_fieldnames", Nelson::QmlEngineGateway::handle_QML_fieldnamesBuiltin, 1, 1 },
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
