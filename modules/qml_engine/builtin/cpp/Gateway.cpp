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
#include "qml_loadstringBuiltin.hpp"
#include "qml_evaluatefileBuiltin.hpp"
#include "qml_evaluatestringBuiltin.hpp"
#include "qml_undefineBuiltin.hpp"
#include "QObject_deleteBuiltin.hpp"
#include "QObject_dispBuiltin.hpp"
#include "QObject_getBuiltin.hpp"
#include "QObject_setBuiltin.hpp"
#include "QObject_fieldnamesBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"qml_engine";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "qml_loadfile", Nelson::QmlEngineGateway::qml_loadfileBuiltin, 1, 1 },
	{ "qml_loadstring", Nelson::QmlEngineGateway::qml_loadstringBuiltin, 1, 1 },
	{ "qml_evaluatestring", Nelson::QmlEngineGateway::qml_evaluatestringBuiltin, 0, 1 },
	{ "qml_evaluatefile", Nelson::QmlEngineGateway::qml_evaluatefileBuiltin, 0, 1 },
	{ "qml_undefine", Nelson::QmlEngineGateway::qml_undefineBuiltin, 0, 1 },

	{ "QObject_delete", Nelson::QmlEngineGateway::QObject_deleteBuiltin, 0, 1 },
    { "QObject_disp", Nelson::QmlEngineGateway::QObject_dispBuiltin, 0, 1 },
    { "QObject_get", Nelson::QmlEngineGateway::QObject_getBuiltin, 1, 2 },
    { "QObject_set", Nelson::QmlEngineGateway::QObject_setBuiltin, 1, 3 },
    { "QObject_fieldnames", Nelson::QmlEngineGateway::QObject_fieldnamesBuiltin, 1, 1 },
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
