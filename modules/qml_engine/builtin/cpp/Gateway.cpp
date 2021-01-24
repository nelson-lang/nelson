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
#include "QmlEngine.hpp"
//=============================================================================
#include "qml_addimportpathBuiltin.hpp"
#include "qml_addpluginpathBuiltin.hpp"
#include "qml_clearcomponentcacheBuiltin.hpp"
#include "qml_collectgarbageBuiltin.hpp"
#include "qml_createqquickviewBuiltin.hpp"
#include "qml_evaluatefileBuiltin.hpp"
#include "qml_evaluatestringBuiltin.hpp"
#include "qml_importpathlistBuiltin.hpp"
#include "qml_loadfileBuiltin.hpp"
#include "qml_loadstringBuiltin.hpp"
#include "qml_offlinestoragepathBuiltin.hpp"
#include "qml_pluginpathlistBuiltin.hpp"
#include "qml_setofflinestoragepathBuiltin.hpp"
//=============================================================================
#include "QObject_classnameBuiltin.hpp"
#include "QObject_deleteBuiltin.hpp"
#include "QObject_dispBuiltin.hpp"
#include "QObject_fieldnamesBuiltin.hpp"
#include "QObject_findchildrenBuiltin.hpp"
#include "QObject_getBuiltin.hpp"
#include "QObject_invokeBuiltin.hpp"
#include "QObject_ismethodBuiltin.hpp"
#include "QObject_ispropBuiltin.hpp"
#include "QObject_isvalidBuiltin.hpp"
#include "QObject_iswidgettypeBuiltin.hpp"
#include "QObject_iswindowtypeBuiltin.hpp"
#include "QObject_methodsBuiltin.hpp"
#include "QObject_methodsignatureBuiltin.hpp"
#include "QObject_propertiesBuiltin.hpp"
#include "QObject_rootBuiltin.hpp"
#include "QObject_setBuiltin.hpp"
#include "QObject_undefineBuiltin.hpp"
#include "QObject_usedBuiltin.hpp"
#include "qt_versionBuiltin.hpp"
#include "qt_constantBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"qml_engine";
//=============================================================================
static const nlsGateway gateway[] = {
    { "QObject_isvalid", (void*)Nelson::QmlEngineGateway::QObject_isvalidBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "QObject_properties", (void*)Nelson::QmlEngineGateway::QObject_propertiesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "qml_loadfile", (void*)Nelson::QmlEngineGateway::qml_loadfileBuiltin, 1, 1 },
    { "qml_loadstring", (void*)Nelson::QmlEngineGateway::qml_loadstringBuiltin, 1, 1 },
    { "qml_evaluatestring", (void*)Nelson::QmlEngineGateway::qml_evaluatestringBuiltin, 1, 1 },
    { "qml_evaluatefile", (void*)Nelson::QmlEngineGateway::qml_evaluatefileBuiltin, 1, 1 },
    { "qml_clearcomponentcache", (void*)Nelson::QmlEngineGateway::qml_clearcomponentcacheBuiltin, 0,
        0 },
    { "qml_collectgarbage", (void*)Nelson::QmlEngineGateway::qml_collectgarbageBuiltin, 0, 0 },
    { "qml_importpathlist", (void*)Nelson::QmlEngineGateway::qml_importpathlistBuiltin, 1, 0 },
    { "qml_pluginpathlist", (void*)Nelson::QmlEngineGateway::qml_pluginpathlistBuiltin, 1, 0 },
    { "qml_offlinestoragepath", (void*)Nelson::QmlEngineGateway::qml_offlinestoragepathBuiltin, 1,
        0 },
    { "qml_setofflinestoragepath",
        (void*)Nelson::QmlEngineGateway::qml_setofflinestoragepathBuiltin, 0, 1 },
    { "qml_addimportpath", (void*)Nelson::QmlEngineGateway::qml_addimportpathBuiltin, 0, 1 },
    { "qml_addpluginpath", (void*)Nelson::QmlEngineGateway::qml_addpluginpathBuiltin, 0, 1 },
    { "qml_createqquickview", (void*)Nelson::QmlEngineGateway::qml_createqquickviewBuiltin, 1, 1 },
    { "QObject_undefine", (void*)Nelson::QmlEngineGateway::QObject_undefineBuiltin, 0, 2 },
    { "QObject_delete", (void*)Nelson::QmlEngineGateway::QObject_deleteBuiltin, 0, 1 },
    { "QObject_disp", (void*)Nelson::QmlEngineGateway::QObject_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "QObject_get", (void*)Nelson::QmlEngineGateway::QObject_getBuiltin, 1, 2 },
    { "QObject_set", (void*)Nelson::QmlEngineGateway::QObject_setBuiltin, 1, 3 },
    { "QObject_fieldnames", (void*)Nelson::QmlEngineGateway::QObject_fieldnamesBuiltin, 1, 1 },
    { "QObject_methods", (void*)Nelson::QmlEngineGateway::QObject_methodsBuiltin, 1, 1 },
    { "QObject_invoke", (void*)Nelson::QmlEngineGateway::QObject_invokeBuiltin, -1, -2 },
    { "QObject_isprop", (void*)Nelson::QmlEngineGateway::QObject_ispropBuiltin, 1, 2 },
    { "QObject_ismethod", (void*)Nelson::QmlEngineGateway::QObject_ismethodBuiltin, 1, 2 },
    { "QObject_methodsignature", (void*)Nelson::QmlEngineGateway::QObject_methodsignatureBuiltin, 1,
        2 },
    { "QObject_iswidgettype", (void*)Nelson::QmlEngineGateway::QObject_iswidgettypeBuiltin, 1, 1 },
    { "QObject_iswindowtype", (void*)Nelson::QmlEngineGateway::QObject_iswindowtypeBuiltin, 1, 1 },
    { "QObject_classname", (void*)Nelson::QmlEngineGateway::QObject_classnameBuiltin, 1, 1 },
    { "QObject_root", (void*)Nelson::QmlEngineGateway::QObject_rootBuiltin, 1, 0 },
    { "QObject_findchildren", (void*)Nelson::QmlEngineGateway::QObject_findchildrenBuiltin, 1, 2 },
    { "QObject_used", (void*)Nelson::QmlEngineGateway::QObject_usedBuiltin, 1, 0 },
    { "qt_version", (void*)Nelson::QmlEngineGateway::qt_versionBuiltin, 1, 0 },
    { "qt_constant", (void*)Nelson::QmlEngineGateway::qt_constantBuiltin, 1, 1 },

};
//=============================================================================
static bool
finishQmlEngineModule(Nelson::Evaluator* eval)
{
    terminateQmlEngine();
    return true;
}
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishQmlEngineModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
