//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "OverloadName.hpp"
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
#include "QObject_displayBuiltin.hpp"
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
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "set"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_setBuiltin, 1, 3, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "fieldnames"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "methods"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_methodsBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "invoke"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_invokeBuiltin, -1, -2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "isprop"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "ismethod"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_ismethodBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "isvalid"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_isvalidBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_QOBJECT_CATEGORY_STR, "properties"),
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_propertiesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { "qml_loadfile", (ptrBuiltin)Nelson::QmlEngineGateway::qml_loadfileBuiltin, 1, 1 },
    { "qml_loadstring", (ptrBuiltin)Nelson::QmlEngineGateway::qml_loadstringBuiltin, 1, 1 },
    { "qml_evaluatestring", (ptrBuiltin)Nelson::QmlEngineGateway::qml_evaluatestringBuiltin, 1, 1 },
    { "qml_evaluatefile", (ptrBuiltin)Nelson::QmlEngineGateway::qml_evaluatefileBuiltin, 1, 1 },
    { "qml_clearcomponentcache",
        (ptrBuiltin)Nelson::QmlEngineGateway::qml_clearcomponentcacheBuiltin, 0, 0 },
    { "qml_collectgarbage", (ptrBuiltin)Nelson::QmlEngineGateway::qml_collectgarbageBuiltin, 0, 0 },
    { "qml_importpathlist", (ptrBuiltin)Nelson::QmlEngineGateway::qml_importpathlistBuiltin, 1, 0 },
    { "qml_pluginpathlist", (ptrBuiltin)Nelson::QmlEngineGateway::qml_pluginpathlistBuiltin, 1, 0 },
    { "qml_offlinestoragepath", (ptrBuiltin)Nelson::QmlEngineGateway::qml_offlinestoragepathBuiltin,
        1, 0 },
    { "qml_setofflinestoragepath",
        (ptrBuiltin)Nelson::QmlEngineGateway::qml_setofflinestoragepathBuiltin, 0, 1 },
    { "qml_addimportpath", (ptrBuiltin)Nelson::QmlEngineGateway::qml_addimportpathBuiltin, 0, 1 },
    { "qml_addpluginpath", (ptrBuiltin)Nelson::QmlEngineGateway::qml_addpluginpathBuiltin, 0, 1 },
    { "qml_createqquickview", (ptrBuiltin)Nelson::QmlEngineGateway::qml_createqquickviewBuiltin, 1,
        1 },
    { "QObject_undefine", (ptrBuiltin)Nelson::QmlEngineGateway::QObject_undefineBuiltin, 0, 2 },
    { "QObject_methodsignature",
        (ptrBuiltin)Nelson::QmlEngineGateway::QObject_methodsignatureBuiltin, 1, 2 },
    { "QObject_iswidgettype", (ptrBuiltin)Nelson::QmlEngineGateway::QObject_iswidgettypeBuiltin, 1,
        1 },
    { "QObject_iswindowtype", (ptrBuiltin)Nelson::QmlEngineGateway::QObject_iswindowtypeBuiltin, 1,
        1 },
    { "QObject_classname", (ptrBuiltin)Nelson::QmlEngineGateway::QObject_classnameBuiltin, 1, 1 },
    { "QObject_root", (ptrBuiltin)Nelson::QmlEngineGateway::QObject_rootBuiltin, 1, 0 },
    { "QObject_findchildren", (ptrBuiltin)Nelson::QmlEngineGateway::QObject_findchildrenBuiltin, 1,
        2 },
    { "QObject_used", (ptrBuiltin)Nelson::QmlEngineGateway::QObject_usedBuiltin, 1, 0 },
    { "qt_version", (ptrBuiltin)Nelson::QmlEngineGateway::qt_versionBuiltin, 1, 0 },
    { "qt_constant", (ptrBuiltin)Nelson::QmlEngineGateway::qt_constantBuiltin, 1, 1 },
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
