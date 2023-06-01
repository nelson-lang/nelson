//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
#include "ComHandleObject.hpp"
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
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "isvalid"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_isvalidBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "actxserver", (ptrBuiltin)Nelson::ComEngineGateway::actxserverBuiltin, 1, -2 },
    { "actxGetRunningServer", (ptrBuiltin)Nelson::ComEngineGateway::actxGetRunningServerBuiltin, 1,
        1 },
    { "actxcontrollist", (ptrBuiltin)Nelson::ComEngineGateway::actxcontrollistBuiltin, 1, 0 },
    { "actxserverlist", (ptrBuiltin)Nelson::ComEngineGateway::actxserverlistBuiltin, 1, 0 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "fieldnames"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_fieldnamesBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "methods"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_methodsBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "used"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_usedBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_deleteBuiltin, 0, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "isprop"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_ispropBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "ismethod"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_ismethodBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "class"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_classBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_getBuiltin, 1, 2 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "set"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_setBuiltin, 1, 3 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "invoke"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_invokeBuiltin, 1, 2 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_COM_CATEGORY_STR, "range"),
        (ptrBuiltin)Nelson::ComEngineGateway::COM_rangeBuiltin, 1, -1 },
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
