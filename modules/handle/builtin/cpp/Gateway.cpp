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
//=============================================================================
#include "deleteBuiltin.hpp"
#include "getBuiltin.hpp"
#include "invokeBuiltin.hpp"
#include "ismethodBuiltin.hpp"
#include "ispropBuiltin.hpp"
#include "isvalidBuiltin.hpp"
#include "methodsBuiltin.hpp"
#include "propertiesBuiltin.hpp"
#include "setBuiltin.hpp"
//=============================================================================
#include "handle_eqBuiltin.hpp"
#include "handle_deleteBuiltin.hpp"
#include "handle_displayBuiltin.hpp"
#include "handle_fieldnamesBuiltin.hpp"
#include "handle_getBuiltin.hpp"
#include "handle_invokeBuiltin.hpp"
#include "handle_isequalBuiltin.hpp"
#include "handle_ismethodBuiltin.hpp"
#include "handle_ispropBuiltin.hpp"
#include "handle_isvalidBuiltin.hpp"
#include "handle_methodsBuiltin.hpp"
#include "handle_propertiesBuiltin.hpp"
#include "handle_setBuiltin.hpp"
#include "handle_vertcatBuiltin.hpp"
#include "handle_horzcatBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"handle";
//=============================================================================
static const nlsGateway gateway[] = {
    { "delete", (ptrBuiltin)Nelson::HandleGateway::deleteBuiltin, 0, 1 },
    { "get", (ptrBuiltin)Nelson::HandleGateway::getBuiltin, 1, 1 },
    { "set", (ptrBuiltin)Nelson::HandleGateway::setBuiltin, 1, 1 },
    { "invoke", (ptrBuiltin)Nelson::HandleGateway::invokeBuiltin, 1, 2 },
    { "isvalid", (ptrBuiltin)Nelson::HandleGateway::isvalidBuiltin, 1, 1 },
    { "methods", (ptrBuiltin)Nelson::HandleGateway::methodsBuiltin, 1, 1 },
    { "properties", (ptrBuiltin)Nelson::HandleGateway::propertiesBuiltin, 1, 1 },
    { "isprop", (ptrBuiltin)Nelson::HandleGateway::ispropBuiltin, 1, 2 },
    { "ismethod", (ptrBuiltin)Nelson::HandleGateway::ismethodBuiltin, 1, 2 },
    //=============================================================================
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "vertcat"),
        (ptrBuiltin)Nelson::HandleGateway::handle_vertcatBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "horzcat"),
        (ptrBuiltin)Nelson::HandleGateway::handle_horzcatBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "eq"),
        (ptrBuiltin)Nelson::HandleGateway::handle_eqBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isequal"),
        (ptrBuiltin)Nelson::HandleGateway::handle_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isequaln"),
        (ptrBuiltin)Nelson::HandleGateway::handle_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isequalto"),
        (ptrBuiltin)Nelson::HandleGateway::handle_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "get"),
        (ptrBuiltin)Nelson::HandleGateway::handle_getBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "set"),
        (ptrBuiltin)Nelson::HandleGateway::handle_setBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "invoke"),
        (ptrBuiltin)Nelson::HandleGateway::handle_invokeBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "display"),
        (ptrBuiltin)Nelson::HandleGateway::handle_displayBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "disp"),
        (ptrBuiltin)Nelson::HandleGateway::handle_displayBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "delete"),
        (ptrBuiltin)Nelson::HandleGateway::handle_deleteBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isvalid"),
        (ptrBuiltin)Nelson::HandleGateway::handle_isvalidBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "fieldnames"),
        (ptrBuiltin)Nelson::HandleGateway::handle_fieldnamesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "methods"),
        (ptrBuiltin)Nelson::HandleGateway::handle_methodsBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "ismethod"),
        (ptrBuiltin)Nelson::HandleGateway::handle_ismethodBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isprop"),
        (ptrBuiltin)Nelson::HandleGateway::handle_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "properties"),
        (ptrBuiltin)Nelson::HandleGateway::handle_propertiesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
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
