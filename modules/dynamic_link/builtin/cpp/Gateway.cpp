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
#include "dlcallBuiltin.hpp"
#include "dlcloseBuiltin.hpp"
#include "dllib_displayBuiltin.hpp"
#include "dllib_fieldnamesBuiltin.hpp"
#include "dllib_getBuiltin.hpp"
#include "dllib_ismethodBuiltin.hpp"
#include "dllib_ispropBuiltin.hpp"
#include "dllib_isvalidBuiltin.hpp"
#include "dllib_usedBuiltin.hpp"
#include "dllibinfoBuiltin.hpp"
#include "dlopenBuiltin.hpp"
#include "dlsymBuiltin.hpp"
#include "dlsym_deleteBuiltin.hpp"
#include "dlsym_displayBuiltin.hpp"
#include "dlsym_fieldnamesBuiltin.hpp"
#include "dlsym_getBuiltin.hpp"
#include "dlsym_ismethodBuiltin.hpp"
#include "dlsym_ispropBuiltin.hpp"
#include "dlsym_isvalidBuiltin.hpp"
#include "dlsym_usedBuiltin.hpp"
#include "getdynlibextBuiltin.hpp"
#include "isNullBuiltin.hpp"
#include "libpointerBuiltin.hpp"
#include "libpointer_deleteBuiltin.hpp"
#include "libpointer_displayBuiltin.hpp"
#include "libpointer_fieldnamesBuiltin.hpp"
#include "libpointer_getBuiltin.hpp"
#include "libpointer_isNullBuiltin.hpp"
#include "libpointer_ismethodBuiltin.hpp"
#include "libpointer_ispropBuiltin.hpp"
#include "libpointer_isvalidBuiltin.hpp"
#include "libpointer_plusBuiltin.hpp"
#include "libpointer_reshapeBuiltin.hpp"
#include "libpointer_setdatatypeBuiltin.hpp"
#include "libpointer_usedBuiltin.hpp"
#include "dllibisloadedBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"dynamic_link";
//=============================================================================
static const nlsGateway gateway[] = {
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLSYM_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dlsym_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLSYM_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dlsym_dispBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLSYM_CATEGORY_STR, "isvalid"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dlsym_isvalidBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLSYM_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dlsym_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLSYM_CATEGORY_STR, "isprop"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dlsym_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLSYM_CATEGORY_STR, "ismethod"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dlsym_ismethodBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLSYM_CATEGORY_STR, "fieldnames"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dlsym_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLSYM_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dlsym_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLLIB_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dllib_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLLIB_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dllib_dispBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLLIB_CATEGORY_STR, "isvalid"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dllib_isvalidBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLLIB_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dllib_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLLIB_CATEGORY_STR, "isprop"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dllib_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLLIB_CATEGORY_STR, "ismethod"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dllib_ismethodBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLLIB_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dlcloseBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_DLLIB_CATEGORY_STR, "fieldnames"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::dllib_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "isNull"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_isNullBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "reshape"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_reshapeBuiltin, 0, 3, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_getBuiltin, 1, -1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "isprop"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "ismethod"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_ismethodBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "plus"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_plusBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "fieldnames"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "isvalid"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_isvalidBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, "setdatatype"),
        (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_setdatatypeBuiltin, 0, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { "libpointer_used", (ptrBuiltin)Nelson::DynamicLinkGateway::libpointer_usedBuiltin, 1, 0,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { "dlsym_used", (ptrBuiltin)Nelson::DynamicLinkGateway::dlsym_usedBuiltin, 1, 0, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "dllib_used", (ptrBuiltin)Nelson::DynamicLinkGateway::dllib_usedBuiltin, 1, 0, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "dlcall", (ptrBuiltin)Nelson::DynamicLinkGateway::dlcallBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dlopen", (ptrBuiltin)Nelson::DynamicLinkGateway::dlopenBuiltin, 1, 1 },
    { "dlclose", (ptrBuiltin)Nelson::DynamicLinkGateway::dlcloseBuiltin, 0, 1 },
    { "dlsym", (ptrBuiltin)Nelson::DynamicLinkGateway::dlsymBuiltin, -1, 4 },
    { "dllibinfo", (ptrBuiltin)Nelson::DynamicLinkGateway::dllibinfoBuiltin, 1, 1 },
    { "libpointer", (ptrBuiltin)Nelson::DynamicLinkGateway::libpointerBuiltin, 1, -1 },
    { "getdynlibext", (ptrBuiltin)Nelson::DynamicLinkGateway::getdynlibextBuiltin, 1, 0 },
    { "dllibisloaded", (ptrBuiltin)Nelson::DynamicLinkGateway::dllibisloadedBuiltin, 1, 1 },
    { "isNull", (ptrBuiltin)Nelson::DynamicLinkGateway::isNullBuiltin, 1, 1 },
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
