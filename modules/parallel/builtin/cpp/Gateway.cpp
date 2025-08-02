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
#include "BackgroundPoolObject.hpp"
#include "backgroundPoolBuiltin.hpp"
#include "backgroundPool_getBuiltin.hpp"
#include "backgroundPool_displayBuiltin.hpp"
#include "backgroundPool_usedBuiltin.hpp"
#include "backgroundPool_deleteBuiltin.hpp"
#include "backgroundPool_fieldnamesBuiltin.hpp"
#include "backgroundPool_structBuiltin.hpp"
#include "parfevalBuiltin.hpp"
#include "FevalFuture_usedBuiltin.hpp"
#include "AfterEachFuture_usedBuiltin.hpp"
#include "AfterAllFuture_usedBuiltin.hpp"
#include "FevalFuture_deleteBuiltin.hpp"
#include "AfterEachFuture_deleteBuiltin.hpp"
#include "AfterAllFuture_deleteBuiltin.hpp"
#include "Future_displayBuiltin.hpp"
#include "Future_waitBuiltin.hpp"
#include "Future_cancelBuiltin.hpp"
#include "Future_getBuiltin.hpp"
#include "FevalQueue_displayBuiltin.hpp"
#include "FevalQueue_getBuiltin.hpp"
#include "FevalQueue_usedBuiltin.hpp"
#include "FevalQueue_deleteBuiltin.hpp"
#include "FevalQueue_cancelAllBuiltin.hpp"
#include "fetchOutputsBuiltin.hpp"
#include "afterAllBuiltin.hpp"
#include "afterEachBuiltin.hpp"
#include "fetchNextBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"parallel";
//=============================================================================
static const nlsGateway gateway[] = {
    //=============================================================================
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { "backgroundPool", (ptrBuiltin)Nelson::ParallelGateway::backgroundPoolBuiltin, 1, 0,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_deleteBuiltin, 0, 1 },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR, "fieldnames"),
        (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR, "struct"),
        (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_structBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_FEVALFUTURE_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::ParallelGateway::Future_getBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::ParallelGateway::Future_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::ParallelGateway::Future_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_FEVALFUTURE_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::ParallelGateway::FevalFuture_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::ParallelGateway::AfterAllFuture_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::ParallelGateway::AfterEachFuture_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_FEVALQUEUE_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_FEVALQUEUE_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_FEVALQUEUE_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_FEVALQUEUE_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_FEVALFUTURE_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::ParallelGateway::Future_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::ParallelGateway::Future_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::ParallelGateway::Future_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_FEVALFUTURE_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::ParallelGateway::Future_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::ParallelGateway::Future_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::ParallelGateway::Future_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { "fetchNext", (ptrBuiltin)Nelson::ParallelGateway::fetchNextBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "afterEach", (ptrBuiltin)Nelson::ParallelGateway::afterEachBuiltin, 1, 3, CPP_BUILTIN },
    { "parfeval", (ptrBuiltin)Nelson::ParallelGateway::parfevalBuiltin, 1, -3, CPP_BUILTIN },
    { "cancelAll", (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_cancelAllBuiltin, 0, 1 },
    { "afterAll", (ptrBuiltin)Nelson::ParallelGateway::afterAllBuiltin, 1, 3, CPP_BUILTIN },
    { "cancel", (ptrBuiltin)Nelson::ParallelGateway::Future_cancelBuiltin, 0, 1 },
    { "wait", (ptrBuiltin)Nelson::ParallelGateway::Future_waitBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "fetchOutputs", (ptrBuiltin)Nelson::ParallelGateway::fetchOutputsBuiltin, -1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    //=============================================================================
    { "FevalFuture_used", (ptrBuiltin)Nelson::ParallelGateway::FevalFuture_usedBuiltin, 1, 0 },
    { "FevalFuture_used", (ptrBuiltin)Nelson::ParallelGateway::FevalFuture_usedBuiltin, 1, 0 },
    { "AfterEachFuture_used", (ptrBuiltin)Nelson::ParallelGateway::AfterEachFuture_usedBuiltin, 1,
        0 },
    { "AfterAllFuture_used", (ptrBuiltin)Nelson::ParallelGateway::AfterAllFuture_usedBuiltin, 1,
        0 },
    { "backgroundPool_used", (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_usedBuiltin, 1,
        0 },
    { "FevalQueue_used", (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_usedBuiltin, 1, 0 },
};
//=============================================================================
static bool
finishParallelModule(Nelson::Evaluator* eval)
{
    if (BackgroundPoolObject::isInitialized()) {
        BackgroundPoolObject::getInstance()->resetThreadPool();
    }
    return true;
}
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishParallelModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
