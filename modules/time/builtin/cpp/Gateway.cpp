//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "Evaluator.hpp"
#include "NelsonGateway.hpp"
#include "OverloadName.hpp"
#include "calendarBuiltin.hpp"
#include "clockBuiltin.hpp"
#include "cputimeBuiltin.hpp"
#include "datenumBuiltin.hpp"
#include "datevecBuiltin.hpp"
#include "nowBuiltin.hpp"
#include "sleepBuiltin.hpp"
#include "ticBuiltin.hpp"
#include "tocBuiltin.hpp"
#include "timeBuiltin.hpp"
#include "timeitBuiltin.hpp"
#include "datestrBuiltin.hpp"
#include "timezonedstBuiltin.hpp"
#include "timezoneoffsetBuiltin.hpp"
#include "timezoneslistBuiltin.hpp"
#include "timezonenameBuiltin.hpp"
#include "timerBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"time";
//=============================================================================
static const nlsGateway gateway[] = {
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::TimeGateway::timer_displayBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::TimeGateway::timer_dispBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::TimeGateway::timer_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "fieldnames"),
        (ptrBuiltin)Nelson::TimeGateway::timer_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::TimeGateway::timer_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "ismethod"),
        (ptrBuiltin)Nelson::TimeGateway::timer_ismethodBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "isprop"),
        (ptrBuiltin)Nelson::TimeGateway::timer_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "isvalid"),
        (ptrBuiltin)Nelson::TimeGateway::timer_isvalidBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "methods"),
        (ptrBuiltin)Nelson::TimeGateway::timer_methodsBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "properties"),
        (ptrBuiltin)Nelson::TimeGateway::timer_propertiesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "set"),
        (ptrBuiltin)Nelson::TimeGateway::timer_setBuiltin, 0, -1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "start"),
        (ptrBuiltin)Nelson::TimeGateway::timer_startBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "startat"),
        (ptrBuiltin)Nelson::TimeGateway::timer_startatBuiltin, 0, -1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "stop"),
        (ptrBuiltin)Nelson::TimeGateway::timer_stopBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_TIMER_CATEGORY_STR, "wait"),
        (ptrBuiltin)Nelson::TimeGateway::timer_waitBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { "tic", (ptrBuiltin)Nelson::TimeGateway::ticBuiltin, 0, 0, CPP_BUILTIN_WITH_EVALUATOR },
    { "toc", (ptrBuiltin)Nelson::TimeGateway::tocBuiltin, 1, 0, CPP_BUILTIN_WITH_EVALUATOR },
    { "sleep", (ptrBuiltin)Nelson::TimeGateway::sleepBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "datevec", (ptrBuiltin)Nelson::TimeGateway::datevecBuiltin, 6, 1 },
    { "calendar", (ptrBuiltin)Nelson::TimeGateway::calendarBuiltin, 1, 2, CPP_BUILTIN },
    { "now", (ptrBuiltin)Nelson::TimeGateway::nowBuiltin, 1, 0, CPP_BUILTIN },
    { "clock", (ptrBuiltin)Nelson::TimeGateway::clockBuiltin, 1, 0, CPP_BUILTIN },
    { "cputime", (ptrBuiltin)Nelson::TimeGateway::cputimeBuiltin, 1, 0, CPP_BUILTIN },
    { "datenum", (ptrBuiltin)Nelson::TimeGateway::datenumBuiltin, 1, 6, CPP_BUILTIN },
    { "datestr", (ptrBuiltin)Nelson::TimeGateway::datestrBuiltin, 1, -2, CPP_BUILTIN },
    { "time", (ptrBuiltin)Nelson::TimeGateway::timeBuiltin, 1, 0, CPP_BUILTIN },
    { "__datetime_serial_add_seconds__",
        (ptrBuiltin)Nelson::TimeGateway::datetime_serial_add_secondsBuiltin, 1, 3, CPP_BUILTIN },
    { "timeit", (ptrBuiltin)Nelson::TimeGateway::timeitBuiltin, 1, -2, CPP_BUILTIN_WITH_EVALUATOR },
    { "timezoneoffset", (ptrBuiltin)Nelson::TimeGateway::timezoneoffsetBuiltin, 1, 2, CPP_BUILTIN },
    { "timezonedst", (ptrBuiltin)Nelson::TimeGateway::timezonedstBuiltin, 1, 2, CPP_BUILTIN },
    { "timezoneslist", (ptrBuiltin)Nelson::TimeGateway::timezoneslistBuiltin, 2, 0, CPP_BUILTIN },
    { "timezonename", (ptrBuiltin)Nelson::TimeGateway::timezonenameBuiltin, 1, 2, CPP_BUILTIN },
    { "timer", (ptrBuiltin)Nelson::TimeGateway::timerBuiltin, 1, -1, CPP_BUILTIN },
    { "timerfind", (ptrBuiltin)Nelson::TimeGateway::timerfindBuiltin, 1, -1, CPP_BUILTIN },
    { "timerfindall", (ptrBuiltin)Nelson::TimeGateway::timerfindallBuiltin, 1, -1, CPP_BUILTIN },
    { "start", (ptrBuiltin)Nelson::TimeGateway::startBuiltin, 0, 1, CPP_BUILTIN },
    { "startat", (ptrBuiltin)Nelson::TimeGateway::startatBuiltin, 0, -1, CPP_BUILTIN },
    { "stop", (ptrBuiltin)Nelson::TimeGateway::stopBuiltin, 0, 1, CPP_BUILTIN },
    { "wait", (ptrBuiltin)Nelson::TimeGateway::waitBuiltin, 1, -1, CPP_BUILTIN },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
