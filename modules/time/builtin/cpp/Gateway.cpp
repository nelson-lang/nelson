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
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"time";
//=============================================================================
static const nlsGateway gateway[] = {
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
    { "timeit", (ptrBuiltin)Nelson::TimeGateway::timeitBuiltin, 1, -2, CPP_BUILTIN_WITH_EVALUATOR },
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
