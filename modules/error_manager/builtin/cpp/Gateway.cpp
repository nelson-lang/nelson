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
#include "errorBuiltin.hpp"
#include "lasterrorBuiltin.hpp"
#include "lastwarnBuiltin.hpp"
#include "warningBuiltin.hpp"
#include "getLastReportBuiltin.hpp"
#include "MException_fieldnamesBuiltin.hpp"
#include "MExceptionBuiltin.hpp"
#include "throwBuiltin.hpp"
#include "throwAsCallerBuiltin.hpp"
#include "rethrowBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"error_manager";
//=============================================================================
static const nlsGateway gateway[] = {
    { "error", (ptrBuiltin)Nelson::ErrorManagerGateway::errorBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "warning", (ptrBuiltin)Nelson::ErrorManagerGateway::warningBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lasterror", (ptrBuiltin)Nelson::ErrorManagerGateway::lasterrorBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lastwarn", (ptrBuiltin)Nelson::ErrorManagerGateway::lastwarnBuiltin, 2, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "getLastReport", (ptrBuiltin)Nelson::ErrorManagerGateway::getLastReportBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "MException", (ptrBuiltin)Nelson::ErrorManagerGateway::MExceptionBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "throw", (ptrBuiltin)Nelson::ErrorManagerGateway::throwBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "throwAsCaller", (ptrBuiltin)Nelson::ErrorManagerGateway::throwAsCallerBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rethrow", (ptrBuiltin)Nelson::ErrorManagerGateway::rethrowBuiltin, 0, 1, CPP_BUILTIN },
    { "MException_fieldnames",
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_fieldnamesBuiltin, 1, 1, CPP_BUILTIN }
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
