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
#include "GetPreferencesPath.hpp"
#include "NelsonGateway.hpp"
#include "bannerBuiltin.hpp"
#include "echoBuiltin.hpp"
#include "evalBuiltin.hpp"
#include "evalcBuiltin.hpp"
#include "evalinBuiltin.hpp"
#include "execstrBuiltin.hpp"
#include "exitBuiltin.hpp"
#include "formatBuiltin.hpp"
#include "maxNumCompThreadsBuiltin.hpp"
#include "namelengthmaxBuiltin.hpp"
#include "narginBuiltin.hpp"
#include "nargoutBuiltin.hpp"
#include "nelsonrootBuiltin.hpp"
#include "nfilenameBuiltin.hpp"
#include "pauseBuiltin.hpp"
#include "prefdirBuiltin.hpp"
#include "runBuiltin.hpp"
#include "versionBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"core";
//=============================================================================
static const nlsGateway gateway[] = {
    { "exit", (void*)Nelson::CoreGateway::exitBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "run", (void*)Nelson::CoreGateway::runBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "nfilename", (void*)Nelson::CoreGateway::nfilenameBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "banner", (void*)Nelson::CoreGateway::bannerBuiltin, 0, 0, CPP_BUILTIN_WITH_EVALUATOR },
    { "execstr", (void*)Nelson::CoreGateway::execstrBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "eval", (void*)Nelson::CoreGateway::evalBuiltin, -1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "evalc", (void*)Nelson::CoreGateway::evalcBuiltin, -1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "evalin", (void*)Nelson::CoreGateway::evalinBuiltin, -1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "echo", (void*)Nelson::CoreGateway::echoBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "nargin", (void*)Nelson::CoreGateway::narginBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "nargout", (void*)Nelson::CoreGateway::nargoutBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "pause", (void*)Nelson::CoreGateway::pauseBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "namelengthmax", (void*)Nelson::CoreGateway::namelengthmaxBuiltin, 1, 1 },
    { "format", (void*)Nelson::CoreGateway::formatBuiltin, 1, 1 },
    { "nelsonroot", (void*)Nelson::CoreGateway::nelsonrootBuiltin, 1, 0 },
    { "version", (void*)Nelson::CoreGateway::versionBuiltin, 2, 1 },
    { "prefdir", (void*)Nelson::CoreGateway::prefdirBuiltin, 1, 0 },
    { "maxNumCompThreads", (void*)Nelson::CoreGateway::maxNumCompThreadsBuiltin, 1, -1 },
};
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
static bool
initializeCoreModule(Nelson::Evaluator* eval)
{
    ComputePreferencesPath();
    return true;
}
//=============================================================================
static bool
finishCoreModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeCoreModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishCoreModule)
//=============================================================================
