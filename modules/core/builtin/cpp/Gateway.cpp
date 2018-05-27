//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    { "exit", Nelson::CoreGateway::exitBuiltin, 0, 1 },
    { "nelsonroot", Nelson::CoreGateway::nelsonrootBuiltin, 1, 0 },
    { "run", Nelson::CoreGateway::runBuiltin, 1, 3 },
    { "nfilename", Nelson::CoreGateway::nfilenameBuiltin, 1, 1 },
    { "namelengthmax", Nelson::CoreGateway::namelengthmaxBuiltin, 1, 1 },
    { "banner", Nelson::CoreGateway::bannerBuiltin, 0, 0 },
    { "format", Nelson::CoreGateway::formatBuiltin, 1, 1 },
    { "execstr", Nelson::CoreGateway::execstrBuiltin, 1, 2 },
    { "eval", Nelson::CoreGateway::evalBuiltin, -1, 1 },
    { "evalc", Nelson::CoreGateway::evalcBuiltin, -1, 1 },
    { "evalin", Nelson::CoreGateway::evalinBuiltin, -1, 2 },
    { "echo", Nelson::CoreGateway::echoBuiltin, 1, 1 },
    { "version", Nelson::CoreGateway::versionBuiltin, 2, 1 },
    { "nargin", Nelson::CoreGateway::narginBuiltin, 1, 1 },
    { "nargout", Nelson::CoreGateway::nargoutBuiltin, 1, 1 },
    { "prefdir", Nelson::CoreGateway::prefdirBuiltin, 1, 0 },
    { "maxNumCompThreads", Nelson::CoreGateway::maxNumCompThreadsBuiltin, 1, -1 },
    { "pause", Nelson::CoreGateway::pauseBuiltin, 1, 1 },
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
