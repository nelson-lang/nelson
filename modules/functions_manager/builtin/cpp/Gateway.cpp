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
#include "addpathBuiltin.hpp"
#include "builtinBuiltin.hpp"
#include "clearfunBuiltin.hpp"
#include "fevalBuiltin.hpp"
#include "isbuiltinBuiltin.hpp"
#include "ismexBuiltin.hpp"
#include "ismacroBuiltin.hpp"
#include "macroargsBuiltin.hpp"
#include "pathBuiltin.hpp"
#include "rehashBuiltin.hpp"
#include "restoredefaultpathBuiltin.hpp"
#include "rmpathBuiltin.hpp"
#include "userpathBuiltin.hpp"
#include "whatBuiltin.hpp"
#include "whichBuiltin.hpp"
#include "inmemBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"functions_manager";
//=============================================================================
static const nlsGateway gateway[] = {
    { "which", (ptrBuiltin)Nelson::FunctionsGateway::whichBuiltin, 1, 1 },
    { "macroargs", (ptrBuiltin)Nelson::FunctionsGateway::macroargsBuiltin, 2, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "builtin", (ptrBuiltin)Nelson::FunctionsGateway::builtinBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "feval", (ptrBuiltin)Nelson::FunctionsGateway::fevalBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "what", (ptrBuiltin)Nelson::FunctionsGateway::whatBuiltin, -1, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "addpath", (ptrBuiltin)Nelson::FunctionsGateway::addpathBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "clearfun", (ptrBuiltin)Nelson::FunctionsGateway::clearfunBuiltin, 1, 1 },
    { "rmpath", (ptrBuiltin)Nelson::FunctionsGateway::rmpathBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "path", (ptrBuiltin)Nelson::FunctionsGateway::pathBuiltin, 1, 2 },
    { "ismacro", (ptrBuiltin)Nelson::FunctionsGateway::ismacroBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isbuiltin", (ptrBuiltin)Nelson::FunctionsGateway::isbuiltinBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "restoredefaultpath", (ptrBuiltin)Nelson::FunctionsGateway::restoredefaultpathBuiltin, 0, 0 },
    { "rehash", (ptrBuiltin)Nelson::FunctionsGateway::rehashBuiltin, 0, 0 },
    { "userpath", (ptrBuiltin)Nelson::FunctionsGateway::userpathBuiltin, 1, 1 },
    { "inmem", (ptrBuiltin)Nelson::FunctionsGateway::inmemBuiltin, 2, 0 },
    { "ismex", (ptrBuiltin)Nelson::FunctionsGateway::ismexBuiltin, 1, 1, CPP_BUILTIN },

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
