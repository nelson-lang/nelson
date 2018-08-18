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
#include "NelsonGateway.hpp"
#include "dbstackBuiltin.hpp"
#include "iskeywordBuiltin.hpp"
#include "max_recursion_depthBuiltin.hpp"
#include "parsefileBuiltin.hpp"
#include "parsestringBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"interpreter";
//=============================================================================
static const nlsGateway gateway[] = {
    { "iskeyword", Nelson::InterpreterGateway::iskeywordBuiltin, 1, 1 },
    { "parsefile", Nelson::InterpreterGateway::parsefileBuiltin, 1, 1 },
    { "parsestring", Nelson::InterpreterGateway::parsestringBuiltin, 1, 1 },
    { "max_recursion_depth", Nelson::InterpreterGateway::max_recursion_depthBuiltin, 1, 1 },
    { "dbstack", Nelson::ErrorManagerGateway::dbstackBuiltin, -1, -1 },

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
