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
#include "addmoduleBuiltin.hpp"
#include "getmodulesBuiltin.hpp"
#include "ismoduleBuiltin.hpp"
#include "modulepathBuiltin.hpp"
#include "removemoduleBuiltin.hpp"
#include "requiremoduleBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"modules_manager";
//=============================================================================
static const nlsGateway gateway[]
    = { { "removemodule", Nelson::ModulesManagerGateway::removemoduleBuiltin, 0, 1 },
          { "addmodule", Nelson::ModulesManagerGateway::addmoduleBuiltin, 0, 2 },
          { "modulepath", Nelson::ModulesManagerGateway::modulepathBuiltin, 1, 3 },
          { "getmodules", Nelson::ModulesManagerGateway::getmodulesBuiltin, 1, 2 },
          { "ismodule", Nelson::ModulesManagerGateway::ismoduleBuiltin, 1, 1 },
          { "requiremodule", Nelson::ModulesManagerGateway::requiremoduleBuiltin, 1, 1 } };
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
