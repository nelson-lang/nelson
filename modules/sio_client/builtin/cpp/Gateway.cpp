//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "sioemitBuiltin.hpp"
#include "sioregisterBuiltin.hpp"
#include "siounregisterBuiltin.hpp"
#include "SioClientQuit.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"sio_client";
//=============================================================================
static const nlsGateway gateway[]
    = { { "sioemit", Nelson::SioClientGateway::sioemitBuiltin, 0, -1 },
          { "sioregister", Nelson::SioClientGateway::sioregisterBuiltin, 0, 2 },
          { "siounregister", Nelson::SioClientGateway::siounregisterBuiltin, 0, 1 } };
//=============================================================================
static bool
finishSioClientModule(Nelson::Evaluator* eval)
{
    sioquit();
    return true;
}
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishSioClientModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
