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
#include "eyeBuiltin.hpp"
#include "nanBuiltin.hpp"
#include "infBuiltin.hpp"
#include "iBuiltin.hpp"
#include "piBuiltin.hpp"
#include "epsBuiltin.hpp"
#include "onesBuiltin.hpp"
#include "zerosBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"constructors_functions";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "eye", Nelson::ConstructorsGateway::eyeBuiltin, 1, 0 },
    { "i", Nelson::ConstructorsGateway::iBuiltin, 1, 0 },
    { "j", Nelson::ConstructorsGateway::iBuiltin, 1, 0 },
    { "nan", Nelson::ConstructorsGateway::nanBuiltin, 1, 0 },
    { "NaN", Nelson::ConstructorsGateway::nanBuiltin, 1, 0 },
    { "inf", Nelson::ConstructorsGateway::infBuiltin, 1, 0 },
    { "Inf", Nelson::ConstructorsGateway::infBuiltin, 1, 0 },
    { "eps", Nelson::ConstructorsGateway::epsBuiltin, 1, 0 },
    { "pi", Nelson::ConstructorsGateway::piBuiltin, 1, 0 },
    { "ones", Nelson::ConstructorsGateway::onesBuiltin, -1, 1 },
    { "zeros", Nelson::ConstructorsGateway::zerosBuiltin, -1, 1 }
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

