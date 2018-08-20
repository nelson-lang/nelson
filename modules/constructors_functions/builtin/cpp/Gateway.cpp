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
#include "epsBuiltin.hpp"
#include "eyeBuiltin.hpp"
#include "iBuiltin.hpp"
#include "infBuiltin.hpp"
#include "nanBuiltin.hpp"
#include "onesBuiltin.hpp"
#include "piBuiltin.hpp"
#include "zerosBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"constructors_functions";
//=============================================================================
static const nlsGateway gateway[]
    = { { "eye", Nelson::ConstructorsGateway::eyeBuiltin, 1, 0, CPP_BUILTIN },
          { "i", Nelson::ConstructorsGateway::iBuiltin, 1, 0, CPP_BUILTIN },
          { "j", Nelson::ConstructorsGateway::iBuiltin, 1, 0, CPP_BUILTIN },
          { "nan", Nelson::ConstructorsGateway::nanBuiltin, 1, 0, CPP_BUILTIN },
          { "NaN", Nelson::ConstructorsGateway::nanBuiltin, 1, 0, CPP_BUILTIN },
          { "inf", Nelson::ConstructorsGateway::infBuiltin, 1, 0, CPP_BUILTIN },
          { "Inf", Nelson::ConstructorsGateway::infBuiltin, 1, 0, CPP_BUILTIN },
          { "eps", Nelson::ConstructorsGateway::epsBuiltin, 1, 0, CPP_BUILTIN },
          { "pi", Nelson::ConstructorsGateway::piBuiltin, 1, 0, CPP_BUILTIN },
          { "ones", Nelson::ConstructorsGateway::onesBuiltin, -1, 1, CPP_BUILTIN },
          { "zeros", Nelson::ConstructorsGateway::zerosBuiltin, -1, 1, CPP_BUILTIN } };
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
