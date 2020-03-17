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
#include "epsBuiltin.hpp"
#include "eyeBuiltin.hpp"
#include "iBuiltin.hpp"
#include "infBuiltin.hpp"
#include "nanBuiltin.hpp"
#include "onesBuiltin.hpp"
#include "piBuiltin.hpp"
#include "zerosBuiltin.hpp"
#include "diagBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"constructors_functions";
//=============================================================================
static const nlsGateway gateway[]
    = { { "eye", (void*)Nelson::ConstructorsGateway::eyeBuiltin, 1, 0, CPP_BUILTIN },
          { "i", (void*)Nelson::ConstructorsGateway::iBuiltin, 1, 0, CPP_BUILTIN },
          { "j", (void*)Nelson::ConstructorsGateway::iBuiltin, 1, 0, CPP_BUILTIN },
          { "nan", (void*)Nelson::ConstructorsGateway::nanBuiltin, 1, 0, CPP_BUILTIN },
          { "NaN", (void*)Nelson::ConstructorsGateway::nanBuiltin, 1, 0, CPP_BUILTIN },
          { "inf", (void*)Nelson::ConstructorsGateway::infBuiltin, 1, 0, CPP_BUILTIN },
          { "Inf", (void*)Nelson::ConstructorsGateway::infBuiltin, 1, 0, CPP_BUILTIN },
          { "eps", (void*)Nelson::ConstructorsGateway::epsBuiltin, 1, 0, CPP_BUILTIN },
          { "pi", (void*)Nelson::ConstructorsGateway::piBuiltin, 1, 0, CPP_BUILTIN },
          { "ones", (void*)Nelson::ConstructorsGateway::onesBuiltin, -1, 1, CPP_BUILTIN },
          { "zeros", (void*)Nelson::ConstructorsGateway::zerosBuiltin, -1, 1, CPP_BUILTIN },
          { "diag", (void*)Nelson::ConstructorsGateway::diagBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR } };
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
