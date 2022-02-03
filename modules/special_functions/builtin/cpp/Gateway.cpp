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
#include "gammaBuiltin.hpp"
#include "betaincBuiltin.hpp"
#include "gcdBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"special_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "gamma", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gammaBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "betainc", (ptrBuiltin)Nelson::SpecialFunctionsGateway::betaincBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "gcd", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gcdBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
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
