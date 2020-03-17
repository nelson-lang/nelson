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
#include "acosBuiltin.hpp"
#include "asinBuiltin.hpp"
#include "atanBuiltin.hpp"
#include "cosBuiltin.hpp"
#include "coshBuiltin.hpp"
#include "sinBuiltin.hpp"
#include "sinhBuiltin.hpp"
#include "tanBuiltin.hpp"
#include "tanhBuiltin.hpp"
#include "cosmBuiltin.hpp"
#include "cosmBuiltin.hpp"
#include "sinmBuiltin.hpp"
#include "tanmBuiltin.hpp"
#include "atan2Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"trigonometric_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "cos", (void*)Nelson::TrigonometricGateway::cosBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "sin", (void*)Nelson::TrigonometricGateway::sinBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "tan", (void*)Nelson::TrigonometricGateway::tanBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "cosh", (void*)Nelson::TrigonometricGateway::coshBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "sinh", (void*)Nelson::TrigonometricGateway::sinhBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "tanh", (void*)Nelson::TrigonometricGateway::tanhBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "acos", (void*)Nelson::TrigonometricGateway::acosBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "asin", (void*)Nelson::TrigonometricGateway::asinBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "atan", (void*)Nelson::TrigonometricGateway::atanBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "cosm", (void*)Nelson::TrigonometricGateway::cosmBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "sinm", (void*)Nelson::TrigonometricGateway::sinmBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "tanm", (void*)Nelson::TrigonometricGateway::tanmBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "atan2", (void*)Nelson::TrigonometricGateway::atan2Builtin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
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
