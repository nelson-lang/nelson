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
#include "int8Builtin.hpp"
#include "int16Builtin.hpp"
#include "int32Builtin.hpp"
#include "int64Builtin.hpp"
#include "uint8Builtin.hpp"
#include "uint16Builtin.hpp"
#include "uint32Builtin.hpp"
#include "uint64Builtin.hpp"
#include "intmaxBuiltin.hpp"
#include "intminBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"integer";
//=============================================================================
static const nlsGateway gateway[] = {
    { "int8", (ptrBuiltin)Nelson::IntegerGateway::int8Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "int16", (ptrBuiltin)Nelson::IntegerGateway::int16Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "int32", (ptrBuiltin)Nelson::IntegerGateway::int32Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "int64", (ptrBuiltin)Nelson::IntegerGateway::int64Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "uint8", (ptrBuiltin)Nelson::IntegerGateway::uint8Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "uint16", (ptrBuiltin)Nelson::IntegerGateway::uint16Builtin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint32", (ptrBuiltin)Nelson::IntegerGateway::uint32Builtin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uint64", (ptrBuiltin)Nelson::IntegerGateway::uint64Builtin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "intmax", (ptrBuiltin)Nelson::IntegerGateway::intmaxBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "intmin", (ptrBuiltin)Nelson::IntegerGateway::intminBuiltin, -1, -1,
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
