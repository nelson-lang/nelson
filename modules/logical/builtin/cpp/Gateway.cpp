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
#include "logicalBuiltin.hpp"
#include "logical_dispBuiltin.hpp"
#include "trueBuiltin.hpp"
#include "falseBuiltin.hpp"
#include "ndarraylogical_dispBuiltin.hpp"
#include "logical_or_logicalBuiltin.hpp"
#include "logical_and_logicalBuiltin.hpp"
#include "logical_notBuiltin.hpp"
#include "xorBuiltin.hpp"
#include "anyBuiltin.hpp"
#include "allBuiltin.hpp"
#include "logical_anyBuiltin.hpp"
#include "logical_allBuiltin.hpp"
#include "logical_isequalBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"logical";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "logical", Nelson::LogicalGateway::logicalBuiltin, 1, 1 },
    { "true", Nelson::LogicalGateway::trueBuiltin, 1, 1 },
    { "false", Nelson::LogicalGateway::falseBuiltin, 1, 1 },
    { "logical_disp", Nelson::LogicalGateway::logical_dispBuiltin, 0, 1 },
    { "logical_or_logical", Nelson::LogicalGateway::logical_or_logicalBuiltin, 1, 2 },
    { "logical_and_logical", Nelson::LogicalGateway::logical_and_logicalBuiltin, 1, 2 },
    { "ndarraylogical_disp", Nelson::LogicalGateway::ndarraylogical_dispBuiltin, 0, 1 },
    { "logical_not", Nelson::LogicalGateway::logical_notBuiltin, 1, 1 },
    { "xor", Nelson::LogicalGateway::xorBuiltin, 1, 2 },
    { "any", Nelson::LogicalGateway::anyBuiltin, 1, 2 },
    { "all", Nelson::LogicalGateway::allBuiltin, 1, 1 },
    { "logical_any", Nelson::LogicalGateway::logical_anyBuiltin, 1, 2 },
    { "logical_all", Nelson::LogicalGateway::logical_allBuiltin, 1, 1 },
    { "logical_isequal", Nelson::LogicalGateway::logical_isequalBuiltin, 1, 2 },
    { "logical_isequaln", Nelson::LogicalGateway::logical_isequalBuiltin, 1, 2 },
    { "ndarraylogical_isequal", Nelson::LogicalGateway::logical_isequalBuiltin, 1, 2 },
    { "ndarraylogical_isequaln", Nelson::LogicalGateway::logical_isequalBuiltin, 1, 2 },

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
