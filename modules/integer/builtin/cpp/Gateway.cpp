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
    { "int8", Nelson::IntegerGateway::int8Builtin, 1, 1 },
    { "int16", Nelson::IntegerGateway::int16Builtin, 1, 1 },
    { "int32", Nelson::IntegerGateway::int32Builtin, 1, 1 },
    { "int64", Nelson::IntegerGateway::int64Builtin, 1, 1 },
    { "uint8", Nelson::IntegerGateway::uint8Builtin, 1, 1 },
    { "uint16", Nelson::IntegerGateway::uint16Builtin, 1, 1 },
    { "uint32", Nelson::IntegerGateway::uint32Builtin, 1, 1 },
    { "uint64", Nelson::IntegerGateway::uint64Builtin, 1, 1 },
    { "intmax", Nelson::IntegerGateway::intmaxBuiltin, -1, -1 },
    { "intmin", Nelson::IntegerGateway::intminBuiltin, -1, -1 },
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
