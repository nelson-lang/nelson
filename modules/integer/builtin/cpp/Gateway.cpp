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
#include "integer_isequalBuiltin.hpp"
#include "ndarrayinteger_isequalBuiltin.hpp"
#include "intmaxBuiltin.hpp"
#include "intminBuiltin.hpp"
#include "integer_dispBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"integer";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "int8", Nelson::IntegerGateway::int8Builtin, 1, 1 },
    { "int16", Nelson::IntegerGateway::int16Builtin, 1, 1 },
    { "int32", Nelson::IntegerGateway::int32Builtin, 1, 1 },
    { "int64", Nelson::IntegerGateway::int64Builtin, 1, 1 },
    { "uint8", Nelson::IntegerGateway::uint8Builtin, 1, 1 },
    { "uint16", Nelson::IntegerGateway::uint16Builtin, 1, 1 },
    { "uint32", Nelson::IntegerGateway::uint32Builtin, 1, 1 },
    { "uint64", Nelson::IntegerGateway::uint64Builtin, 1, 1 },
    { "int8_isequal", Nelson::IntegerGateway::int8_isequalBuiltin, 1, 2 },
    { "int16_isequal", Nelson::IntegerGateway::int16_isequalBuiltin, 1, 2 },
    { "int32_isequal", Nelson::IntegerGateway::int32_isequalBuiltin, 1, 2 },
    { "int64_isequal", Nelson::IntegerGateway::int64_isequalBuiltin, 1, 2 },
    { "uint8_isequal", Nelson::IntegerGateway::uint8_isequalBuiltin, 1, 2 },
    { "uint16_isequal", Nelson::IntegerGateway::uint16_isequalBuiltin, 1, 2 },
    { "uint32_isequal", Nelson::IntegerGateway::uint32_isequalBuiltin, 1, 2 },
    { "uint64_isequal", Nelson::IntegerGateway::uint64_isequalBuiltin, 1, 2 },
    { "ndarrayint8_isequal", Nelson::IntegerGateway::ndarrayint8_isequalBuiltin, 1, 2 },
    { "ndarrayint16_isequal", Nelson::IntegerGateway::ndarrayint16_isequalBuiltin, 1, 2 },
    { "ndarrayint32_isequal", Nelson::IntegerGateway::ndarrayint32_isequalBuiltin, 1, 2 },
    { "ndarrayint64_isequal", Nelson::IntegerGateway::ndarrayint64_isequalBuiltin, 1, 2 },
    { "ndarrayuint8_isequal", Nelson::IntegerGateway::ndarrayuint8_isequalBuiltin, 1, 2 },
    { "ndarrayuint16_isequal", Nelson::IntegerGateway::ndarrayuint16_isequalBuiltin, 1, 2 },
    { "ndarrayuint32_isequal", Nelson::IntegerGateway::ndarrayuint32_isequalBuiltin, 1, 2 },
    { "ndarrayuint64_isequal", Nelson::IntegerGateway::ndarrayuint64_isequalBuiltin, 1, 2 },
    { "int8_isequaln", Nelson::IntegerGateway::int8_isequalBuiltin, 1, 2 },
    { "int16_isequaln", Nelson::IntegerGateway::int16_isequalBuiltin, 1, 2 },
    { "int32_isequaln", Nelson::IntegerGateway::int32_isequalBuiltin, 1, 2 },
    { "int64_isequaln", Nelson::IntegerGateway::int64_isequalBuiltin, 1, 2 },
    { "uint8_isequaln", Nelson::IntegerGateway::uint8_isequalBuiltin, 1, 2 },
    { "uint16_isequaln", Nelson::IntegerGateway::uint16_isequalBuiltin, 1, 2 },
    { "uint32_isequaln", Nelson::IntegerGateway::uint32_isequalBuiltin, 1, 2 },
    { "uint64_isequaln", Nelson::IntegerGateway::uint64_isequalBuiltin, 1, 2 },
    { "ndarrayint8_isequaln", Nelson::IntegerGateway::ndarrayint8_isequalBuiltin, 1, 2 },
    { "ndarrayint16_isequaln", Nelson::IntegerGateway::ndarrayint16_isequalBuiltin, 1, 2 },
    { "ndarrayint32_isequaln", Nelson::IntegerGateway::ndarrayint32_isequalBuiltin, 1, 2 },
    { "ndarrayint64_isequaln", Nelson::IntegerGateway::ndarrayint64_isequalBuiltin, 1, 2 },
    { "ndarrayuint8_isequaln", Nelson::IntegerGateway::ndarrayuint8_isequalBuiltin, 1, 2 },
    { "ndarrayuint16_isequaln", Nelson::IntegerGateway::ndarrayuint16_isequalBuiltin, 1, 2 },
    { "ndarrayuint32_isequaln", Nelson::IntegerGateway::ndarrayuint32_isequalBuiltin, 1, 2 },
    { "ndarrayuint64_isequaln", Nelson::IntegerGateway::ndarrayuint64_isequalBuiltin, 1, 2 },
    { "intmax", Nelson::IntegerGateway::intmaxBuiltin, -1, -1 },
    { "intmin", Nelson::IntegerGateway::intminBuiltin, -1, -1 },
    { "int8_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "int16_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "int32_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "int64_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "uint8_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "uint16_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "uint32_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "uint64_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "ndarrayint8_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "ndarrayint16_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "ndarrayint32_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "ndarrayint64_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "ndarrayuint8_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "ndarrayuint16_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "ndarrayuint32_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },
    { "ndarrayuint64_disp", Nelson::IntegerGateway::integer_dispBuiltin, 0, 1 },

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
