//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "int8_uminusBuiltin.hpp"
#include "int16_uminusBuiltin.hpp"
#include "int32_uminusBuiltin.hpp"
#include "int64_uminusBuiltin.hpp"
#include "uint8_uminusBuiltin.hpp"
#include "uint16_uminusBuiltin.hpp"
#include "uint32_uminusBuiltin.hpp"
#include "uint64_uminusBuiltin.hpp"
#include "integer_horzcat_integerBuiltin.hpp"
#include "ndarrayinteger_horzcat_ndarrayintegerBuiltin.hpp"
#include "integer_vertcat_integerBuiltin.hpp"
#include "ndarrayinteger_vertcat_ndarrayintegerBuiltin.hpp"
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
    { "int8_uminus", Nelson::IntegerGateway::int8_uminusBuiltin, 1, 1 },
    { "int16_uminus", Nelson::IntegerGateway::int16_uminusBuiltin, 1, 1 },
    { "int32_uminus", Nelson::IntegerGateway::int32_uminusBuiltin, 1, 1 },
    { "int64_uminus", Nelson::IntegerGateway::int64_uminusBuiltin, 1, 1 },
    { "uint8_uminus", Nelson::IntegerGateway::uint8_uminusBuiltin, 1, 1 },
    { "uint16_uminus", Nelson::IntegerGateway::uint16_uminusBuiltin, 1, 1 },
    { "uint32_uminus", Nelson::IntegerGateway::uint32_uminusBuiltin, 1, 1 },
    { "uint64_uminus", Nelson::IntegerGateway::uint64_uminusBuiltin, 1, 1 },
    { "int8_vertcat_int8", Nelson::IntegerGateway::int8_vertcat_int8Builtin, 1, 2 },
    { "int16_vertcat_int16", Nelson::IntegerGateway::int16_vertcat_int16Builtin, 1, 2 },
    { "int32_vertcat_int32", Nelson::IntegerGateway::int32_vertcat_int32Builtin, 1, 2 },
    { "int64_vertcat_int64", Nelson::IntegerGateway::int64_vertcat_int64Builtin, 1, 2 },
    { "uint8_vertcat_uint8", Nelson::IntegerGateway::uint8_vertcat_uint8Builtin, 1, 2 },
    { "uint16_vertcat_uint16", Nelson::IntegerGateway::uint16_vertcat_uint16Builtin, 1, 2 },
    { "uint32_vertcat_uint32", Nelson::IntegerGateway::uint32_vertcat_uint32Builtin, 1, 2 },
    { "uint64_vertcat_uint64", Nelson::IntegerGateway::uint64_vertcat_uint64Builtin, 1, 2 },
    { "ndarrayint8_vertcat_ndarrayint8", Nelson::IntegerGateway::ndarrayint8_vertcat_ndarrayint8Builtin, 1, 2 },
    { "ndarrayint16_vertcat_ndarrayint16", Nelson::IntegerGateway::ndarrayint16_vertcat_ndarrayint16Builtin, 1, 2 },
    { "ndarrayint32_vertcat_ndarrayint32", Nelson::IntegerGateway::ndarrayint32_vertcat_ndarrayint32Builtin, 1, 2 },
    { "ndarrayint64_vertcat_ndarrayint64", Nelson::IntegerGateway::ndarrayint64_vertcat_ndarrayint64Builtin, 1, 2 },
    { "ndarrayuint8_vertcat_ndarrayuint8", Nelson::IntegerGateway::ndarrayuint8_vertcat_ndarrayuint8Builtin, 1, 2 },
    { "ndarrayuint16_vertcat_ndarrayuint16", Nelson::IntegerGateway::ndarrayuint16_vertcat_ndarrayuint16Builtin, 1, 2 },
    { "ndarrayuint32_vertcat_ndarrayuint32", Nelson::IntegerGateway::ndarrayuint32_vertcat_ndarrayuint32Builtin, 1, 2 },
    { "ndarrayuint64_vertcat_ndarrayuint64", Nelson::IntegerGateway::ndarrayuint64_vertcat_ndarrayuint64Builtin, 1, 2 },
    { "int8_horzcat_int8", Nelson::IntegerGateway::int8_horzcat_int8Builtin, 1, 2 },
    { "int16_horzcat_int16", Nelson::IntegerGateway::int16_horzcat_int16Builtin, 1, 2 },
    { "int32_horzcat_int32", Nelson::IntegerGateway::int32_horzcat_int32Builtin, 1, 2 },
    { "int64_horzcat_int64", Nelson::IntegerGateway::int64_horzcat_int64Builtin, 1, 2 },
    { "uint8_horzcat_uint8", Nelson::IntegerGateway::uint8_horzcat_uint8Builtin, 1, 2 },
    { "uint16_horzcat_uint16", Nelson::IntegerGateway::uint16_horzcat_uint16Builtin, 1, 2 },
    { "uint32_horzcat_uint32", Nelson::IntegerGateway::uint32_horzcat_uint32Builtin, 1, 2 },
    { "uint64_horzcat_uint64", Nelson::IntegerGateway::uint64_horzcat_uint64Builtin, 1, 2 },
    { "ndarrayint8_horzcat_ndarrayint8", Nelson::IntegerGateway::ndarrayint8_horzcat_ndarrayint8Builtin, 1, 2 },
    { "ndarrayint16_horzcat_ndarrayint16", Nelson::IntegerGateway::ndarrayint16_horzcat_ndarrayint16Builtin, 1, 2 },
    { "ndarrayint32_horzcat_ndarrayint32", Nelson::IntegerGateway::ndarrayint32_horzcat_ndarrayint32Builtin, 1, 2 },
    { "ndarrayint64_horzcat_ndarrayint64", Nelson::IntegerGateway::ndarrayint64_horzcat_ndarrayint64Builtin, 1, 2 },
    { "ndarrayuint8_horzcat_ndarrayuint8", Nelson::IntegerGateway::ndarrayuint8_horzcat_ndarrayuint8Builtin, 1, 2 },
    { "ndarrayuint16_horzcat_ndarrayuint16", Nelson::IntegerGateway::ndarrayuint16_horzcat_ndarrayuint16Builtin, 1, 2 },
    { "ndarrayuint32_horzcat_ndarrayuint32", Nelson::IntegerGateway::ndarrayuint32_horzcat_ndarrayuint32Builtin, 1, 2 },
    { "ndarrayuint64_horzcat_ndarrayuint64", Nelson::IntegerGateway::ndarrayuint64_horzcat_ndarrayuint64Builtin, 1, 2 },
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
