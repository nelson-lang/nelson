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
#include "charBuiltin.hpp"
#include "strcmpBuiltin.hpp"
#include "strncmpBuiltin.hpp"
#include "string_dispBuiltin.hpp"
#include "tolowerBuiltin.hpp"
#include "toupperBuiltin.hpp"
#include "strfindBuiltin.hpp"
#include "string_horzcat_stringBuiltin.hpp"
#include "string_vertcat_stringBuiltin.hpp"
#include "ndarraystring_horzcat_ndarraystringBuiltin.hpp"
#include "ndarraystring_vertcat_ndarraystringBuiltin.hpp"
#include "string_isequalBuiltin.hpp"
#include "ndarraystring_isequalBuiltin.hpp"
#include "sprintfBuiltin.hpp"
#include "int2strBuiltin.hpp"
#include "str2doubleBuiltin.hpp"
#include "mat2strBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"string";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "char", Nelson::StringGateway::charBuiltin, 1, -1 },
    { "strcmp", Nelson::StringGateway::strcmpBuiltin, 1, 2 },
    { "strcmpi", Nelson::StringGateway::strcmpiBuiltin, 1, 2 },
    { "strncmp", Nelson::StringGateway::strncmpBuiltin, 1, 3 },
    { "strncmpi", Nelson::StringGateway::strncmpiBuiltin, 1, 3 },
    { "string_disp", Nelson::StringGateway::string_dispBuiltin, 0, 1 },
    { "tolower", Nelson::StringGateway::tolowerBuiltin, 1, 1 },
    { "lower", Nelson::StringGateway::tolowerBuiltin, 1, 1 },
    { "toupper", Nelson::StringGateway::toupperBuiltin, 1, 1 },
    { "upper", Nelson::StringGateway::toupperBuiltin, 1, 1 },
    { "strfind", Nelson::StringGateway::strfindBuiltin, 1, 2 },
    { "string_vertcat_string", Nelson::StringGateway::string_vertcat_stringBuiltin, 1, 2 },
    { "string_horzcat_string", Nelson::StringGateway::string_horzcat_stringBuiltin, 1, 2 },
    { "ndarraystring_vertcat_ndarraystring", Nelson::StringGateway::ndarraystring_vertcat_ndarraystringBuiltin, 1, 2 },
    { "ndarraystring_horzcat_ndarraystring", Nelson::StringGateway::ndarraystring_horzcat_ndarraystringBuiltin, 1, 2 },
    { "string_isequal", Nelson::StringGateway::string_isequalBuiltin, 1, 2 },
    { "string_isequaln", Nelson::StringGateway::string_isequalBuiltin, 1, 2 },
    { "ndarraystring_isequal", Nelson::StringGateway::ndarraystring_isequalBuiltin, 1, 2 },
    { "ndarraystring_isequaln", Nelson::StringGateway::ndarraystring_isequalBuiltin, 1, 2 },
    { "sprintf", Nelson::StringGateway::sprintfBuiltin, 2, -1},
    { "int2str", Nelson::StringGateway::int2strBuiltin, 1, 1 },
    { "str2double", Nelson::StringGateway::str2doubleBuiltin, 1, 1 },
    { "mat2str", Nelson::StringGateway::mat2strBuiltin, 1, -2 },
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
