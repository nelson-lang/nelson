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
#include "charBuiltin.hpp"
#include "char_dispBuiltin.hpp"
#include "char_horzcat_charBuiltin.hpp"
#include "char_isequalBuiltin.hpp"
#include "char_vertcat_charBuiltin.hpp"
#include "containsBuiltin.hpp"
#include "countBuiltin.hpp"
#include "endsWithBuiltin.hpp"
#include "int2strBuiltin.hpp"
#include "mat2strBuiltin.hpp"
#include "ndarraychar_dispBuiltin.hpp"
#include "ndarraychar_horzcat_ndarraycharBuiltin.hpp"
#include "ndarraychar_isequalBuiltin.hpp"
#include "ndarraychar_vertcat_ndarraycharBuiltin.hpp"
#include "replaceBuiltin.hpp"
#include "sprintfBuiltin.hpp"
#include "startsWithBuiltin.hpp"
#include "str2doubleBuiltin.hpp"
#include "strcmpBuiltin.hpp"
#include "strfindBuiltin.hpp"
#include "strlengthBuiltin.hpp"
#include "strncmpBuiltin.hpp"
#include "strrepBuiltin.hpp"
#include "tolowerBuiltin.hpp"
#include "toupperBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"string";
//=============================================================================
static const nlsGateway gateway[] = {
    { "char", Nelson::StringGateway::charBuiltin, 1, -1 },
    { "strcmp", Nelson::StringGateway::strcmpBuiltin, 1, 2 },
    { "strcmpi", Nelson::StringGateway::strcmpiBuiltin, 1, 2 },
    { "strncmp", Nelson::StringGateway::strncmpBuiltin, 1, 3 },
    { "strncmpi", Nelson::StringGateway::strncmpiBuiltin, 1, 3 },
    { "char_disp", Nelson::StringGateway::char_dispBuiltin, 0, 1 },
    { "tolower", Nelson::StringGateway::tolowerBuiltin, 1, 1 },
    { "lower", Nelson::StringGateway::tolowerBuiltin, 1, 1 },
    { "toupper", Nelson::StringGateway::toupperBuiltin, 1, 1 },
    { "upper", Nelson::StringGateway::toupperBuiltin, 1, 1 },
    { "strfind", Nelson::StringGateway::strfindBuiltin, 1, 2 },
    { "char_vertcat_char", Nelson::StringGateway::char_vertcat_charBuiltin, 1, 2 },
    { "char_horzcat_char", Nelson::StringGateway::char_horzcat_charBuiltin, 1, 2 },
    { "char_isequal", Nelson::StringGateway::char_isequalBuiltin, 1, 2 },
    { "char_isequaln", Nelson::StringGateway::char_isequalBuiltin, 1, 2 },
    { "sprintf", Nelson::StringGateway::sprintfBuiltin, 2, -1 },
    { "int2str", Nelson::StringGateway::int2strBuiltin, 1, 1 },
    { "str2double", Nelson::StringGateway::str2doubleBuiltin, 1, 1 },
    { "mat2str", Nelson::StringGateway::mat2strBuiltin, 1, -2 },
    { "ndarraychar_disp", Nelson::StringGateway::ndarraychar_dispBuiltin, 0, 1 },
    { "ndarraychar_isequal", Nelson::StringGateway::ndarraychar_isequalBuiltin, 1, 2 },
    { "ndarraychar_isequaln", Nelson::StringGateway::ndarraychar_isequalBuiltin, 1, 2 },
    { "ndarraychar_vertcat_ndarraychar",
        Nelson::StringGateway::ndarraychar_vertcat_ndarraycharBuiltin, 1, 2 },
    { "ndarraychar_horzcat_ndarraychar",
        Nelson::StringGateway::ndarraychar_horzcat_ndarraycharBuiltin, 1, 2 },
    { "startsWith", Nelson::StringGateway::startsWithBuiltin, 1, -3 },
    { "endsWith", Nelson::StringGateway::endsWithBuiltin, 1, -3 },
    { "contains", Nelson::StringGateway::containsBuiltin, 1, -3 },
    { "count", Nelson::StringGateway::countBuiltin, 1, -3 },
    { "strrep", Nelson::StringGateway::strrepBuiltin, 1, 3 },
    { "replace", Nelson::StringGateway::replaceBuiltin, 1, 3 },
    { "strlength", Nelson::StringGateway::strlengthBuiltin, 1, 1 },
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
