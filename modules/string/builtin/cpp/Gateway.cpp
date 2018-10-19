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
#include "containsBuiltin.hpp"
#include "countBuiltin.hpp"
#include "endsWithBuiltin.hpp"
#include "int2strBuiltin.hpp"
#include "mat2strBuiltin.hpp"
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
#include "strtrimBuiltin.hpp"
#include "stringBuiltin.hpp"
#include "stringsBuiltin.hpp"
#include "deblankBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"string";
//=============================================================================
static const nlsGateway gateway[] = { { "char", Nelson::StringGateway::charBuiltin, 1, -1,
                                          CPP_BUILTIN_WITH_EVALUATOR },
    { "strcmp", Nelson::StringGateway::strcmpBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "strcmpi", Nelson::StringGateway::strcmpiBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "strncmp", Nelson::StringGateway::strncmpBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "strncmpi", Nelson::StringGateway::strncmpiBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "tolower", Nelson::StringGateway::tolowerBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "lower", Nelson::StringGateway::tolowerBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "toupper", Nelson::StringGateway::toupperBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "upper", Nelson::StringGateway::toupperBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "strfind", Nelson::StringGateway::strfindBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "sprintf", Nelson::StringGateway::sprintfBuiltin, 2, -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "int2str", Nelson::StringGateway::int2strBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "str2double", Nelson::StringGateway::str2doubleBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "mat2str", Nelson::StringGateway::mat2strBuiltin, 1, -2, CPP_BUILTIN_WITH_EVALUATOR },
    { "startsWith", Nelson::StringGateway::startsWithBuiltin, 1, -3, CPP_BUILTIN_WITH_EVALUATOR },
    { "endsWith", Nelson::StringGateway::endsWithBuiltin, 1, -3, CPP_BUILTIN_WITH_EVALUATOR },
    { "contains", Nelson::StringGateway::containsBuiltin, 1, -3, CPP_BUILTIN_WITH_EVALUATOR },
    { "count", Nelson::StringGateway::countBuiltin, 1, -3, CPP_BUILTIN_WITH_EVALUATOR },
    { "strrep", Nelson::StringGateway::strrepBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "replace", Nelson::StringGateway::replaceBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "strtrim", Nelson::StringGateway::strtrimBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "deblank", Nelson::StringGateway::deblankBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "strlength", Nelson::StringGateway::strlengthBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "string", Nelson::StringGateway::stringBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "strings", Nelson::StringGateway::stringsBuiltin, 1, -1, CPP_BUILTIN_WITH_EVALUATOR } };
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
