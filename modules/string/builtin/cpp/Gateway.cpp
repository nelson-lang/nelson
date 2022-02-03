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
#include "charBuiltin.hpp"
#include "containsBuiltin.hpp"
#include "countBuiltin.hpp"
#include "endsWithBuiltin.hpp"
#include "int2strBuiltin.hpp"
#include "num2strBuiltin.hpp"
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
#include "convertStringsToCharsBuiltin.hpp"
#include "convertCharsToStringsBuiltin.hpp"
#include "blanksBuiltin.hpp"
#include "matchesBuiltin.hpp"
#include "strcatBuiltin.hpp"
#include "appendBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"string";
//=============================================================================
static const nlsGateway gateway[] = {
    { "char", (ptrBuiltin)Nelson::StringGateway::charBuiltin, 1, -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "strcmp", (ptrBuiltin)Nelson::StringGateway::strcmpBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "strcmpi", (ptrBuiltin)Nelson::StringGateway::strcmpiBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "strncmp", (ptrBuiltin)Nelson::StringGateway::strncmpBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "strncmpi", (ptrBuiltin)Nelson::StringGateway::strncmpiBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "matches", (ptrBuiltin)Nelson::StringGateway::matchesBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "tolower", (ptrBuiltin)Nelson::StringGateway::tolowerBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lower", (ptrBuiltin)Nelson::StringGateway::tolowerBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "toupper", (ptrBuiltin)Nelson::StringGateway::toupperBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "upper", (ptrBuiltin)Nelson::StringGateway::toupperBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "strfind", (ptrBuiltin)Nelson::StringGateway::strfindBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sprintf", (ptrBuiltin)Nelson::StringGateway::sprintfBuiltin, 2, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "int2str", (ptrBuiltin)Nelson::StringGateway::int2strBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "num2str", (ptrBuiltin)Nelson::StringGateway::num2strBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "str2double", (ptrBuiltin)Nelson::StringGateway::str2doubleBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mat2str", (ptrBuiltin)Nelson::StringGateway::mat2strBuiltin, 1, -2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "startsWith", (ptrBuiltin)Nelson::StringGateway::startsWithBuiltin, 1, -3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "endsWith", (ptrBuiltin)Nelson::StringGateway::endsWithBuiltin, 1, -3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "contains", (ptrBuiltin)Nelson::StringGateway::containsBuiltin, 1, -3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "count", (ptrBuiltin)Nelson::StringGateway::countBuiltin, 1, -3, CPP_BUILTIN_WITH_EVALUATOR },
    { "strrep", (ptrBuiltin)Nelson::StringGateway::strrepBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "replace", (ptrBuiltin)Nelson::StringGateway::replaceBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "strtrim", (ptrBuiltin)Nelson::StringGateway::strtrimBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "deblank", (ptrBuiltin)Nelson::StringGateway::deblankBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "strlength", (ptrBuiltin)Nelson::StringGateway::strlengthBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "string", (ptrBuiltin)Nelson::StringGateway::stringBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "strings", (ptrBuiltin)Nelson::StringGateway::stringsBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "convertStringsToChars", (ptrBuiltin)Nelson::StringGateway::convertStringsToCharsBuiltin, -1,
        -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "convertCharsToStrings", (ptrBuiltin)Nelson::StringGateway::convertCharsToStringsBuiltin, -1,
        -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "blanks", (ptrBuiltin)Nelson::StringGateway::blanksBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "strcat", (ptrBuiltin)Nelson::StringGateway::strcatBuiltin, 1, -1, CPP_BUILTIN },
    { "append", (ptrBuiltin)Nelson::StringGateway::appendBuiltin, 1, -1, CPP_BUILTIN },
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
