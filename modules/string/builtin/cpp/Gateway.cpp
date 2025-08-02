//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
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
#include "isletterBuiltin.hpp"
#include "joinBuiltin.hpp"
#include "strjustBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"string";
//=============================================================================
static const nlsGateway gateway[] = {
    { "sprintf", (ptrBuiltin)Nelson::StringGateway::sprintfBuiltin, 2, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "char", (ptrBuiltin)Nelson::StringGateway::charBuiltin, 1, -1 },
    { "strcmp", (ptrBuiltin)Nelson::StringGateway::strcmpBuiltin, 1, 2 },
    { "strcmpi", (ptrBuiltin)Nelson::StringGateway::strcmpiBuiltin, 1, 2 },
    { "strncmp", (ptrBuiltin)Nelson::StringGateway::strncmpBuiltin, 1, 3 },
    { "strncmpi", (ptrBuiltin)Nelson::StringGateway::strncmpiBuiltin, 1, 3 },
    { "matches", (ptrBuiltin)Nelson::StringGateway::matchesBuiltin, 1, 2 },
    { "tolower", (ptrBuiltin)Nelson::StringGateway::tolowerBuiltin, 1, 1 },
    { "lower", (ptrBuiltin)Nelson::StringGateway::tolowerBuiltin, 1, 1 },
    { "toupper", (ptrBuiltin)Nelson::StringGateway::toupperBuiltin, 1, 1 },
    { "upper", (ptrBuiltin)Nelson::StringGateway::toupperBuiltin, 1, 1 },
    { "strfind", (ptrBuiltin)Nelson::StringGateway::strfindBuiltin, 1, 2 },
    { "int2str", (ptrBuiltin)Nelson::StringGateway::int2strBuiltin, 1, 1 },
    { "num2str", (ptrBuiltin)Nelson::StringGateway::num2strBuiltin, 1, 2 },
    { "str2double", (ptrBuiltin)Nelson::StringGateway::str2doubleBuiltin, 1, 1 },
    { "mat2str", (ptrBuiltin)Nelson::StringGateway::mat2strBuiltin, 1, -2 },
    { "startsWith", (ptrBuiltin)Nelson::StringGateway::startsWithBuiltin, 1, -3 },
    { "endsWith", (ptrBuiltin)Nelson::StringGateway::endsWithBuiltin, 1, -3 },
    { "contains", (ptrBuiltin)Nelson::StringGateway::containsBuiltin, 1, -3 },
    { "count", (ptrBuiltin)Nelson::StringGateway::countBuiltin, 1, -3 },
    { "strrep", (ptrBuiltin)Nelson::StringGateway::strrepBuiltin, 1, 3 },
    { "replace", (ptrBuiltin)Nelson::StringGateway::replaceBuiltin, 1, 3 },
    { "strtrim", (ptrBuiltin)Nelson::StringGateway::strtrimBuiltin, 1, 1 },
    { "deblank", (ptrBuiltin)Nelson::StringGateway::deblankBuiltin, 1, 1 },
    { "strlength", (ptrBuiltin)Nelson::StringGateway::strlengthBuiltin, 1, 1 },
    { "string", (ptrBuiltin)Nelson::StringGateway::stringBuiltin, 1, 1 },
    { "strings", (ptrBuiltin)Nelson::StringGateway::stringsBuiltin, 1, -1 },
    { "convertStringsToChars", (ptrBuiltin)Nelson::StringGateway::convertStringsToCharsBuiltin, -1,
        -1 },
    { "convertCharsToStrings", (ptrBuiltin)Nelson::StringGateway::convertCharsToStringsBuiltin, -1,
        -1 },
    { "blanks", (ptrBuiltin)Nelson::StringGateway::blanksBuiltin, 1, 1 },
    { "strcat", (ptrBuiltin)Nelson::StringGateway::strcatBuiltin, 1, -1 },
    { "append", (ptrBuiltin)Nelson::StringGateway::appendBuiltin, 1, -1 },
    { "isletter", (ptrBuiltin)Nelson::StringGateway::isletterBuiltin, 1, 1 },
    { "join", (ptrBuiltin)Nelson::StringGateway::joinBuiltin, 1, -2 },
    { "strjust", (ptrBuiltin)Nelson::StringGateway::strjustBuiltin, 1, 2 },
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
