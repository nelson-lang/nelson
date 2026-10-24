//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "clearvarsBuiltin.hpp"
#include "ClearVariables.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isStringArgument(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
}
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::clearvarsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    wstringVector arguments;
    arguments.reserve(argIn.size());
    for (size_t k = 0; k < argIn.size(); k++) {
        if (!isStringArgument(argIn[k])) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (k + 1),
                L"string");
        }
        arguments.push_back(argIn[k].asWideString());
    }
    ClearVariables(eval, arguments);
    return retval;
}
//=============================================================================
