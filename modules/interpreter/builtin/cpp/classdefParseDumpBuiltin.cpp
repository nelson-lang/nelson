//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "classdefParseDumpBuiltin.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Internal diagnostic: returns the deterministic structural rendering of a
// classdef source.
ArrayOfVector
Nelson::InterpreterGateway::classdefParseDumpBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::string source;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        source = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    retval << ArrayOf::characterArray(classdefCanonicalDefinitionDump(source));
    return retval;
}
//=============================================================================
