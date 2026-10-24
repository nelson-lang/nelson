//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "argvBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::EngineGateway::argvBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.empty()) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
            eval->getCommandLineArguments());
        return retval;
    }
    if (!(argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray())) {
        Error(L"Nelson:argv:InvalidArgument", L"Wrong type for argument #%d: %s expected.", (1),
            L"string");
    }
    std::wstring option = argIn[0].asWideString();
    if (option != L"user") {
        Error(L"Nelson:argv:InvalidValue", L"Wrong value for argument #%d: %s expected.", 1,
            L"'user'");
    }
    wstringVector userCommandLineArguments = eval->getUserCommandLineArguments();
    if (userCommandLineArguments.empty()) {
        retval << ArrayOf::emptyCell(Dimensions(0, 0));
        return retval;
    }
    retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(userCommandLineArguments);
    return retval;
}
//=============================================================================
