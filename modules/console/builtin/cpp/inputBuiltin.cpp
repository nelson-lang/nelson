//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "inputBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConsoleGateway::inputBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::wstring param1;
    std::wstring param2;
    if (argIn[0].isRowVectorCharacterArray()) {
        param1 = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    if (argIn.size() > 1) {
        if (argIn[1].isRowVectorCharacterArray()) {
            param2 = argIn[1].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
        }
        if (param2.compare(L"s") != 0) {
            Error(_W("Unrecognized option. \"s\" expected."));
        }
    }
    Interface* io = eval->getInterface();
    std::wstring curline;
    if (io) {
        curline = io->getInput(param1);
    }
    ArrayOfVector retval(1);
    retval << ArrayOf::characterArrayConstructor(curline);
    return retval;
}
//=============================================================================
