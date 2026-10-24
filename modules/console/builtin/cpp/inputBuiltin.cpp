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
#include "ErrorID.hpp"
#include "i18n.hpp"
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
        param1 = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (argIn.size() > 1) {
        if (argIn[1].isRowVectorCharacterArray()) {
            param2 = argIn[1].asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"string");
        }
        if (param2.compare(L"s") != 0) {
            Error(L"Nelson:input:unrecognizedOption", L"Unrecognized option. \"s\" expected.");
        }
    }
    Interface* io = eval->getInterface();
    std::wstring curline;
    if (io) {
        curline = io->getInput(param1);
    }
    ArrayOfVector retval(1);
    retval << ArrayOf::characterArray(curline);
    return retval;
}
//=============================================================================
