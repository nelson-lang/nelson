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
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "execstrBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "EvaluateCommand.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::execstrBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool bErrorCatch = false;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(nLhs);
    std::wstring line;
    if (argIn[0].isRowVectorCharacterArray() || (argIn[0].isStringArray() && argIn[0].isScalar())) {
        line = argIn[0].getContentAsWideString();
    } else {
        raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), 1, L"string");
    }
    if (argIn.size() > 1) {
        if (argIn[1].isRowVectorCharacterArray()
            || (argIn[1].isStringArray() && argIn[1].isScalar())) {
            std::wstring catchstr;
            catchstr = argIn[1].getContentAsWideString();
            if ((catchstr == L"errcatch") || (catchstr == L"nocatch")) {
                if (catchstr == L"errcatch") {
                    bErrorCatch = true;
                } else {
                    bErrorCatch = false;
                }
            } else {
                raiseError(L"Nelson:core:ERROR_EXECSTR_ARG2_ERRCATCH_NOCATCH_EXPECTED",
                    ERROR_EXECSTR_ARG2_ERRCATCH_NOCATCH_EXPECTED);
            }
        } else {
            raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), 2, L"string");
        }
    }
    if (bErrorCatch) {
        bool bRes = true;
        try {
            EvaluateCommand(eval, line, true);
        } catch (const Exception&) {
            bRes = false;
        }
        retval << ArrayOf::logicalConstructor(bRes);
    } else {
        EvaluateCommand(eval, line, false);
    }
    return retval;
}
//=============================================================================
