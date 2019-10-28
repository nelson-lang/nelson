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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "execstrBuiltin.hpp"
#include "Error.hpp"
#include "EvaluateCommand.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::execstrBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bErrorCatch = false;
    if (argIn.empty() || argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    std::wstring line;
    if (argIn[0].isRowVectorCharacterArray()) {
        line = argIn[0].getContentAsWideString();
    } else {
        Error(_W("#1 string expected."));
    }
    if (argIn.size() > 1) {
        if (argIn[1].isRowVectorCharacterArray()) {
            std::wstring catchstr;
            catchstr = argIn[1].getContentAsWideString();
            if ((catchstr == L"errcatch") || (catchstr == L"nocatch")) {
                if (catchstr == L"errcatch") {
                    bErrorCatch = true;
                } else {
                    bErrorCatch = false;
                }
            } else {
                Error(_W("#2 'errcatch' or 'nocatch' expected."));
            }
        } else {
            Error(_W("#2 string expected."));
        }
    }
    if (bErrorCatch) {
        bool bRes = true;
        try {
            EvaluateCommand(eval, line, true);
        } catch (const Exception&) {
            bRes = false;
        }
        retval.push_back(ArrayOf::logicalConstructor(bRes));
    } else {
        EvaluateCommand(eval, line, false);
    }
    return retval;
}
//=============================================================================
