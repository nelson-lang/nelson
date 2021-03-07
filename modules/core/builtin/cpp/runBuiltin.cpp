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
#include "runBuiltin.hpp"
#include "Error.hpp"
#include "EvaluateScriptFile.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
runBuiltinCommon(
    Evaluator* eval, const std::wstring& filename, bool errorCatch, bool changeDirectory)
{
    ArrayOfVector retval;
    bool bSuccess = false;
    try {
        bSuccess = EvaluateScriptFile(eval, filename.c_str(), changeDirectory);
    } catch (Exception& e) {
        if (errorCatch) {
            eval->setLastErrorException(e);
            bSuccess = false;
        } else {
            throw;
        }
    }
    if (errorCatch) {
        retval << ArrayOf::logicalConstructor(bSuccess);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
runBuiltinThreeRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool bErrorCatch = false;
    bool bChangeDir = true;
    std::wstring wpath;
    if (argIn[2].isLogical()) {
        bChangeDir = (argIn[2].getContentAsLogicalScalar() == 0) ? false : true;
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_LOGICAL_EXPECTED);
    }
    if (argIn[1].isRowVectorCharacterArray()) {
        std::wstring arg2 = argIn[1].getContentAsWideString();
        if (arg2.compare(L"errcatch") == 0) {
            bErrorCatch = true;
            nargoutcheck(nLhs, 0, 1);
        } else if (arg2.compare(L"nocatch") == 0) {
            bErrorCatch = false;
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_VALUE);
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    if (argIn[0].isRowVectorCharacterArray()) {
        wpath = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    return runBuiltinCommon(eval, wpath, bErrorCatch, bChangeDir);
}
//=============================================================================
static ArrayOfVector
runBuiltinTwoRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool bErrorCatch = false;
    bool bChangeDir = true;
    std::wstring wpath;
    if (argIn[1].isLogical()) {
        nargoutcheck(nLhs, 0, 0);
        bChangeDir = (argIn[1].getContentAsLogicalScalar() == 0) ? false : true;
    } else {
        if (argIn[1].isRowVectorCharacterArray()) {
            std::wstring arg2 = argIn[1].getContentAsWideString();
            if (arg2.compare(L"errcatch") == 0) {
                bErrorCatch = true;
                nargoutcheck(nLhs, 0, 1);
            } else if (arg2.compare(L"nocatch") == 0) {
                bErrorCatch = false;
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_VALUE);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE);
        }
    }
    if (argIn[0].isRowVectorCharacterArray()) {
        wpath = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    return runBuiltinCommon(eval, wpath, bErrorCatch, bChangeDir);
}
//=============================================================================
static ArrayOfVector
runBuiltinOneRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    std::wstring wpath;
    if (argIn[0].isRowVectorCharacterArray()) {
        wpath = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    return runBuiltinCommon(eval, wpath, false, true);
}
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::runBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 1) {
        return runBuiltinOneRhs(eval, nLhs, argIn);
    } else if (argIn.size() == 2) {
        return runBuiltinTwoRhs(eval, nLhs, argIn);
    } else if (argIn.size() == 3) {
        return runBuiltinThreeRhs(eval, nLhs, argIn);
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
