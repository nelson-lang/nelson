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
#include "runBuiltin.hpp"
#include "Error.hpp"
#include "EvaluateScriptFile.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
        bSuccess = EvaluateScriptFile(eval, filename, changeDirectory);
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
    }
    if (argIn.size() == 2) {
        return runBuiltinTwoRhs(eval, nLhs, argIn);
    }
    if (argIn.size() == 3) {
        return runBuiltinThreeRhs(eval, nLhs, argIn);
    }
    Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);

    return retval;
}
//=============================================================================
