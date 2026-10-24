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
#include "ErrorID.hpp"
#include "EvaluateScriptFile.hpp"
#include "EvaluateCommand.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
#include <cwctype>
//=============================================================================
using namespace Nelson;
//=============================================================================
// A Notebook document (.nlx) is not a plain script: it is handed to the
// notebook module's builtin, resolved at run time (core does not depend on
// that optional module).
static bool
isNotebookDocument(const std::wstring& filename)
{
    if (filename.size() < 4) {
        return false;
    }
    std::wstring ext = filename.substr(filename.size() - 4);
    std::transform(ext.begin(), ext.end(), ext.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    return ext == L".nlx";
}
//=============================================================================
static std::wstring
quoteForNelson(const std::wstring& filename)
{
    std::wstring escaped;
    for (const wchar_t c : filename) {
        escaped += c;
        if (c == L'\'') {
            escaped += L'\'';
        }
    }
    return L"notebook('" + escaped + L"');";
}
//=============================================================================
static ArrayOfVector
runBuiltinCommon(
    Evaluator* eval, const std::wstring& filename, bool errorCatch, bool changeDirectory)
{
    ArrayOfVector retval;
    bool bSuccess = false;
    try {
        if (isNotebookDocument(filename)) {
            bSuccess = EvaluateCommand(eval, quoteForNelson(filename), false);
        } else {
            bSuccess = EvaluateScriptFile(eval, filename, changeDirectory);
        }
    } catch (NelsonException& e) {
        if (eval->getState() == NLS_STATE_ABORT || eval->isQuitOrForceQuitState()) {
            throw;
        }
        if (errorCatch) {
            eval->setLastErrorException(e);
            bSuccess = false;
        } else {
            throw;
        }
    }
    if (errorCatch) {
        retval << ArrayOf::logicalScalar(bSuccess);
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
        bChangeDir = (argIn[2].asLogicalScalar() == 0) ? false : true;
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"logical");
    }
    if (argIn[1].isRowVectorCharacterArray()) {
        std::wstring arg2 = argIn[1].asWideString();
        if (arg2.compare(L"errcatch") == 0) {
            bErrorCatch = true;
            nargoutcheck(nLhs, 0, 1);
        } else if (arg2.compare(L"nocatch") == 0) {
            bErrorCatch = false;
        } else {
            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (argIn[0].isRowVectorCharacterArray()) {
        wpath = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
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
        bChangeDir = (argIn[1].asLogicalScalar() == 0) ? false : true;
    } else {
        if (argIn[1].isRowVectorCharacterArray()) {
            std::wstring arg2 = argIn[1].asWideString();
            if (arg2.compare(L"errcatch") == 0) {
                bErrorCatch = true;
                nargoutcheck(nLhs, 0, 1);
            } else if (arg2.compare(L"nocatch") == 0) {
                bErrorCatch = false;
            } else {
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"logical or string");
        }
    }
    if (argIn[0].isRowVectorCharacterArray()) {
        wpath = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
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
        wpath = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    return runBuiltinCommon(eval, wpath, false, true);
}
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::runBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 3);
    if (argIn.size() == 1) {
        return runBuiltinOneRhs(eval, nLhs, argIn);
    }
    if (argIn.size() == 2) {
        return runBuiltinTwoRhs(eval, nLhs, argIn);
    }
    if (argIn.size() == 3) {
        return runBuiltinThreeRhs(eval, nLhs, argIn);
    }
    return retval;
}
//=============================================================================
