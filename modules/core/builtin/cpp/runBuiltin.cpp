//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "runBuiltin.hpp"
#include "Error.hpp"
#include "EvaluateScriptFile.hpp"
#include "StringFormat.hpp"
#include "characters_encoding.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
runBuiltinThreeRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bErrorCatch = false;
    bool bChangeDir = true;
    std::wstring wpath;
    if (argIn[2].isLogical()) {
        bChangeDir = (argIn[2].getContentAsLogicalScalar() == 0) ? false : true;
    } else {
        Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_LOGICAL_EXPECTED);
    }
    if (argIn[1].isColonVectorCharacterArray()) {
        std::wstring arg2 = argIn[1].getContentAsWideString();
        if (arg2.compare(L"errcatch") == 0) {
            bErrorCatch = true;
            if (nLhs > 1) {
                Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
        } else if (arg2.compare(L"nocatch") == 0) {
            bErrorCatch = false;
        } else {
            Error(eval, ERROR_WRONG_ARGUMENT_2_VALUE);
        }
    } else {
        Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    if (argIn[0].isColonVectorCharacterArray()) {
        wpath = argIn[0].getContentAsWideString();
    } else {
        Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    bool bSuccess = false;
    try {
        bSuccess = EvaluateScriptFile(eval, wpath.c_str(), bChangeDir);
    } catch (Exception& e) {
        eval->setLastException(e);
        std::string fname = wstring_to_utf8(wpath);
        if (bErrorCatch) {
            e.what();
            bSuccess = false;
        } else {
            Interface* io = eval->getInterface();
            e.printMe(io);
            std::string str;
            if (fname.size() > 50) {
                str = StringFormat(
                    _("at line %5d\nof \'%s\'\n").c_str(), e.getLine(), fname.c_str());
            } else {
                str = StringFormat(
                    _("at line %5d of \'%s\'\n").c_str(), e.getLine(), fname.c_str());
            }
            io->errorMessage(str);
            throw Exception("");
        }
    }
    if (bErrorCatch) {
        retval.push_back(ArrayOf::logicalConstructor(bSuccess));
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
runBuiltinTwoRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bErrorCatch = false;
    bool bChangeDir = true;
    std::wstring wpath;
    if (argIn[1].isLogical()) {
        if (nLhs != 0) {
            Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        bChangeDir = (argIn[1].getContentAsLogicalScalar() == 0) ? false : true;
    } else {
        if (argIn[1].isColonVectorCharacterArray()) {
            std::wstring arg2 = argIn[1].getContentAsWideString();
            if (arg2.compare(L"errcatch") == 0) {
                bErrorCatch = true;
                if (nLhs > 1) {
                    Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
            } else if (arg2.compare(L"nocatch") == 0) {
                bErrorCatch = false;
            } else {
                Error(eval, ERROR_WRONG_ARGUMENT_2_VALUE);
            }
        } else {
            Error(eval, ERROR_WRONG_ARGUMENT_2_TYPE);
        }
    }
    if (argIn[0].isColonVectorCharacterArray()) {
        wpath = argIn[0].getContentAsWideString();
    } else {
        Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    bool bSuccess = false;
    try {
        bSuccess = EvaluateScriptFile(eval, wpath.c_str(), bChangeDir);
    } catch (Exception& e) {
        eval->setLastException(e);
        std::string fname = wstring_to_utf8(wpath);
        if (bErrorCatch) {
            e.what();
            bSuccess = false;
        } else {
            Interface* io = eval->getInterface();
            e.printMe(io);
            std::string str;
            if (fname.size() > 50) {
                str = StringFormat(
                    _("at line %5d\nof \'%s\'\n").c_str(), e.getLine(), fname.c_str());
            } else {
                str = StringFormat(
                    _("at line %5d of \'%s\'\n").c_str(), e.getLine(), fname.c_str());
            }
            io->errorMessage(str);
            throw Exception("");
        }
    }
    if (bErrorCatch) {
        retval.push_back(ArrayOf::logicalConstructor(bSuccess));
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
runBuiltinOneRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs != 0) {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn[0].isColonVectorCharacterArray()) {
        std::wstring wpath = argIn[0].getContentAsWideString();
        try {
            EvaluateScriptFile(eval, wpath.c_str(), true);
        } catch (Exception& e) {
            eval->setLastException(e);
            std::string fname = wstring_to_utf8(wpath);
            Interface* io = eval->getInterface();
            e.printMe(io);
            std::string str;
            if (fname.size() > 50) {
                str = StringFormat(
                    _("at line %5d\nof \'%s\'\n").c_str(), e.getLine(), fname.c_str());
            } else {
                str = StringFormat(
                    _("at line %5d of \'%s\'\n").c_str(), e.getLine(), fname.c_str());
            }
            io->errorMessage(str);
            throw Exception("");
        }
    } else {
        Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    return retval;
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
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
