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
#include "modulepathBuiltin.hpp"
#include "Error.hpp"
#include "ModulePath.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define STR_OPTION_ETC L"etc"
#define STR_OPTION_BIN L"bin"
#define STR_OPTION_ROOT L"root"
#define STR_OPTION_BUILTIN L"builtin"
#define STR_OPTION_SCRIPTS L"functions"
#define STR_OPTION_TESTS L"tests"

//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::modulepathBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // st = modulepath('existing_module')
    // return a struct with different paths of an existing module (shortname)
    // p = modulepath('path', 'module short name', 'etc')
    // p = modulepath('path', 'module short name', 'bin')
    // p = modulepath('path', 'module short name', 'root')
    // p = modulepath('path', 'module short name', 'builtin')
    ArrayOfVector retval;
    if ((argIn.size() != 1) && (argIn.size() != 3)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 1) {
        std::wstring moduleshortname = L"";
        if (argIn[0].isRowVectorCharacterArray()) {
            moduleshortname = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        if (IsExistingModuleName(moduleshortname)) {
            retval.push_back(ArrayOf::characterArrayConstructor(GetModulePath(moduleshortname)));
        } else {
            Error(_W("invalid module name."));
        }
    } else // argIn.size() == 3
    {
        std::wstring modulerootpath = L"";
        std::wstring moduleshortname = L"";
        std::wstring option = L"";
        if (argIn[0].isRowVectorCharacterArray()) {
            modulerootpath = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        if (argIn[1].isRowVectorCharacterArray()) {
            moduleshortname = argIn[1].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
        }
        if (argIn[2].isRowVectorCharacterArray()) {
            option = argIn[2].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_3_TYPE_STRING_EXPECTED);
        }
        if (option.compare(STR_OPTION_ETC) == 0) {
            retval.push_back(ArrayOf::characterArrayConstructor(
                ModulePath(modulerootpath, moduleshortname, GET_ETC_PATH)));
        } else if (option.compare(STR_OPTION_BIN) == 0) {
            retval.push_back(ArrayOf::characterArrayConstructor(
                ModulePath(modulerootpath, moduleshortname, GET_BINARY_PATH)));
        } else if (option.compare(STR_OPTION_ROOT) == 0) {
            retval.push_back(ArrayOf::characterArrayConstructor(
                ModulePath(modulerootpath, moduleshortname, GET_ROOT_PATH)));
        } else if (option.compare(STR_OPTION_BUILTIN) == 0) {
            retval.push_back(ArrayOf::characterArrayConstructor(
                ModulePath(modulerootpath, moduleshortname, GET_DYNLIB_FULLPATH)));
        } else if (option.compare(STR_OPTION_SCRIPTS) == 0) {
            retval.push_back(ArrayOf::characterArrayConstructor(
                ModulePath(modulerootpath, moduleshortname, GET_SCRIPT_PATH)));
        } else {
            Error(_W("Argument #3 must be a valid option."));
        }
    }
    return retval;
}
//=============================================================================
