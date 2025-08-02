//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "modulepathBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ModulePath.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define STR_OPTION_ETC L"etc"
#define STR_OPTION_BIN L"bin"
#define STR_OPTION_ROOT L"root"
#define STR_OPTION_BUILTIN L"builtin"
#define STR_OPTION_FUNCTIONS L"functions"
#define STR_OPTION_TESTS L"tests"
//=============================================================================
static bool
isModulePathOption(const std::wstring& optionStr, MODULEPATH_OPTION& option);
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::modulepathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // p = modulepath('existing_module')
    // return path of an existing module (shortname) equivalent to
    // p = modulepath('module short name', 'root')
    //
    // p = modulepath('module short name', 'etc')
    // p = modulepath('module short name', 'bin')
    // p = modulepath('module short name', 'root')
    // p = modulepath('module short name', 'builtin')
    // p = modulepath('module short name', 'functions')
    // p = modulepath('module short name', 'tests')

    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);

    switch (argIn.size()) {
    case 1: {
        std::wstring moduleshortname;
        if (argIn[0].isRowVectorCharacterArray()
            || (argIn[0].isStringArray() && argIn[0].isScalar())) {
            moduleshortname = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        retval << ArrayOf::characterArrayConstructor(ModulePath(moduleshortname));
    } break;
    case 2: {
        std::wstring moduleshortname;
        if (argIn[0].isRowVectorCharacterArray()
            || (argIn[0].isStringArray() && argIn[0].isScalar())) {
            moduleshortname = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring option;
        if (argIn[1].isRowVectorCharacterArray()
            || (argIn[1].isStringArray() && argIn[1].isScalar())) {
            option = argIn[1].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
        }
        MODULEPATH_OPTION modulePathOption;
        if (!isModulePathOption(option, modulePathOption)) {
            Error(_W("Argument #2 must be a valid option."));
        }
        retval << ArrayOf::characterArrayConstructor(ModulePath(moduleshortname, modulePathOption));
    } break;
    default: {
    } break;
    }
    return retval;
}
//=============================================================================
bool
isModulePathOption(const std::wstring& optionStr, MODULEPATH_OPTION& option)
{
    if (optionStr == STR_OPTION_ETC) {
        option = GET_ETC_PATH;
        return true;
    } else if (optionStr == STR_OPTION_BIN) {
        option = GET_BINARY_PATH;
        return true;
    } else if (optionStr == STR_OPTION_ROOT) {
        option = GET_ROOT_PATH;
        return true;
    } else if (optionStr == STR_OPTION_BUILTIN) {
        option = GET_LIBRARY_FULLPATH;
        return true;
    } else if (optionStr == STR_OPTION_FUNCTIONS) {
        option = GET_FUNCTIONS_PATH;
        return true;
    } else if (optionStr == STR_OPTION_TESTS) {
        option = GET_TESTS_PATH;
        return true;
    }
    return false;
}
//=============================================================================
