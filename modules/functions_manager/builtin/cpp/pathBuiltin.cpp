//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "pathBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PathFuncManager.hpp"
#include "ToCellString.hpp"
#include "NelsonPrint.hpp"
#include "StringHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::pathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 0) {
        if (nLhs == 0) {
            wstringVector list = PathFuncManager::getInstance()->getPathNameVector();
            if (list.empty()) {
                NelsonPrint(_W("The path is empty. Please restore path.") + L"\n");
            } else {
                NelsonPrint(_W("Nelson's search path contains the following directories:") + L"\n");
                NelsonPrint(L"\n");
                for (auto& k : list) {
                    NelsonPrint(L"	" + k + L"\n");
                }
            }
        } else {
            retval << ArrayOf::characterArrayConstructor(
                PathFuncManager::getInstance()->getPathNameAsString());
        }
    }
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        if (!param1.isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring p = param1.getContentAsWideString();
        wstringVector paths;
#ifdef _MSC_VER
        StringHelpers::split(paths, p, L';');
#else
        StringHelpers::split(paths, p, L':');
#endif
        PathFuncManager::getInstance()->clearUserPath();
        PathFuncManager::getInstance()->clear();
        wstringVector::reverse_iterator it;
        for (it = paths.rbegin(); it != paths.rend(); ++it) {
            PathFuncManager::getInstance()->addPath(*it, true, false);
        }
    }
    if (argIn.size() == 2) {
        ArrayOf param1 = argIn[0];
        if (!param1.isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        ArrayOf param2 = argIn[1];
        if (!param2.isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
        }
        std::wstring p1 = param1.getContentAsWideString();
        std::wstring p2 = param2.getContentAsWideString();
        wstringVector paths1;
#ifdef _MSC_VER
        StringHelpers::split(paths1, p1, L';');
#else
        StringHelpers::split(paths1, p1, L':');
#endif
        wstringVector paths2;
#ifdef _MSC_VER
        StringHelpers::split(paths2, p2, L';');
#else
        StringHelpers::split(paths2, p2, L':');
#endif
        PathFuncManager::getInstance()->clearUserPath();
        PathFuncManager::getInstance()->clear();
        wstringVector::reverse_iterator rit;
        wstringVector::iterator it;
        for (rit = paths1.rbegin(); rit != paths1.rend(); ++rit) {
            PathFuncManager::getInstance()->addPath(*rit, true, false);
        }
        for (it = paths2.begin(); it != paths2.end(); ++it) {
            PathFuncManager::getInstance()->addPath(*it, false, false);
        }
    }
    return retval;
}
//=============================================================================
