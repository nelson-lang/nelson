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
#define _SCL_SECURE_NO_WARNINGS
#include "pathBuiltin.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
#include "ToCellString.hpp"
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::pathBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        if (nLhs == 0) {
            wstringVector list = PathFuncManager::getInstance()->getPathNameVector();
            Interface* io = eval->getInterface();
            if (io) {
                if (list.size() == 0) {
                    io->outputMessage(_W("The path is empty. Please restore path.") + L"\n");
                } else {
                    io->outputMessage(
                        _W("Nelson's search path contains the following directories:") + L"\n");
                    io->outputMessage(L"\n");
                    for (size_t k = 0; k < list.size(); ++k) {
                        io->outputMessage(L"	" + list[k] + L"\n");
                    }
                }
            } else {
                retval.push_back(ToCellStringAsColumn(list));
            }
        } else {
            retval.push_back(ArrayOf::characterArrayConstructor(
                PathFuncManager::getInstance()->getPathNameAsString()));
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
        boost::split(paths, p, boost::is_any_of(L";"));
#else
        boost::split(paths, p, boost::is_any_of(L":"));
#endif
        PathFuncManager::getInstance()->clearUserPath();
        PathFuncManager::getInstance()->clear();
        wstringVector::reverse_iterator it;
        for (it = paths.rbegin(); it != paths.rend(); ++it) {
            PathFuncManager::getInstance()->addPath(*it, true);
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
        boost::split(paths1, p1, boost::is_any_of(L";"));
#else
        boost::split(paths1, p1, boost::is_any_of(L":"));
#endif
        wstringVector paths2;
#ifdef _MSC_VER
        boost::split(paths2, p2, boost::is_any_of(L";"));
#else
        boost::split(paths2, p2, boost::is_any_of(L":"));
#endif
        PathFuncManager::getInstance()->clearUserPath();
        PathFuncManager::getInstance()->clear();
        wstringVector::reverse_iterator rit;
        wstringVector::iterator it;
        for (rit = paths1.rbegin(); rit != paths1.rend(); ++rit) {
            PathFuncManager::getInstance()->addPath(*rit, true);
        }
        for (it = paths2.begin(); it != paths2.end(); ++it) {
            PathFuncManager::getInstance()->addPath(*it, false);
        }
    }
    return retval;
}
//=============================================================================
