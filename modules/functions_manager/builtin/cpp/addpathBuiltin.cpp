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
#include "addpathBuiltin.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
#include "StringFormat.hpp"
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::addpathBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    bool begin = true;
    bool withOption = false;
    if (argIn.size() > 1) {
        size_t lastpos = argIn.size() - 1;
        ArrayOf lastParam = argIn[lastpos];
        if (lastParam.isColonVectorCharacterArray()) {
            std::wstring option = lastParam.getContentAsWideString();
            if ((option == L"-begin") || (option == L"-end")) {
                if (option == L"-begin") {
                    begin = true;
                } else {
                    begin = false;
                }
                withOption = true;
            } else {
                withOption = false;
            }
        } else {
            Error(eval,
                StringFormat(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED.c_str(), lastpos + 1));
        }
    }
    wstringVector params;
    size_t lastpos;
    if (withOption) {
        lastpos = argIn.size() - 1;
    } else {
        lastpos = argIn.size();
    }
    for (size_t k = 0; k < lastpos; k++) {
        ArrayOf param = argIn[k];
        if (param.isColonVectorCharacterArray()) {
            params.push_back(param.getContentAsWideString());
        } else {
            Error(eval, StringFormat(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED.c_str(), k + 1));
        }
    }
    std::wstring previousPaths = PathFuncManager::getInstance()->getPathNameAsString();
    for (size_t k = 0; k < params.size(); k++) {
        boost::filesystem::path data_dir(params[k]);
        bool bRes = false;
        try {
            bRes = boost::filesystem::is_directory(data_dir);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
            }
            bRes = false;
        }
        if (bRes) {
            if (PathFuncManager::getInstance()->addPath(params[k], begin)) {
                stringVector exceptedFunctionsName = eval->getCallers(true);
                PathFuncManager::getInstance()->clearCache(exceptedFunctionsName);
            }
        } else {
            Interface* io = eval->getInterface();
            if (io) {
                io->warningMessage(_W("Warning: Not a directory:") + L" " + params[k] + L"\n");
            }
        }
    }
    if (nLhs == 1) {
        retval.push_back(ArrayOf::characterArrayConstructor(previousPaths));
    }
    return retval;
}
//=============================================================================
