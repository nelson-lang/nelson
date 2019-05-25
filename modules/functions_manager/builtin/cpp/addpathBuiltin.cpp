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
#include "addpathBuiltin.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
#include "StringFormat.hpp"
#include "Warning.hpp"
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
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    bool begin = true;
    bool withOption = false;
    if (argIn.size() > 1) {
        size_t lastpos = argIn.size() - 1;
        ArrayOf lastParam = argIn[lastpos];
        if (lastParam.isRowVectorCharacterArray()) {
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
            Error(StringFormat(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED.c_str(),
                static_cast<int>(lastpos) + 1));
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
        if (param.isRowVectorCharacterArray()) {
            params.push_back(param.getContentAsWideString());
        } else {
            Error(StringFormat(
                ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED.c_str(), static_cast<int>(k) + 1));
        }
    }
    std::wstring previousPaths = PathFuncManager::getInstance()->getPathNameAsString();
    for (const auto& param : params) {
        boost::filesystem::path data_dir(param);
        bool bRes = false;
        try {
            bRes = boost::filesystem::is_directory(data_dir);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
                // ONLY FOR DEBUG
            }
            bRes = false;
        }
        if (bRes) {
            if (PathFuncManager::getInstance()->addPath(param, begin)) {
                stringVector exceptedFunctionsName = eval->getCallers(true);
                PathFuncManager::getInstance()->clearCache(exceptedFunctionsName);
            }
        } else {
            Warning(_W("Warning: Not a directory:") + L" " + param + L"\n");
        }
    }
    if (nLhs == 1) {
        retval.push_back(ArrayOf::characterArrayConstructor(previousPaths));
    }
    return retval;
}
//=============================================================================
