//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "saveBuiltin.hpp"
#include "Error.hpp"
#include "IsValidVariableName.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFuncManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// save(filename)
// save(filename, '-mat')
// save(filename, '-nh5')
// save(filename, '-nh5', '-append', '-nocompression')
// save(filename, '-mat', '-append', '-nocompression', variables)
// save(filename, '-mat', '-append', '-nocompression', version, variables)
//=============================================================================
static bool
isOption(std::wstring param)
{
    return param.size() > 2 && param[0] == L'-';
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::saveBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    std::wstring paramFilename = argIn[0].getContentAsWideString();
    if (paramFilename.empty()) {
        paramFilename = L"nelson.nh5";
    }
    wstringVector names;
    bool forceAsMat = false;
    bool forceAsNh5 = false;
    bool nocompression = false;
    bool append = false;
    std::wstring version;
    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf paramK = argIn[k];
        std::wstring param = paramK.getContentAsWideString();
        if (isOption(param)) {
            if (param == L"-mat" && !forceAsNh5) {
                forceAsMat = true;
            } else if (param == L"-nh5" && !forceAsMat) {
                forceAsNh5 = true;
            } else if (param == L"-append") {
                append = true;
            } else if (param == L"-nocompression") {
                nocompression = true;
            } else if ((param == L"-v7.3") || (param == L"-v7") || (param == L"-v6")
                || (param == L"-v4")) {
                version = param;
                forceAsMat = true;
            } else {
                Error(_W("Valid option expected."));
            }
        } else {
            if (IsValidVariableName(param)) {
                names.push_back(param);
            } else {
                Error(_W("Valid variable name expected."));
            }
        }
    }

    if (!forceAsMat && !forceAsNh5) {
        std::string extension = boost::filesystem::extension(paramFilename);
        if (extension == ".nh5") {
            forceAsNh5 = true;
        } else if (extension == ".mat") {
            forceAsMat = true;
        } else if (extension == "") {
            forceAsNh5 = true;
            paramFilename = paramFilename + L".nh5";
        } else {
            forceAsNh5 = true;
        }
    }

    std::string saveFunctionName;
    if (forceAsNh5) {
        saveFunctionName = "savenh5";
    } else if (forceAsMat) {
        saveFunctionName = "savemat";
    }
    if (saveFunctionName.empty()) {
        Error(_W("save function expected."));
    }

    FunctionDef* funcDef = nullptr;
    if (!PathFuncManager::getInstance()->find(saveFunctionName, funcDef)) {
        if (!BuiltInFunctionDefManager::getInstance()->find(saveFunctionName, funcDef)) {
            Error(_W("load function expected."));
        }
    }

    ArrayOfVector inputArguments;
    inputArguments.push_back(ArrayOf::characterArrayConstructor(paramFilename));
    for (std::wstring name : names) {
        inputArguments.push_back(ArrayOf::characterArrayConstructor(name));
    }
    if (!version.empty()) {
        inputArguments.push_back(ArrayOf::characterArrayConstructor(version));
    }
    if (append) {
        inputArguments.push_back(ArrayOf::characterArrayConstructor("-append"));
    }
    if (nocompression) {
        inputArguments.push_back(ArrayOf::characterArrayConstructor("-nocompression"));
    }
    return funcDef->evaluateFunction(eval, inputArguments, nLhs);
}
//=============================================================================
