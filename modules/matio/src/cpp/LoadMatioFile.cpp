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
#include <matio.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "LoadMatioFile.hpp"
#include "LoadMatioVariable.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
LoadMatioFile(
    Evaluator* eval, const std::wstring& filename, const wstringVector& names, bool asStruct)
{
    ArrayOf res;
    boost::filesystem::path mat_filename(filename);
    bool fileExistPreviously = false;
    try {
        fileExistPreviously = boost::filesystem::exists(mat_filename)
            && !boost::filesystem::is_directory(mat_filename);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            Error(_W("Permission denied."));
        }
        fileExistPreviously = false;
    }
    if (!fileExistPreviously) {
        Error(_W("File does not exist."));
    }

    std::string utf8filename = wstring_to_utf8(filename);
    mat_t* matfile = Mat_Open(utf8filename.c_str(), MAT_ACC_RDONLY);
    if (!matfile) {
        Error(_W("Cannot open .mat file."));
    }

    stringVector variableNamesInFile;
    size_t nVars = 0;
    char** variableNames = Mat_GetDir(matfile, &nVars);
    for (size_t k = 0; k < nVars; k++) {
        variableNamesInFile.push_back(variableNames[k]);
    }
    stringVector variablesNamesToRead;
    if (names.empty()) {
        variablesNamesToRead = variableNamesInFile;
    } else {
        for (std::wstring uname : names) {
            std::string name = wstring_to_utf8(uname);
            if (std::find(variableNamesInFile.begin(), variableNamesInFile.end(), name)
                == variableNamesInFile.end()) {
                std::string msg = _("Variable not found:") + std::string(" ") + name;
                Warning(msg);
            }
        }
    }
    ArrayOfVector values;
    for (std::string name : variablesNamesToRead) {
        ArrayOf value;
        matvar_t* matVariable = Mat_VarRead(matfile, name.c_str());
        if (matVariable == nullptr) {
            Warning(WARNING_MATIO_TYPE_NOT_SUPPORTED,
                _W("Cannot read variable:") + L" " + utf8_to_wstring(name));
            values.push_back(ArrayOf::emptyStructWithoutFields());
        } else {
            bool bSuccess = LoadMatioVariable(matVariable, false, value);
            Mat_VarFree(matVariable);
            matVariable = nullptr;
            if (bSuccess) {
                values.push_back(value);
            } else {
                Mat_Close(matfile);
                matfile = nullptr;
                std::string msg = _("Cannot read variable:") + std::string(" ") + name;
                Error(msg);
            }
        }
    }
    if (matfile != nullptr) {
        Mat_Close(matfile);
        matfile = nullptr;
    }

    if (asStruct) {
        res = ArrayOf::structScalarConstructor(variablesNamesToRead, values);
    } else {
        for (indexType i = 0; i < variablesNamesToRead.size(); i++) {
            eval->getContext()->getCurrentScope()->insertVariable(
                variablesNamesToRead[i], values[i]);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
