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
#include <matio.h>
#include "SaveMatioFile.hpp"
#include "SaveMatioVariable.hpp"
#include "IsValidVariableName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static mat_ft
versionToEnum(std::wstring matFileVersion)
{
    if (matFileVersion == L"-v7.3") {
        return MAT_FT_MAT73;
    }
    if (matFileVersion == L"-v7") {
        return MAT_FT_MAT5;
    }
    if (matFileVersion == L"-v6" || matFileVersion == L"-v4") {
        return MAT_FT_MAT4;
    }
    return MAT_FT_UNDEFINED;
}
//=============================================================================
void
SaveMatioFile(Evaluator* eval, const std::wstring& filename, wstringVector names,
    std::wstring matFileVersion, bool append, bool nocompression)
{
    wstringVector variablesName;
    for (size_t k = 0; k < names.size(); k++) {

        if (!IsValidVariableName(names[k])) {
            Error(_W("Invalid variable name:") + names[k]);
        }
        if (!eval->getContext()->isVariable(names[k])) {
            Error(_W("Variable does not exist:") + names[k]);
        }
    }

    variablesName = names;
    if (variablesName.empty()) {
        eval->getContext()->getCurrentScope()->getVariablesList(false, variablesName);
    }

    mat_ft matVersion = versionToEnum(matFileVersion);
    if (matVersion == MAT_FT_UNDEFINED) {
        Error(_("Unknown save format."));
    }
    matio_compression matCompression = nocompression ? MAT_COMPRESSION_NONE : MAT_COMPRESSION_ZLIB;

    mat_t* matFile = nullptr;
    if (append) {
        matFile = Mat_Open(wstring_to_utf8(filename).c_str(), MAT_ACC_RDWR);
        if (matFile != nullptr) {
            matVersion = Mat_GetVersion(matFile);
            if (matVersion != MAT_FT_MAT73) {
                Error(_("Cannot append variable (-v7.3 required)."));
                Mat_Close(matFile);
            }
        } else {
            matFile = Mat_CreateVer(wstring_to_utf8(filename).c_str(), NULL, matVersion);
        }
    } else {
        matFile = Mat_CreateVer(wstring_to_utf8(filename).c_str(), NULL, matVersion);
    }

    if (matFile == nullptr) {
        Error(_W("Cannot save file."));
    }

    for (size_t k = 0; k < variablesName.size(); k++) {
        ArrayOf variableValue;
        std::string variableName = wstring_to_utf8(variablesName[k]);
        eval->getContext()->getCurrentScope()->lookupVariable(variableName, variableValue);
        matvar_t* matioVariable = SaveMatioVariable(variableName, variableValue);
        if (matioVariable == nullptr) {
            Mat_Close(matFile);
            Error(_("Cannot save variable:") + variableName);
        }
        int resWrite = 0;
        if (append) {
            resWrite = Mat_VarWriteAppend(matFile, matioVariable, matCompression, 1);
        } else {
            resWrite = Mat_VarWrite(matFile, matioVariable, matCompression);
        }
        Mat_VarFree(matioVariable);
        if (resWrite) {
            Mat_Close(matFile);
            Error(_("Cannot save variable:") + variableName);
        }
    }
    Mat_Close(matFile);
}
//=============================================================================
}
//=============================================================================
