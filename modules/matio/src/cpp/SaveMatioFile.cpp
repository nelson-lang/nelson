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
#include <ctime>
#include <boost/algorithm/string.hpp>
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
versionToEnum(const std::wstring& matFileVersion, matio_compression& compressionLevel)
{
    if (matFileVersion == L"-v7.3") {
        compressionLevel = MAT_COMPRESSION_ZLIB;
        return MAT_FT_MAT73;
    }
    if (matFileVersion == L"-v7") {
        compressionLevel = MAT_COMPRESSION_ZLIB;
        return MAT_FT_MAT5;
    }
    if (matFileVersion == L"-v6") {
        compressionLevel = MAT_COMPRESSION_NONE;
        return MAT_FT_MAT5;
    }
    if (matFileVersion == L"-v4") {
        compressionLevel = MAT_COMPRESSION_NONE;
        return MAT_FT_MAT4;
    }
    return MAT_FT_UNDEFINED;
}
//=============================================================================
static std::string
createHeaderMatioFile()
{
    std::string header = std::string("Nelson 1.0 MAT-file");
    header = header + std::string(" Created by libmatio ") + std::string(MATIO_VERSION_STR);
#ifdef _MSC_VER
    time_t _tm = time(nullptr);
    struct tm* curtime = localtime(&_tm);
    std::string timestr = asctime(curtime);
#else
    struct tm newtime;
    time_t ltime;
    char buf[128];
    ltime = time(&ltime);
    localtime_r(&ltime, &newtime);
    std::string timestr = asctime_r(&newtime, buf);
#endif
    boost::algorithm::replace_last(timestr, "\n", "");
    header = header + std::string(" on ") + timestr;
    return header;
}
//=============================================================================
void
SaveMatioFile(Evaluator* eval, const std::wstring& filename, const wstringVector& names,
    const std::wstring& matFileVersion, bool append, bool nocompression)
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

    matio_compression matCompression;
    mat_ft matVersion = versionToEnum(matFileVersion, matCompression);
    if (matVersion == MAT_FT_UNDEFINED) {
        Error(_("Unknown save format."));
    }
    matCompression = nocompression ? MAT_COMPRESSION_NONE : matCompression;
    std::string headerFile = createHeaderMatioFile();
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
            matFile
                = Mat_CreateVer(wstring_to_utf8(filename).c_str(), headerFile.c_str(), matVersion);
        }
    } else {
        matFile = Mat_CreateVer(wstring_to_utf8(filename).c_str(), headerFile.c_str(), matVersion);
    }

    if (matFile == nullptr) {
        Error(_W("Cannot save file."));
    }

    for (size_t k = 0; k < variablesName.size(); k++) {
        ArrayOf variableValue;
        std::string variableName = wstring_to_utf8(variablesName[k]);
        eval->getContext()->getCurrentScope()->lookupVariable(variableName, variableValue);
        matvar_t* matioVariable = SaveMatioVariable(variableName, variableValue, matVersion);
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
} // namespace Nelson
//=============================================================================
