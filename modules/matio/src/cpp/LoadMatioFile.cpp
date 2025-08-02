//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <matio.h>
#include "LoadMatioFile.hpp"
#include "LoadMatioVariable.hpp"
#include "characters_encoding.hpp"
#include "Warning.hpp"
#include "FileSystemWrapper.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
LoadMatioFile(
    Evaluator* eval, const std::wstring& filename, const wstringVector& names, bool asStruct)
{
    ArrayOf res;
    FileSystemWrapper::Path mat_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(mat_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
    }
    if (!fileExistPreviously) {
        Error(_W("File does not exist."));
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
    char* const* variableNames = Mat_GetDir(matfile, &nVars);
    for (size_t k = 0; k < nVars; k++) {
        variableNamesInFile.push_back(variableNames[k]);
    }
    stringVector variablesNamesToRead;
    if (names.empty()) {
        variablesNamesToRead = variableNamesInFile;
    } else {
        for (const std::wstring& uname : names) {
            std::string name = wstring_to_utf8(uname);
            if (std::find(variableNamesInFile.begin(), variableNamesInFile.end(), name)
                == variableNamesInFile.end()) {
                std::string msg = _("Variable not found:") + std::string(" ") + name;
                Warning(msg);
            }
        }
    }
    ArrayOfVector values;
    for (const std::string& name : variablesNamesToRead) {
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
