//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include "FileSystemWrapper.hpp"
#include "h5Save.hpp"
#include "Evaluator.hpp"
#include "IsValidVariableName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ClassName.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
h5Save(Evaluator* eval, const std::wstring& filename, const wstringVector& names, bool append,
    bool nocompression)
{
    wstringVector variablesName;
    for (const auto& name : names) {

        if (!IsValidVariableName(name)) {
            Error(L"Nelson:h5save:invalidVariableName", L"Invalid variable name: %s", name);
        }
        if (!eval->getContext()->isVariable(name)) {
            Error(L"Nelson:h5save:variableDoesNotExist", L"Variable does not exist: %s", name);
        }
    }

    variablesName = names;
    if (variablesName.empty()) {
        eval->getContext()->getCurrentScope()->getVariablesList(false, variablesName);
    }

    hid_t fid = H5I_INVALID_HID;
    FileSystemWrapper::Path hdf5_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(hdf5_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            Error(L"Nelson:hdf5:permissionDenied", L"Permission denied.");
        }
    }
    if (!fileExistPreviously) {
        fid = createNh5FileWithHeader(hdf5_filename.wstring());
    } else {
        if (!H5Fis_hdf5(wstring_to_utf8(hdf5_filename.wstring()).c_str())) {
            Error(L"Nelson:hdf5:notHdf5File", L"HDF5 format file expected.");
        } else {
            if (append) {
                fid = H5Fopen(
                    wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
            } else {
                FileSystemWrapper::Path p(hdf5_filename);
                if (!FileSystemWrapper::Path::remove(p)) {
                    Error(L"Nelson:h5save:cannotReplaceFile", L"Cannot replace file");
                }
                fid = createNh5FileWithHeader(hdf5_filename.wstring());
                updateNelsonH5Header(fid);
            }
        }
    }

    if (fid == H5I_INVALID_HID) {
        Error(L"Nelson:hdf5:openFailed", L"Open file failed.");
    }
    if (fileExistPreviously) {
        if (!isNelsonH5File(fid)) {
            H5Fclose(fid);
            Error(L"Nelson:hdf5:invalidFileFormat", L"Invalid file format.");
        }
        if (append) {
            int32 schema = getNelsonH5Schema(fid);
            bool isUnmanaged = (schema != NELSON_SCHEMA);
            if (isUnmanaged) {
                Error(L"Nelson:h5save:invalidFileVersion", L"Invalid file version.");
                H5Fclose(fid);
            }
        }
    }
    if (!fileExistPreviously) {
        updateNelsonH5Header(fid);
    }

    std::string location = "/";
    for (auto& k : variablesName) {
        ArrayOf variableValue;
        std::string variableName = wstring_to_utf8(k);
        eval->getContext()->getCurrentScope()->lookupVariable(variableName, variableValue);
        bool bSuccess
            = h5SaveVariable(eval, fid, location, variableName, variableValue, !nocompression);
        if (!bSuccess) {
            H5Fclose(fid);
            Error("Nelson:h5save:cannotSaveVariable", "Cannot save variable: %s", variableName);
        }
    }
    H5Fclose(fid);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
