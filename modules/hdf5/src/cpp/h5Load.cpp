//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "h5Load.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "characters_encoding.hpp"
#include "h5LoadVariable.hpp"
#include "haveNh5Header.hpp"
#include "Warning.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5Load(Evaluator* eval, const std::wstring& filename, const wstringVector& names, bool asStruct)
{
    ArrayOf res;
    FileSystemWrapper::Path hdf5_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(hdf5_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
    }
    if (!fileExistPreviously) {
        Error(_W("File does not exist."));
    }

    int16 nh5Version;
    int16 nh5Endian;
    std::wstring header;
    bool haveHeader = haveNh5Header(hdf5_filename.wstring(), header, nh5Version, nh5Endian);
    if (haveHeader) {
        if (nh5Version != NELSON_HEADER_VERSION) {
            Error(_W("Invalid file format."));
        }
    }
    hid_t fid
        = H5Fopen(wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
    if (fid == H5I_INVALID_HID) {
        Error(_W("Open file failed."));
    }
    if (!haveHeader) {
        if (!isNelsonH5File(fid)) {
            H5Fclose(fid);
            Error(_W("Invalid file format."));
        }
    }
    stringVector variableNamesInFile = getVariableNames(fid);
    stringVector variableNames;
    if (names.empty()) {
        variableNames = variableNamesInFile;
    } else {
        for (const std::wstring& uname : names) {
            std::string name = wstring_to_utf8(uname);
            if (std::find(variableNamesInFile.begin(), variableNamesInFile.end(), name)
                != variableNamesInFile.end()) {
                variableNames.push_back(name);
            } else {
                std::string msg = _("Variable not found:") + std::string(" ") + name;
                Warning(msg);
            }
        }
    }
    ArrayOfVector values;
    for (const std::string& name : variableNames) {
        ArrayOf value;
        if (h5LoadVariable(fid, "/", name, value)) {
            values.push_back(value);
        } else {
            H5Fclose(fid);
            std::string msg = _("Cannot read variable:") + std::string(" ") + name;
            Error(msg);
        }
    }
    herr_t status = H5Fclose(fid);
    if (asStruct) {
        res = ArrayOf::structScalarConstructor(variableNames, values);
    } else {
        for (size_t i = 0; i < variableNames.size(); i++) {
            eval->getContext()->getCurrentScope()->insertVariable(variableNames[i], values[i]);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
