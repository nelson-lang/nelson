//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include <vector>
#include "FileSystemWrapper.hpp"
#include "whoNh5File.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5LoadVariable.hpp"
#include "characters_encoding.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
whoNh5File(Interface* io, const std::wstring& filename, const wstringVector& names, bool asCell)
{
    ArrayOf res;
    FileSystemWrapper::Path nh5_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(nh5_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
    }
    if (!fileExistPreviously) {
        Error(_W("File does not exist."));
    }

    hid_t fid
        = H5Fopen(wstring_to_utf8(nh5_filename.wstring()).c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
    if (fid == H5I_INVALID_HID) {
        Error(_W("Open file failed."));
    }
    if (!isNelsonH5File(fid)) {
        H5Fclose(fid);
        Error(_W("Invalid file format."));
    }
    stringVector variableNamesInFile = getVariableNames(fid);
    H5Fclose(fid);
    stringVector variablesNamesToRead;
    if (names.empty()) {
        variablesNamesToRead = variableNamesInFile;
    } else {
        for (const std::wstring& uname : names) {
            std::string name = wstring_to_utf8(uname);
            if (std::find(variableNamesInFile.begin(), variableNamesInFile.end(), name)
                != variableNamesInFile.end()) {
                variablesNamesToRead.push_back(name);
            }
        }
    }
    if (asCell) {
        Dimensions dims(variablesNamesToRead.size(), 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
        indexType k = 0;
        for (const std::string& name : variablesNamesToRead) {
            elements[k] = ArrayOf::characterArrayConstructor(name);
            k++;
        }
        res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    } else {
        if (variablesNamesToRead.size()) {
            size_t ncharmax = io->getTerminalWidth();
            size_t nbchar = 0;
            io->outputMessage(_W("Your variables are:") + L"\n\n");
            for (auto& k : variablesNamesToRead) {
                if (nbchar + k.size() < ncharmax) {
                    io->outputMessage(k);
                    io->outputMessage(" ");
                    nbchar = 1 + nbchar + k.size();
                } else {
                    nbchar = 0;
                    io->outputMessage("\n");
                    io->outputMessage(k);
                    io->outputMessage(" ");
                    nbchar = 1 + nbchar + k.size();
                }
            }
            io->outputMessage("\n");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
