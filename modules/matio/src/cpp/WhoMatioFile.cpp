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
#include <vector>
#include "FileSystemWrapper.hpp"
#include "WhoMatioFile.hpp"
#include "matioHelpers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
WhoMatioFile(Interface* io, const std::wstring& filename, const wstringVector& names, bool asCell)
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
        Error(_W("Valid .mat file expected."));
    }
    stringVector variableNamesInFile;
    size_t nVars = 0;
    char* const* variableNames = Mat_GetDir(matfile, &nVars);
    for (size_t k = 0; k < nVars; k++) {
        variableNamesInFile.push_back(variableNames[k]);
    }
    Mat_Close(matfile);
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
