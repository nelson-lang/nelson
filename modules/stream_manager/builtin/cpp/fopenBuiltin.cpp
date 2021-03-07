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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "fopenBuiltin.hpp"
#include "Error.hpp"
#include "File.hpp"
#include "FileOpen.hpp"
#include "FilesManager.hpp"
#include "Endian.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// [filename, permission] = fopen(fileID)
// fIDs = fopen('all')
//=============================================================================
static ArrayOfVector
Fopen(Evaluator* eval, const std::wstring& filename, const std::wstring& mode,
    const std::wstring& machineFormat, const std::wstring& encoding)
{
    ArrayOfVector retval;
    auto* fm = static_cast<FilesManager*>(eval->FileManager);
    int filepos = -1;
    FOPEN_ERROR_TYPE fopen_error = FileOpen(fm, filename, mode, machineFormat, encoding, filepos);
    std::wstring msg;
    switch (fopen_error) {
    case FOPEN_NO_ERROR: {
        msg.clear();
    } break;
    case FOPEN_INVALID_NAME: {
        msg = _W("Invalid name.");
    } break;
    case FOPEN_INVALID_MODE: {
        msg = _W("Invalid file mode.");
    } break;
    case FOPEN_IMPOSSIBLE_TO_ADD_FILE: {
        msg = _W("Impossible to add file.");
    } break;
    case FOPEN_INVALID_MACHINE_FORMAT: {
        msg = _W("Invalid machine format.");
    } break;
    case FOPEN_INVALID_ENCODING: {
        msg = _W("Invalid encoding.");
    } break;
    case FOPEN_CANNOT_OPEN:
    default: {
        msg = _W("Impossible to open file.");
    } break;
    }
    retval << ArrayOf::doubleConstructor((double)filepos);
    retval << ArrayOf::characterArrayConstructor(msg);
    return retval;
}
//=============================================================================
static ArrayOfVector
FopenAll(Evaluator* eval)
{
    ArrayOfVector retval;
    auto* fm = static_cast<FilesManager*>(eval->FileManager);
    boost::container::vector<uint64> IDs = fm->getIDs();
    if (IDs.size()) {
        double* dIDs
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, IDs.size(), stringVector(), false);
        for (size_t k = 0; k < IDs.size(); k++) {
            dIDs[k] = (double)IDs[k];
        }
        Dimensions dim(1, IDs.size());
        ArrayOf res = ArrayOf(NLS_DOUBLE, dim, dIDs);
        retval << res;
    } else {
        retval << ArrayOf::emptyConstructor();
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fopenBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    std::wstring mode = L"rb";
    std::wstring filename;
    std::wstring machineFormat = isLittleEndianFormat() ? L"ieee-le" : L"ieee-be";
    std::wstring encoding = L"UTF-8";

    nargincheck(argIn, 0, 4);
    ArrayOf param1 = argIn[0];
    if (argIn.size() == 1) {
        if (param1.isDoubleType()) {
            int32 iValue = (int32)param1.getContentAsDoubleScalar();
            FilesManager* fm = (FilesManager*)(eval->FileManager);
            File* _file = fm->getFile(iValue);
            nargoutcheck(nLhs, 0, 4);
            if (_file) {
                if (nLhs >= 0) {
                    retval << ArrayOf::characterArrayConstructor(_file->getFileName());
                }
                if (nLhs > 1) {
                    retval << ArrayOf::characterArrayConstructor(_file->getFileMode());
                }
                if (nLhs > 2) {
                    retval << ArrayOf::characterArrayConstructor(_file->getMachineFormat());
                }
                if (nLhs > 3) {
                    retval << ArrayOf::characterArrayConstructor(_file->getEncoding());
                }
            } else {
                Error(_W("Invalid file identifier."));
            }
            return retval;
        }
    }
    filename = param1.getContentAsWideString();
    if (filename == L"all") {
        return FopenAll(eval);
    }
    if (argIn.size() > 1) {
        ArrayOf param2 = argIn[1];
        mode = param2.getContentAsWideString();
    }
    if (argIn.size() > 2) {
        ArrayOf param3 = argIn[2];
        machineFormat = param3.getContentAsWideString();
        if (machineFormat != L"n" && machineFormat != L"native") {
            Warning(_W("machine format option ignored."));
        }
    }
    if (argIn.size() > 3) {
        ArrayOf param4 = argIn[3];
        encoding = param4.getContentAsWideString();
    }
    return Fopen(eval, filename, mode, machineFormat, encoding);
}
//=============================================================================
