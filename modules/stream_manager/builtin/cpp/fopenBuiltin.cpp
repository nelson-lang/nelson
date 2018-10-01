//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "fopenBuiltin.hpp"
#include "Error.hpp"
#include "File.hpp"
#include "FileOpen.hpp"
#include "FilesManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// [filename, permission] = fopen(fileID)
// fIDs = fopen('all')
//=============================================================================
static ArrayOfVector
Fopen(Evaluator* eval, std::wstring filename, std::wstring mode)
{
    ArrayOfVector retval;
    FilesManager* fm = (FilesManager*)(eval->FileManager);
    int filepos = -1;
    FOPEN_ERROR_TYPE fopen_error = FileOpen(fm, filename, mode, filepos);
    std::wstring msg = L"";
    switch (fopen_error) {
    case FOPEN_NO_ERROR: {
        msg = L"";
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
    case FOPEN_CANNOT_OPEN:
    default: {
        msg = _W("Impossible to open file.");
    } break;
    }
    retval.push_back(ArrayOf::doubleConstructor((double)filepos));
    retval.push_back(ArrayOf::characterArrayConstructor(msg));
    return retval;
}
//=============================================================================
static ArrayOfVector
FopenAll(Evaluator* eval)
{
    ArrayOfVector retval;
    FilesManager* fm = (FilesManager*)(eval->FileManager);
    boost::container::vector<uint64> IDs = fm->getIDs();
    if (IDs.size()) {
        double* dIDs = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, IDs.size());
        for (size_t k = 0; k < IDs.size(); k++) {
            dIDs[k] = (double)IDs[k];
        }
        Dimensions dim(1, IDs.size());
        ArrayOf res = ArrayOf(NLS_DOUBLE, dim, dIDs);
        retval.push_back(res);
    } else {
        retval.push_back(ArrayOf::emptyConstructor());
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
    switch (argIn.size()) {
    case 1: {
        ArrayOf param1 = argIn[0];
        if (param1.isDoubleType()) {
            int32 iValue = (int32)param1.getContentAsDoubleScalar();
            FilesManager* fm = (FilesManager*)(eval->FileManager);
            File* _file = fm->getFile(iValue);
            if (nLhs > 2) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (_file) {
                if (nLhs >= 0) {
                    retval.push_back(ArrayOf::characterArrayConstructor(_file->getFileName()));
                }
                if (nLhs > 0) {
                    retval.push_back(ArrayOf::characterArrayConstructor(_file->getFileMode()));
                }
            } else {
                Error(_W("Invalid file identifier."));
            }
            return retval;
        } else if (param1.isRowVectorCharacterArray()) {
            filename = param1.getContentAsWideString();
            if (filename == L"all") {
                return FopenAll(eval);
            } else {
                return Fopen(eval, filename, mode);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_DOUBLE_EXPECTED);
        }
    } break;
    case 2: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param1.isRowVectorCharacterArray() && param2.isRowVectorCharacterArray()) {
            filename = param1.getContentAsWideString();
            mode = param2.getContentAsWideString();
            return Fopen(eval, filename, mode);
        } else {
            if (param1.isRowVectorCharacterArray()) {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
            }
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
