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
#include "audiowriteBuiltin.hpp"
#include "AudioWrite.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// audiowrite(filename, y, Fs, ...)
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiowriteBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    std::wstring errorMessage;
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.getContentAsWideString();
    ArrayOf data = argIn[1];
    ArrayOf param3 = argIn[2];
    int fs = param3.getContentAsInteger32Scalar();
    int BitsPerSample = 16;
    int BitRate = -1;
    std::wstring Title = L"";
    std::wstring Artist = L"";
    std::wstring Comment = L"";
    for (size_t i = 3; i < argIn.size(); i += 2) {
        if (i >= argIn.size() - 1) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        ArrayOf paramFieldName = argIn[i];
        ArrayOf paramFieldValue = argIn[i + 1];
        std::wstring fieldname = paramFieldName.getContentAsWideString();
        bool validFieldname = false;
        if (fieldname == L"BitsPerSample") {
            BitsPerSample = paramFieldValue.getContentAsInteger32Scalar();
            validFieldname = true;
        }
        if (fieldname == L"BitRate") {
            BitRate = paramFieldValue.getContentAsInteger32Scalar();
            validFieldname = true;
        }
        if (fieldname == L"Title") {
            Title = paramFieldValue.getContentAsWideString();
            validFieldname = true;
        }
        if (fieldname == L"Artist") {
            Artist = paramFieldValue.getContentAsWideString();
            validFieldname = true;
        }
        if (fieldname == L"Comment") {
            Comment = paramFieldValue.getContentAsWideString();
            validFieldname = true;
        }
        if (!validFieldname) {
            wchar_t buffer[4096];
            swprintf(buffer, 4096, std::wstring(ERROR_WRONG_ARGUMENT_X_VALUE).c_str(), i);
            Error(std::wstring(buffer));
        }
    }
    wstringVector metadata;
    metadata.push_back(Title);
    metadata.push_back(Artist);
    metadata.push_back(Comment);
    bool res = AudioWrite(filename, data, fs, metadata, BitsPerSample, BitRate, errorMessage);
    if (!res) {
        Error(errorMessage);
    }
    return retval;
}
//=============================================================================
