//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiowriteBuiltin.hpp"
#include "AudioWrite.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// audiowrite(filename, y, Fs, ...)
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiowriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 3);
    std::wstring errorMessage;
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.getContentAsWideString();
    ArrayOf data = argIn[1];
    ArrayOf param3 = argIn[2];
    int fs = param3.getContentAsInteger32Scalar();
    int BitsPerSample = 16;
    int BitRate = -1;
    std::wstring Title;
    std::wstring Artist;
    std::wstring Comment;
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
            swprintf(buffer, 4096, std::wstring(ERROR_WRONG_ARGUMENT_X_VALUE).c_str(),
                static_cast<int>(i));
            Error(std::wstring(buffer));
        }
    }
    wstringVector metadata;
    metadata.reserve(3);
    metadata.push_back(Title);
    metadata.push_back(Artist);
    metadata.push_back(Comment);
    bool res = AudioWrite(filename, data, fs, metadata, BitsPerSample, BitRate, errorMessage);
    if (!res) {
        Error(errorMessage);
    }
    return {};
}
//=============================================================================
