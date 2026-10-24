//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiometadataBuiltin.hpp"
#include "AudioFileMetaData.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// info = audiometadata(filename)
// info_previous = audiometadata(filename, info)
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiometadataBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::wstring errorMessage;
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.asWideString();
    wstringVector names;
    wstringVector values;
    AudioFileMetaData(filename, names, values, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:audiometadata:readFailed", errorMessage);
    }
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (!param2.isStruct()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"struct");
        }
        if (!param2.isScalar()) {
            Error(L"Nelson:scalarExpected", L"Wrong size for argument #%d: scalar expected.", (2));
        }
        stringVector currentFieldnames = param2.fieldNames();
        wstringVector wcurrentFieldname;
        wstringVector currentValues;
        for (const std::string& fieldname : currentFieldnames) {
            ArrayOf value = param2.field(fieldname);
            if ((value.isEmpty(true) && value.isDoubleType())
                || value.isRowVectorCharacterArray()) {
                if (value.isRowVectorCharacterArray()) {
                    wcurrentFieldname.push_back(utf8_to_wstring(fieldname));
                    currentValues.push_back(value.asWideString());
                } else {
                    deleteAudioFileMetaData(filename, utf8_to_wstring(fieldname), errorMessage);
                    if (!errorMessage.empty()) {
                        Error(L"Nelson:audiometadata:readFailed", errorMessage);
                    }
                }
            } else {
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
            }
        }
        currentFieldnames.clear();
        setAudioFileMetaData(filename, wcurrentFieldname, currentValues, errorMessage);
    }
    if (!errorMessage.empty()) {
        Error(L"Nelson:audiometadata:readFailed", errorMessage);
    }
    ArrayOfVector fieldvalues;
    for (size_t i = 0; i < names.size(); i++) {
        fieldvalues << ArrayOf::characterArray(values[i]);
    }
    retval << ArrayOf::structArray(names, fieldvalues);
    return retval;
}
//=============================================================================
