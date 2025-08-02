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
    std::wstring filename = param1.getContentAsWideString();
    wstringVector names;
    wstringVector values;
    AudioFileMetaData(filename, names, values, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (!param2.isStruct()) {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRUCT_EXPECTED);
        }
        if (!param2.isScalar()) {
            Error(ERROR_WRONG_ARGUMENT_2_SIZE_SCALAR_EXPECTED);
        }
        stringVector currentFieldnames = param2.getFieldNames();
        wstringVector wcurrentFieldname;
        wstringVector currentValues;
        for (const std::string& fieldname : currentFieldnames) {
            ArrayOf value = param2.getField(fieldname);
            if ((value.isEmpty(true) && value.isDoubleType())
                || value.isRowVectorCharacterArray()) {
                if (value.isRowVectorCharacterArray()) {
                    wcurrentFieldname.push_back(utf8_to_wstring(fieldname));
                    currentValues.push_back(value.getContentAsWideString());
                } else {
                    deleteAudioFileMetaData(filename, utf8_to_wstring(fieldname), errorMessage);
                    if (!errorMessage.empty()) {
                        Error(errorMessage);
                    }
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_VALUE);
            }
        }
        currentFieldnames.clear();
        setAudioFileMetaData(filename, wcurrentFieldname, currentValues, errorMessage);
    }
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    ArrayOfVector fieldvalues;
    for (size_t i = 0; i < names.size(); i++) {
        fieldvalues << ArrayOf::characterArrayConstructor(values[i]);
    }
    retval << ArrayOf::structConstructor(names, fieldvalues);
    return retval;
}
//=============================================================================
