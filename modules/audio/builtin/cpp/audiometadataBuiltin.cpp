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
#include "audiometadataBuiltin.hpp"
#include "AudioFileMetaData.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
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
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.empty() || argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
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
        for (std::string fieldname : currentFieldnames) {
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
