//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5writeattBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "h5WriteAttribute.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// h5writeatt(filename, location, attname, attvalue)
// h5writeatt(filename, location, attname, attvalue, 'TextEncoding', encoding)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5writeattBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    indexType nbArgIn = argIn.size();
    if (!(nbArgIn == 4 || nbArgIn == 6)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.getContentAsWideString();
    ArrayOf param2 = argIn[1];
    std::wstring location = param2.getContentAsWideString();
    ArrayOf param3 = argIn[2];
    std::wstring attributeName = param3.getContentAsWideString();

    ArrayOf attributeValue = argIn[3];
    std::wstring textEncoding = L"system";
    if (nbArgIn == 6) {
        ArrayOf param5 = argIn[4];
        std::wstring textEncodingComputed = param2.getContentAsWideString();
        if (textEncodingComputed != L"TextEncoding") {
            Error(_W("'TextEncoding' expected."));
        }
        ArrayOf param6 = argIn[5];
        textEncoding = param2.getContentAsWideString();
    }
    h5WriteAttribute(filename, location, attributeName, attributeValue, textEncoding);
    return retval;
}
//=============================================================================
