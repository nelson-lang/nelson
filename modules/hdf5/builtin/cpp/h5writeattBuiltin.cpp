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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "h5WriteAttribute.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
    nargincheck(argIn, 4, 6);
    indexType nbArgIn = argIn.size();
    if (nbArgIn == 5) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.asWideString();
    ArrayOf param2 = argIn[1];
    std::wstring location = param2.asWideString();
    ArrayOf param3 = argIn[2];
    std::wstring attributeName = param3.asWideString();

    ArrayOf attributeValue = argIn[3];
    std::wstring textEncoding = L"system";
    if (nbArgIn == 6) {
        ArrayOf param5 = argIn[4];
        std::wstring textEncodingComputed = param2.asWideString();
        if (textEncodingComputed != L"TextEncoding") {
            Error(L"Nelson:h5writeatt:textEncodingExpected", L"'%s' expected.", L"TextEncoding");
        }
        ArrayOf param6 = argIn[5];
        textEncoding = param2.asWideString();
    }
    h5WriteAttribute(filename, location, attributeName, attributeValue, textEncoding);
    return retval;
}
//=============================================================================
