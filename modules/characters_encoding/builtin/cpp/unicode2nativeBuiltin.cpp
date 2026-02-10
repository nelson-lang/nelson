//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "unicode2nativeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CharactersEncodingGateway::unicode2nativeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    ArrayOfVector retval(nLhs);
    ArrayOf param1 = argIn[0];
    std::string data = param1.getContentAsCString();
    std::string charset;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        charset = param2.getContentAsCString();
        if (charset.empty()) {
            charset = getSystemEncoding();
        }
    } else {
        charset = getSystemEncoding();
    }
    if (!isSupportedEncoding(charset)) {
        raiseError(L"Nelson:characters_encoding:ERROR_INVALID_CHARSET", ERROR_INVALID_CHARSET,
            utf8_to_wstring(charset));
    }
    std::string output;
    if (!utf8ToCharsetConverter(data, output, charset)) {
        raiseError(L"Nelson:characters_encoding:ERROR_CANNOT_CONVERT_STRING_TO_EXPECTED_CHARSET",
            ERROR_CANNOT_CONVERT_STRING_TO_EXPECTED_CHARSET);
    }
    Dimensions dims(1, output.length());
    const uint8_t* src = reinterpret_cast<const uint8_t*>(output.data());
    uint8* values = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, output.length());
    memcpy(values, src, sizeof(uint8) * output.length());
    retval << ArrayOf(NLS_UINT8, dims, values);
    return retval;
}
//=============================================================================
