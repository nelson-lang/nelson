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
#include "native2unicodeBuiltin.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CharactersEncodingGateway::native2unicodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 1 || argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (param1.isSparse()) {
        Error(_W("Sparse type not supported."));
    }
    ArrayOfVector retval(1);
    std::string data;
    bool isSupportedAsNumeric
        = param1.isNumeric() && (param1.isEmpty() || param1.isScalar() || param1.isRowVector());
    bool isSupportedAsCharacterArray = param1.isCharacterArray()
        && (param1.isEmpty() || param1.isScalar() || param1.isRowVector());
    bool isSupportedAsScalarString
        = param1.isStringArray() && (param1.isEmpty() || param1.isScalar());
    if (isSupportedAsNumeric || isSupportedAsCharacterArray || isSupportedAsScalarString) {
        if (isSupportedAsNumeric) {
            param1.promoteType(NLS_UINT8);
            indexType l = param1.getElementCount();
            uint8* s = (uint8*)param1.getDataPointer();
            const char* chars = reinterpret_cast<const char*>(s);
            data = std::string(chars);
            data[l] = 0;
        } else {
            data = param1.getContentAsCString();
        }
    } else {
        Error("Type or dimensions not supported.");
    }
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
    std::string output;
    if (!charsetToUtf8Converter(data, charset, output)) {
        Error(_W("Cannot convert to unicode."));
    }
    retval << ArrayOf::characterArrayConstructor(output);
    return retval;
}
//=============================================================================
