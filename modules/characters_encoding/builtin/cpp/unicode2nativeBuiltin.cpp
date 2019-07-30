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
#include "unicode2nativeBuiltin.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CharactersEncodingGateway::unicode2nativeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 1 || argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
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
        Error(_("Invalid charset: ") + charset);
    }
    std::string output;
    if (!utf8ToCharsetConverter(data, output, charset)) {
        Error(_W("Cannot convert string to expected charset."));
    }
    Dimensions dims(1, output.length());
    const uint8_t* src = reinterpret_cast<const uint8_t*>(output.data());
    uint8* values = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, output.length());
    memcpy(values, src, sizeof(uint8) * output.length());
    ArrayOf res = ArrayOf(NLS_UINT8, dims, values);
    retval.push_back(res);
    return retval;
}
//=============================================================================
