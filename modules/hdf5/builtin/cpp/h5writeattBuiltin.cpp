//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "h5writeattBuiltin.hpp"
#include "Error.hpp"
#include "h5WriteAttribute.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// h5writeatt(filename, location, attname, attvalue)
// h5writeatt(filename, location, attname, attvalue, 'TextEncoding', encoding)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5writeattBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
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
