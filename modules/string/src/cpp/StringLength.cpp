//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "StringLength.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
StringLength(ArrayOf A)
{
    ArrayOf res;
    Dimensions outputDims;
    double* ptrLength = nullptr;
    if (A.isStringArray()) {
        outputDims = A.getDimensions();
        ArrayOf* elements = (ArrayOf*)A.getDataPointer();
        ptrLength = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outputDims.getElementCount());
        for (size_t k = 0; k < outputDims.getElementCount(); k++) {
            if (elements[k].isCharacterArray()) {
                std::wstring wstr = elements[k].getContentAsWideString();
                ptrLength[k] = (double)wstr.length();
            } else {
                ptrLength[k] = std::nan("NaN");
            }
        }
    } else {
        wstringVector wstr = A.getContentAsWideStringVector(false);
        if (A.isCharacterArray() && wstr.empty()) {
            wstr.push_back(A.getContentAsWideString());
        }
        if (wstr.size() == 1) {
            outputDims = Dimensions(1, 1);
        } else {
            outputDims = A.getDimensions();
        }
        ptrLength = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outputDims.getElementCount());
        for (size_t k = 0; k < wstr.size(); k++) {
            ptrLength[k] = (double)wstr[k].length();
        }
    }
    return ArrayOf(NLS_DOUBLE, outputDims, ptrLength);
}
//=============================================================================
}
//=============================================================================
