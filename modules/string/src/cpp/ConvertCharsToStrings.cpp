//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "ConvertCharsToStrings.hpp"
#include "Exception.hpp"
#include "IsCellOfStrings.hpp"
//=============================================================================
namespace Nelson {
ArrayOfVector
ConvertCharsToStrings(const ArrayOfVector& A)
{
    ArrayOfVector res;
    for (auto value : A) {
        Dimensions dims = value.getDimensions();
        if (value.isCharacterArray()) {
            if (value.isEmpty(false)) {
                res.push_back(ArrayOf::stringArrayConstructor(""));
            } else if (value.isColumnVector()) {
                res.push_back(ArrayOf::stringArrayConstructor(value.getContentAsWideString()));
            } else {
                indexType len = dims.getElementCount() / dims.getRows();
                auto* ptrChar = (charType*)value.getDataPointer();
                std::wstring str;
                str.reserve(dims.getElementCount());
                for (indexType i = 0; i < dims.getRows(); i++) {
                    for (indexType j = 0; j < len; j++) {
                        size_t idx = i + j * dims.getRows();
                        if (ptrChar[idx] != 0) {
                            str.push_back(ptrChar[idx]);
                        }
                    }
                }
                res.push_back(ArrayOf::stringArrayConstructor(str));
            }
        } else if (IsCellOfString(value)) {
            auto* elementsString = new_with_exception<ArrayOf>(dims.getElementCount(), false);
            ArrayOf valueAsString = ArrayOf(NLS_STRING_ARRAY, dims, elementsString);
            auto* elementsCell = (ArrayOf*)value.getDataPointer();
            for (indexType q = 0; q < dims.getElementCount(); q++) {
                elementsString[q] = elementsCell[q];
            }
            res.push_back(valueAsString);
        } else {
            res.push_back(value);
        }
    }

    /*
    for (auto value : A) {
        if (value.isStringArray()) {
            Dimensions dims = value.getDimensions();
            if (dims.isEmpty(false)) {
                ArrayOf valueAsCell = ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
                res.push_back(valueAsCell);
            } else if (dims.isScalar()) {
                auto* elementsStr = (ArrayOf*)value.getDataPointer();
                ArrayOf element = elementsStr[0];
                if (element.getDataClass() == NLS_CHAR) {
                    res.push_back(
                        ArrayOf::characterArrayConstructor(element.getContentAsWideString()));
                } else {
                    res.push_back(ArrayOf::characterArrayConstructor(""));
                }
            } else {
                auto* elementsCell = new_with_exception<ArrayOf>(dims.getElementCount(), false);
                ArrayOf valueAsCell = ArrayOf(NLS_CELL_ARRAY, dims, elementsCell);
                auto* elementsStr = (ArrayOf*)value.getDataPointer();
                for (indexType q = 0; q < dims.getElementCount(); q++) {
                    if (elementsStr[q].getDataClass() == NLS_CHAR) {
                        elementsCell[q] = elementsStr[q];
                    } else {
                        elementsCell[q] = ArrayOf::characterArrayConstructor("");
                    }
                }
                res.push_back(valueAsCell);
            }
        } else {
            res.push_back(value);
        }
    }
    */
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
