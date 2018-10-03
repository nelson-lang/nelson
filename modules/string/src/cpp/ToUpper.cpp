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
#include "ToUpper.hpp"
#include "Error.hpp"
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <cctype>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToUpper(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isRowVectorCharacterArray()) {
        return ArrayOf::characterArrayConstructor(ToUpper(A.getContentAsWideString()));
    } else if (A.getDataClass() == NLS_CELL_ARRAY) {
        if (A.isEmpty()) {
            return ArrayOf(A);
        } else {
            res = ArrayOf(A);
            res.ensureSingleOwner();
            ArrayOf* element = (ArrayOf*)(res.getDataPointer());
            for (indexType k = 0; k < A.getDimensions().getElementCount(); k++) {
                if (!element[k].isRowVectorCharacterArray()) {
                    Error(ERROR_TYPE_CELL_OF_STRINGS_EXPECTED);
                }
                element[k] = ArrayOf::characterArrayConstructor(
                    ToUpper(element[k].getContentAsWideString()));
            }
            return res;
        }
    } else if (A.getDataClass() == NLS_STRING_ARRAY) {
        if (A.isEmpty()) {
            return ArrayOf(A);
        } else {
            res = ArrayOf(A);
            res.ensureSingleOwner();
            ArrayOf* element = (ArrayOf*)(res.getDataPointer());
            for (indexType k = 0; k < A.getDimensions().getElementCount(); k++) {
                if (!element[k].isRowVectorCharacterArray()) {
                    element[k] = ArrayOf::emptyConstructor();
                } else {
                    element[k] = ArrayOf::characterArrayConstructor(
                        ToUpper(element[k].getContentAsWideString()));
                }
            }
            return res;
        }
    } else {
        needToOverload = true;
    }
    return res;
}
//=============================================================================
std::wstring
ToUpper(const std::wstring& A)
{
    return boost::to_upper_copy(A);
}
//=============================================================================
}
//=============================================================================
