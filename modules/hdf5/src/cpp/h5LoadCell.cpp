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
#include "h5LoadCell.hpp"
#include "h5LoadVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadCell(hid_t fid, const std::string& location, const std::string& variableName, bool isEmpty,
    const Dimensions& dims, ArrayOf& VariableValue)
{
    indexType nbElements = dims.getElementCount();
    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc&) {
        return false;
    }
    VariableValue = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    for (indexType k = 0; k < nbElements; k++) {
        std::string elementName = std::to_string(k);
        ArrayOf value;
        if (h5LoadVariable(fid, h5path, elementName, value)) {
            elements[k] = value;
        } else {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
