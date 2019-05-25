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
#include <cstring>
#include "LoadMatioCell.hpp"
#include "LoadMatioVariable.hpp"
#include "Exception.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioCell(matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        VariableValue.promoteType(NLS_CELL_ARRAY);
        bSuccess = true;
    } else {
        indexType nbElements = dims.getElementCount();
        ArrayOf* elements = nullptr;
        try {
            elements = new ArrayOf[nbElements];
            ArrayOf cell = ArrayOf(NLS_CELL_ARRAY, dims, elements);
            for (indexType k = 0; k < nbElements; k++) {
                matvar_t* elementMatVar = Mat_VarGetCell(matVariable, (int)k);
                if (!LoadMatioVariable(elementMatVar, true, elements[k])) {
                    return false;
                }
            }
            VariableValue = cell;
            bSuccess = true;
        } catch (Exception&) {
            return false;
        }
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
