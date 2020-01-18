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
#include <cstring>
#include "LoadMatioStruct.hpp"
#include "LoadMatioVariable.hpp"
#include "Exception.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioStruct(matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    unsigned int nbFields = Mat_VarGetNumberOfFields(matVariable);
    char* const* names = Mat_VarGetStructFieldnames(matVariable);
    stringVector fieldnames;
    fieldnames.reserve((size_t)nbFields);
    for (unsigned int k = 0; k < nbFields; k++) {
        fieldnames.push_back(names[k]);
    }

    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyStructConstructor(fieldnames, dims);
        bSuccess = true;
    } else if (dims.equals(Dimensions(1, 1)) && fieldnames.empty()) {
        VariableValue = ArrayOf::emptyStructWithoutFields();
        bSuccess = true;
    } else {
        indexType nbElements = dims.getElementCount();
        ArrayOf* elements = nullptr;
        try {
            elements = new ArrayOf[nbElements * fieldnames.size()];
        } catch (const std::bad_alloc&) {
            return false;
        }
        VariableValue = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
        indexType offset = 0;
        for (indexType j = 0; j < nbElements; j++) {
            for (indexType i = 0; i < (indexType)fieldnames.size(); i++) {
                std::string name = fieldnames[i];
                matvar_t* fieldMatVar = Mat_VarGetStructFieldByName(matVariable, name.c_str(), j);
                bool bSuccess = LoadMatioVariable(fieldMatVar, true, elements[offset]);
                if (!bSuccess) {
                    return false;
                }
                offset++;
            }
        }
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
