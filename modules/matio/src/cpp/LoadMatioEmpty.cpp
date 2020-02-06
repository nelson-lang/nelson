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
#include "LoadMatioEmpty.hpp"
#include "matioHelpers.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioEmpty(matvar_t* matVariable, bool fromCellOrStruct, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        bSuccess = true;
    }
    if (fromCellOrStruct && !bSuccess) {
        VariableValue = ArrayOf::emptyConstructor();
        bSuccess = true;
    } else {
        Warning(
            WARNING_MATIO_TYPE_NOT_SUPPORTED, _W("Cannot read matio variable of type: function."));
    }
    VariableValue = ArrayOf::emptyStructWithoutFields();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
