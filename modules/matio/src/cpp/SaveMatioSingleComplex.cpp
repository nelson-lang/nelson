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
#include "SaveMatioSingleComplex.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioSingleComplex(const std::string& variableName, const ArrayOf& variableValue)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    void* ptrValue = nullptr;
    struct mat_complex_split_t z;
    single* re = nullptr;
    single* im = nullptr;
    if (!variableDims.isEmpty(false)) {
        indexType nbElements = variableDims.getElementCount();
        try {
            re = new single[nbElements];
        } catch (const std::bad_alloc&) {
            return nullptr;
        }
        try {
            im = new single[nbElements];
        } catch (const std::bad_alloc&) {
            delete[] re;
            return nullptr;
        }
        auto* ptrZ = reinterpret_cast<singlecomplex*>((single*)variableValue.getDataPointer());
        for (indexType k = 0; k < nbElements; ++k) {
            re[k] = ptrZ[k].real();
            im[k] = ptrZ[k].imag();
        }
        z.Re = re;
        z.Im = im;
        ptrValue = &z;
    }
    matvar_t* matVariable = Mat_VarCreate(
        variableName.c_str(), MAT_C_SINGLE, MAT_T_SINGLE, (int)rank, dims, ptrValue, MAT_F_COMPLEX);
    delete[] dims;
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
