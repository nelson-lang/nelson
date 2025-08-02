//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SaveMatioDoubleComplex.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioDoubleComplex(const std::string& variableName, const ArrayOf& variableValue)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    void* ptrValue = nullptr;
    struct mat_complex_split_t z;
    double* re = nullptr;
    double* im = nullptr;
    if (!variableDims.isEmpty(false)) {
        indexType nbElements = variableDims.getElementCount();
        try {
            re = new double[nbElements];
        } catch (const std::bad_alloc&) {
            return nullptr;
        }
        try {
            im = new double[nbElements];
        } catch (const std::bad_alloc&) {
            delete[] re;
            return nullptr;
        }
        auto* ptrZ = reinterpret_cast<doublecomplex*>((double*)variableValue.getDataPointer());
        for (indexType k = 0; k < nbElements; ++k) {
            re[k] = ptrZ[k].real();
            im[k] = ptrZ[k].imag();
        }
        z.Re = re;
        z.Im = im;
        ptrValue = &z;
    }
    matvar_t* matVariable = Mat_VarCreate(
        variableName.c_str(), MAT_C_DOUBLE, MAT_T_DOUBLE, (int)rank, dims, ptrValue, MAT_F_COMPLEX);
    delete[] dims;
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
