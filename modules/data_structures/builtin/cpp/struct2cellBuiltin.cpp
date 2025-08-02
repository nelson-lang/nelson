//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "struct2cellBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::struct2cellBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector ret;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (!param1.isStruct()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED);
    }
    stringVector fieldnames = param1.getFieldNames();
    size_t nbFields = fieldnames.size();
    ArrayOf* elements = nullptr;
    Dimensions dimsStruct = param1.getDimensions();
    dimsStruct.simplify();
    try {
        indexType nbElements = dimsStruct.getElementCount() * nbFields;
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    try {
        auto* v = new ArrayOfVector[nbFields];
        indexType S = 0;
        for (size_t k = 0; k < nbFields; k++) {
            v[k] = param1.getFieldAsList(fieldnames[k]);
            S = v[k].size();
        }
        size_t l = 0;
        for (indexType q = 0; q < S; q++) {
            for (size_t k = 0; k < nbFields; k++) {
                elements[l] = v[k][q];
                l++;
            }
        }
        delete[] v;
    } catch (const std::bad_alloc&) {
        delete[] elements;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    Dimensions dims;
    dims[0] = nbFields;
    for (indexType k = 0; k < dimsStruct.getLength(); k++) {
        dims[k + 1] = dimsStruct[k];
    }
    dims.simplify();
    ret.push_back(ArrayOf(NLS_CELL_ARRAY, dims, elements));
    return ret;
}
//=============================================================================
