//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nanBuiltin.hpp"
#include "Error.hpp"
#include "NaN.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::nanBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    sizeType nRhs = argIn.size();
    Dimensions dims(1, 1);
    if (argIn.empty()) {
        // nothing to do
    } else if (argIn.size() == 1) {
        if (argIn[0].isNumeric() && !argIn[0].isSparse()) {
            if (argIn[0].isRowVector()) {
                if (argIn[0].isEmpty()) {
                    Error(ERROR_WRONG_ARGUMENT_1_SIZE_ROW_VECTOR_EXPECTED);
                }
                if (argIn[0].getElementCount() < Nelson::maxDims) {
                    ArrayOf dimVector(argIn[0]);
                    auto* ptrValues = static_cast<indexType*>(dimVector.getContentAsIndexPointer());
                    ompIndexType elementCount = dimVector.getElementCount();
                    for (ompIndexType k = 0; k < elementCount; k++) {
                        dims[k] = static_cast<indexType>(ptrValues[k]);
                    }
                    if (dims.getLength() == 1) {
                        dims[1] = dims[0];
                    }
                } else {
                    Error(_W("Too many dimensions! Current limit is") + L" "
                        + std::to_wstring(Nelson::maxDims) + L".");
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_SIZE_ROW_VECTOR_EXPECTED);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_NUMERIC_EXPECTED);
        }
    } else {
        for (sizeType k = 0; k < nRhs; k++) {
            dims[k] = argIn[k].getContentAsScalarIndex(true, true, true);
        }
        if (dims.getLength() == 1) {
            dims[1] = dims[0];
        }
    }
    retval << NaN(dims);
    return retval;
}
//=============================================================================
