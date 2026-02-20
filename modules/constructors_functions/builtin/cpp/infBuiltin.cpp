//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "infBuiltin.hpp"
#include "Error.hpp"
#include "Inf.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::infBuiltin(int nLhs, const ArrayOfVector& argIn)
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
                    raiseError2(_E("nelson:validators:mustBeRowVectorAtPosition"), 1);
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
                    raiseError2(
                        L"nelson:runtime:tooManyDimensions", std::to_wstring(Nelson::maxDims));
                }
            } else {
                raiseError2(_E("nelson:validators:mustBeRowVectorAtPosition"), 1);
            }
        } else {
            raiseError2(_E("nelson:validators:mustBeNumericAtPosition"), 1);
        }
    } else {
        for (sizeType k = 0; k < nRhs; k++) {
            dims[k] = argIn[k].getContentAsScalarIndex(true, true, true);
        }
        if (dims.getLength() == 1) {
            dims[1] = dims[0];
        }
    }
    retval << Inf(dims);
    return retval;
}
//=============================================================================
