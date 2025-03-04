//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include "cellBuiltin.hpp"
#include "Error.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "NewWithException.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::cellBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.empty()) {
        auto index = static_cast<indexType>(0);
        Dimensions dims(index, index);
        auto* elements = new ArrayOf[index];
        retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    if (argIn.size() == 1) {
        if (argIn[0].getDataClass() == NLS_STRING_ARRAY) {
            auto* elementsCell = new_with_exception<ArrayOf>(argIn[0].getElementCount(), false);
            auto* elementsStringArray = (ArrayOf*)argIn[0].getDataPointer();
            ompIndexType elementCount = argIn[0].getElementCount();
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < elementCount; k++) {
                if (elementsStringArray[k].isCharacterArray()) {
                    elementsCell[k] = elementsStringArray[k];
                } else {
                    elementsCell[k] = ArrayOf::emptyConstructor();
                }
            }
            ArrayOf res = ArrayOf(NLS_CELL_ARRAY, argIn[0].getDimensions(), elementsCell);
            retval << res;
        } else if (argIn[0].getDataClass() == NLS_DOUBLE) {
            if (argIn[0].isVector() || argIn[0].isScalar()) {
                if (argIn[0].isScalar()) {
                    indexType index = argIn[0].getContentAsScalarIndex(true, true, true);
                    Dimensions dims(index, index);
                    auto* elements = new_with_exception<ArrayOf>(index * index, false);
                    for (indexType k = 0; k < index * index; k++) {
                        elements[k] = ArrayOf::emptyConstructor();
                    }
                    retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
                } else {
                    Dimensions dims(argIn[0].getElementCount());
                    auto* dindex = reinterpret_cast<const double*>(argIn[0].getDataPointer());
                    for (indexType k = 0; k < argIn[0].getElementCount(); k++) {
                        double _dIndex = dindex[k];
                        if (!std::isfinite(_dIndex)) {
                            Error(ERROR_WRONG_ARGUMENT_1_FINITE_VECTOR_INTEGER_VALUE_EXPECTED);
                        }
                        if (_dIndex < 0) {
                            _dIndex = 0;
                        }
                        auto index = static_cast<indexType>(_dIndex);
                        if (static_cast<double>(index) != _dIndex) {
                            Error(ERROR_WRONG_ARGUMENT_1_FINITE_VECTOR_INTEGER_VALUE_EXPECTED);
                        }
                        dims.setDimensionLength(k, index);
                    }
                    dims.simplify();
                    ompIndexType elementCount = dims.getElementCount();
                    auto* elements = new_with_exception<ArrayOf>(elementCount, false);
                    OMP_PARALLEL_FOR_LOOP(elementCount)
                    for (ompIndexType k = 0; k < elementCount; k++) {
                        elements[k] = ArrayOf::emptyConstructor();
                    }
                    retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_OR_ROW_VECTOR_EXPECTED);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
        }
    } else {
        Dimensions dims(argIn.size());
        for (sizeType k = 0; k < static_cast<sizeType>(argIn.size()); k++) {
            if (argIn[k].getDataClass() == NLS_DOUBLE) {
                if (argIn[k].isScalar()) {
                    indexType index = argIn[k].getContentAsScalarIndex(true, true, true);
                    dims.setDimensionLength(k, index);
                } else {
                    Error(fmt::sprintf(ERROR_WRONG_ARGUMENT_X_SIZE_SCALAR_EXPECTED, k + 1));
                }
            } else {
                Error(fmt::sprintf(ERROR_WRONG_ARGUMENT_X_TYPE_DOUBLE_EXPECTED, k + 1));
            }
        }
        dims.simplify();
        auto* elements = new_with_exception<ArrayOf>(dims.getElementCount(), false);
        ompIndexType elementCount = dims.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            elements[k].setValue(ArrayOf::emptyConstructor());
        }
        retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    return retval;
}
//=============================================================================
