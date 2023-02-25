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
#include <fmt/printf.h>
#include <fmt/format.h>
#include "cellBuiltin.hpp"
#include "Error.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
            ArrayOf* elementsCell = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
                NLS_CELL_ARRAY, argIn[0].getElementCount(), stringVector(), false));

            auto* elementsStringArray = (ArrayOf*)argIn[0].getDataPointer();
            ompIndexType elementCount = argIn[0].getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < elementCount; k++) {
                if (elementsStringArray[k].isCharacterArray()) {
                    elementsCell[k] = elementsStringArray[k];
                }
            }
            ArrayOf res = ArrayOf(NLS_CELL_ARRAY, argIn[0].getDimensions(), elementsCell);
            retval << res;
        } else if (argIn[0].getDataClass() == NLS_DOUBLE) {
            if (argIn[0].isVector() || argIn[0].isScalar()) {
                if (argIn[0].isScalar()) {
                    ArrayOf arg = argIn[0];
                    double dindex = arg.getContentAsDoubleScalar();
                    if (!std::isfinite(dindex)) {
                        Error(ERROR_WRONG_ARGUMENT_1_FINITE_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    if (dindex < 0) {
                        dindex = 0;
                    }
                    auto index = static_cast<indexType>(dindex);
                    if (static_cast<double>(index) != dindex) {
                        Error(ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    Dimensions dims(index, index);
                    auto* elements = new ArrayOf[index * index];
                    for (indexType k = 0; k < index * index; k++) {
                        elements[k] = ArrayOf::emptyConstructor();
                    }
                    retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
                } else {
                    ArrayOf arg = argIn[0];
                    Dimensions dims(arg.getElementCount());
                    auto* dindex = (double*)arg.getDataPointer();
                    for (indexType k = 0; k < arg.getElementCount(); k++) {
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
                    auto* elements = new ArrayOf[elementCount];
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
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
                    ArrayOf arg = argIn[k];
                    double dindex = arg.getContentAsDoubleScalar();
                    if (!std::isfinite(dindex)) {
                        Error(fmt::sprintf(
                            ERROR_WRONG_ARGUMENT_X_FINITE_SCALAR_INTEGER_VALUE_EXPECTED, k + 1));
                    }
                    if (dindex < 0) {
                        dindex = 0;
                    }
                    auto index = static_cast<indexType>(dindex);
                    if (static_cast<double>(index) != dindex) {
                        Error(fmt::sprintf(
                            ERROR_WRONG_ARGUMENT_X_FINITE_SCALAR_INTEGER_VALUE_EXPECTED, k + 1));
                    }
                    dims.setDimensionLength(k, index);
                } else {
                    Error(fmt::sprintf(ERROR_WRONG_ARGUMENT_X_SIZE_SCALAR_EXPECTED, k + 1));
                }
            } else {
                Error(fmt::sprintf(ERROR_WRONG_ARGUMENT_X_TYPE_DOUBLE_EXPECTED, k + 1));
            }
        }
        dims.simplify();
        auto* elements = new ArrayOf[dims.getElementCount()];
        ompIndexType elementCount = dims.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            elements[k] = ArrayOf::emptyConstructor();
        }
        retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    return retval;
}
//=============================================================================
