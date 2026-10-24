//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "cellBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NewWithException.hpp"
#include "ArrayOfView.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::cellBuiltin(int nLhs, const ArrayOfVector& argsIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argsIn.size() == 1 && argsIn[0].isCell()) {
        retval << argsIn[0];
        return retval;
    }
    // sizes may be given in any numeric or char class; an empty size is 0
    ArrayOfVector argIn(argsIn);
    for (auto& arg : argIn) {
        if (arg.isEmpty() && !arg.isStringArray()) {
            arg = ArrayOf::doubleScalar(0);
        } else if (arg.dataClass() != NLS_DOUBLE && (arg.isNumeric() || arg.isCharacterArray())) {
            arg.promoteType(NLS_DOUBLE);
        }
    }
    if (argIn.empty()) {
        auto index = static_cast<indexType>(0);
        Dimensions dims(index, index);
        auto* elements = new ArrayOf[index];
        retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    if (argIn.size() == 1) {
        if (argIn[0].dataClass() == NLS_STRING_ARRAY) {
            auto* elementsCell = new_with_exception<ArrayOf>(argIn[0].elementCount(), false);
            ArrayOfView<ArrayOf> elementsStringArray(argIn[0], ArrayOfViewAssumeValidated {});
            ompIndexType elementCount = argIn[0].elementCount();
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < elementCount; k++) {
                if (elementsStringArray[k].isCharacterArray()) {
                    elementsCell[k] = elementsStringArray[k];
                }
            }
            ArrayOf res = ArrayOf(NLS_CELL_ARRAY, argIn[0].dimensions(), elementsCell);
            retval << res;
        } else if (argIn[0].dataClass() == NLS_DOUBLE) {
            if (argIn[0].isVector() || argIn[0].isScalar()) {
                if (argIn[0].isScalar()) {
                    indexType index = argIn[0].asScalarIndex(true, true, true);
                    Dimensions dims(index, index);
                    auto* elements = new_with_exception<ArrayOf>(index * index, false);
                    retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
                } else {
                    Dimensions dims(argIn[0].elementCount());
                    ArrayOfView<double> dindex(argIn[0], ArrayOfViewAssumeValidated {});
                    for (indexType k = 0; k < argIn[0].elementCount(); k++) {
                        double _dIndex = dindex[k];
                        if (!std::isfinite(_dIndex)) {
                            Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Argument #1");
                        }
                        if (_dIndex < 0) {
                            _dIndex = 0;
                        }
                        auto index = static_cast<indexType>(_dIndex);
                        if (static_cast<double>(index) != _dIndex) {
                            Error(L"Nelson:expectedInteger", L"%s must be an integer.",
                                L"Argument #1");
                        }
                        dims.setDimensionLength(k, index);
                    }
                    dims.simplify();
                    ompIndexType elementCount = dims.elementCount();
                    auto* elements = new_with_exception<ArrayOf>(elementCount, false);
                    retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
                }
            } else {
                Error(L"Nelson:expectedVector", L"%s must be a vector.", L"Argument #1");
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"double");
        }
    } else {
        Dimensions dims(argIn.size());
        for (sizeType k = 0; k < static_cast<sizeType>(argIn.size()); k++) {
            if (argIn[k].dataClass() == NLS_DOUBLE) {
                if (argIn[k].isScalar()) {
                    indexType index = argIn[k].asScalarIndex(true, true, true);
                    dims.setDimensionLength(k, index);
                } else {
                    Error(L"Nelson:scalarExpected",
                        L"Wrong size for argument #%d: scalar expected.", (k + 1));
                }
            } else {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (k + 1),
                    L"double");
            }
        }
        dims.simplify();
        auto* elements = new_with_exception<ArrayOf>(dims.elementCount(), false);
        retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    return retval;
}
//=============================================================================
