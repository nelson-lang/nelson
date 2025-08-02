//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "reshapeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::reshapeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf M = argIn[0];
    Dimensions dims;
    if (M.isClassType()) {
        Error(_W("Undefined function 'reshape' for input arguments of overloaded type."));
    }
    if (M.isFunctionHandle()) {
        Error(_W("Undefined function 'reshape' for input arguments of type 'function_handle'."));
    }
    if (argIn.size() == 2 && (!argIn[1].isScalar())) {
        ArrayOf v = argIn[1];
        v.promoteType(NLS_DOUBLE);
        auto* dp = (double*)v.getDataPointer();
        for (indexType i = 0; i < v.getElementCount(); i++) {
            if (!std::isfinite(dp[i])) {
                Error(_W("finite value expected."));
            }
            if (dp[i] != dp[i]) {
                Error(_W("finite value expected."));
            }
            auto ivalue = static_cast<int64>(dp[i]);
            if (static_cast<double>(ivalue) != dp[i]) {
                Error(_W("real integer expected."));
            }
            if (ivalue < 0) {
                Error(_W("real positive integer expected."));
            }
            dims[i] = static_cast<indexType>(ivalue);
        }
    } else {
        nargincheck(argIn, 3);
        bool bAllIsScalar = true;
        for (size_t j = 1; j < argIn.size(); j++) {
            bAllIsScalar &= argIn[j].isScalar();
        }
        if (bAllIsScalar) {
            for (size_t i = 1; i < argIn.size(); i++) {
                ArrayOf arg = argIn[i];
                indexType idx = arg.getContentAsScalarIndex();
                dims[i - 1] = idx;
            }
        } else {
            indexType idxEmptyPosition;
            bool bHaveAnEmptyMatrix = false;
            for (size_t i = 1; i < argIn.size(); i++) {
                if (bHaveAnEmptyMatrix) {
                    if (argIn[i].isEmpty()) {
                        Error(_W("only one unknown dimension allowed."));
                    }
                } else {
                    if (argIn[i].isEmpty()) {
                        bHaveAnEmptyMatrix = argIn[i].isEmpty();
                        idxEmptyPosition = static_cast<indexType>(i) - 1;
                    }
                }
            }
            for (size_t i = 1; i < argIn.size(); i++) {
                ArrayOf p = argIn[i];
                if (p.isEmpty()) {
                    dims[i - 1] = 1;
                } else {
                    indexType idx = p.getContentAsScalarIndex();
                    dims[i - 1] = idx;
                }
            }
            if (bHaveAnEmptyMatrix) {
                indexType nbElementsM = M.getElementCount();
                double rest = static_cast<double>(nbElementsM)
                    / static_cast<double>(dims.getElementCount());
                if (!std::isfinite(rest)) {
                    Error(_W("finite value expected."));
                }
                if (rest != rest) {
                    Error(_W("finite value expected."));
                }
                auto ivalue = static_cast<int64>(rest);
                if (static_cast<double>(ivalue) != rest) {
                    Error(_W("real integer expected."));
                }
                if (ivalue < 0) {
                    Error(_W("real positive integer expected."));
                }
                dims[idxEmptyPosition] = static_cast<indexType>(ivalue);
            }
        }
    }
    if (dims.getElementCount() != M.getElementCount()) {
        Error(_W("Reshape operation cannot change the number of elements in array."));
    }
    M.reshape(dims);
    retval << M;

    return retval;
}
//=============================================================================
