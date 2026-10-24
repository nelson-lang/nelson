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
#include "ErrorID.hpp"
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
    if (M.isFunctionHandle()) {
        Error(L"Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            L"Undefined function '%s' for input arguments of type '%s'.", L"reshape",
            L"function_handle");
    }
    if (argIn.size() == 2 && (!argIn[1].isScalar())) {
        ArrayOf v = argIn[1];
        v.promoteType(NLS_DOUBLE);
        auto* dp = (double*)v.dataPointer();
        for (indexType i = 0; i < v.elementCount(); i++) {
            if (!std::isfinite(dp[i])) {
                Error(
                    L"Nelson:elementary_functions:finiteValueExpected", L"Finite value expected.");
            }
            if (dp[i] != dp[i]) {
                Error(
                    L"Nelson:elementary_functions:finiteValueExpected", L"Finite value expected.");
            }
            auto ivalue = static_cast<int64>(dp[i]);
            if (static_cast<double>(ivalue) != dp[i]) {
                Error(
                    L"Nelson:elementary_functions:realIntegerExpected", L"real integer expected.");
            }
            if (ivalue < 0) {
                Error(L"Nelson:elementary_functions:realPositiveIntegerExpected",
                    L"real positive integer expected.");
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
                indexType idx = arg.asScalarIndex();
                dims[i - 1] = idx;
            }
        } else {
            indexType idxEmptyPosition;
            bool bHaveAnEmptyMatrix = false;
            for (size_t i = 1; i < argIn.size(); i++) {
                if (bHaveAnEmptyMatrix) {
                    if (argIn[i].isEmpty()) {
                        Error(L"Nelson:elementary_functions:oneUnknownDimensionAllowed",
                            L"only one unknown dimension allowed.");
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
                    indexType idx = p.asScalarIndex();
                    dims[i - 1] = idx;
                }
            }
            if (bHaveAnEmptyMatrix && M.elementCount() == 0 && dims.elementCount() == 0) {
                // nothing constrains the unknown dimension of an empty result
                dims[idxEmptyPosition] = 0;
            } else if (bHaveAnEmptyMatrix) {
                indexType nbElementsM = M.elementCount();
                double rest
                    = static_cast<double>(nbElementsM) / static_cast<double>(dims.elementCount());
                if (!std::isfinite(rest)) {
                    Error(L"Nelson:elementary_functions:finiteValueExpected",
                        L"Finite value expected.");
                }
                if (rest != rest) {
                    Error(L"Nelson:elementary_functions:finiteValueExpected",
                        L"Finite value expected.");
                }
                auto ivalue = static_cast<int64>(rest);
                if (static_cast<double>(ivalue) != rest) {
                    Error(L"Nelson:elementary_functions:realIntegerExpected",
                        L"real integer expected.");
                }
                if (ivalue < 0) {
                    Error(L"Nelson:elementary_functions:realPositiveIntegerExpected",
                        L"real positive integer expected.");
                }
                dims[idxEmptyPosition] = static_cast<indexType>(ivalue);
            }
        }
    }
    if (dims.elementCount() != M.elementCount()) {
        Error(L"Nelson:elementary_functions:reshapeOperationChangeNumberElements",
            L"Reshape operation cannot change the number of elements in array.");
    }
    // Trailing singleton dimensions are dropped: reshape(x, 1, 6, 1) is 2-D.
    dims.simplify();
    M.reshape(dims);
    retval << M;

    return retval;
}
//=============================================================================
