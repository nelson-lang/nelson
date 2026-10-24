//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include "sumProdOptionsHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson::DataAnalysisGateway {
//=============================================================================
static bool
tryGetDimensionVector(const ArrayOf& arg, std::vector<indexType>& dims);
//=============================================================================
SumProdOptions
parseSumProdOptions(const ArrayOfVector& argIn)
{
    SumProdOptions options;
    bool outTypeGiven = false;
    for (size_t k = 1; k < argIn.size(); ++k) {
        const ArrayOf& arg = argIn[k];
        if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
            std::wstring flag = arg.asWideString();
            if (flag == L"all" && k == 1) {
                options.dim = -1;
            } else if (flag == L"double" || flag == L"native" || flag == L"default") {
                if (outTypeGiven) {
                    Error(L"Nelson:data_analysis:invalidOptionCombination",
                        L"Invalid option combination. Can only set one of 'double', 'native', "
                        L"and 'default'.");
                }
                options.outType = flag;
                outTypeGiven = true;
            } else if (flag == L"includenan" || flag == L"includemissing") {
                options.withNaN = true;
            } else if (flag == L"omitnan" || flag == L"omitmissing") {
                options.withNaN = false;
            } else {
                Error(L"Nelson:data_analysis:wrongValueForArgument",
                    L"Wrong value for #%d argument.", (int)(k + 1));
            }
        } else if (k != 1) {
            Error(L"Nelson:data_analysis:wrongValueForArgument", L"Wrong value for #%d argument.",
                (int)(k + 1));
        } else if (!tryGetDimensionVector(arg, options.vecDims)) {
            options.dim = arg.asScalarIndex(false);
        }
    }
    return options;
}
//=============================================================================
// Collect a vector-of-dimensions argument (e.g. sum(A, [1 2])). Returns true
// when the argument is a non-scalar numeric vector of positive integers.
bool
tryGetDimensionVector(const ArrayOf& arg, std::vector<indexType>& dims)
{
    if (arg.isRowVectorCharacterArray() || arg.isStringArray() || arg.isScalar() || !arg.isNumeric()
        || arg.isComplex() || !arg.isVector()) {
        return false;
    }
    ArrayOf values(arg);
    values.promoteType(NLS_DOUBLE);
    const double* ptr = (const double*)values.dataPointer();
    indexType n = values.elementCount();
    dims.clear();
    for (indexType k = 0; k < n; ++k) {
        double v = ptr[k];
        if (v < 1 || v != std::floor(v)) {
            Error(L"Nelson:data_analysis:invalidDimension",
                L"Dimension argument must be a positive integer scalar or vector of positive "
                L"integers.");
        }
        dims.push_back((indexType)v);
    }
    std::vector<indexType> sortedDims(dims);
    std::sort(sortedDims.begin(), sortedDims.end());
    if (std::adjacent_find(sortedDims.begin(), sortedDims.end()) != sortedDims.end()) {
        Error(L"Nelson:data_analysis:dimensionVectorUnique",
            L"Elements of a dimension vector must be unique.");
    }
    return true;
}
//=============================================================================
} // namespace Nelson::DataAnalysisGateway
//=============================================================================
