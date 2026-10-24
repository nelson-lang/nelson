//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "datevecBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "DateVector.hpp"
#include "DateNumber.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include <Eigen/Sparse>
#include <memory>
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
using SparseDoubleMatrix = Eigen::SparseMatrix<double, 0, signedIndexType>;
using SparseComplexMatrix = Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>;
//=============================================================================
static ArrayOfVector
datevecFromValues(int nLhs, const double* values, indexType len, const Dimensions& outputDims)
{
    ArrayOfVector retval(nLhs);
    if (nLhs < 2) {
        double* res = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, 6 * len, stringVector(), false));

        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
            double DT, Y, M, D, H, MN, S, MS;
            DT = values[k];
            DateVector(DT, Y, M, D, H, MN, S, MS, false);
            res[k] = Y;
            res[k + len] = M;
            res[k + len * 2] = D;
            res[k + len * 3] = H;
            res[k + len * 4] = MN;
            res[k + len * 5] = S + (MS / 1000);
        }
        retval << ArrayOf(NLS_DOUBLE, Dimensions(len, 6), res);
    } else {
        double* Y = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
        double* M = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
        double* D = nullptr;
        double* H = nullptr;
        double* MN = nullptr;
        double* S = nullptr;

        if (nLhs > 2) {
            D = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
        }
        if (nLhs > 3) {
            H = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
        }
        if (nLhs > 4) {
            MN = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
        }
        if (nLhs > 5) {
            S = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
        }

        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
            double DT = values[k];
            double V1, V2, V3, V4, V5, V6, V7;
            DateVector(DT, V1, V2, V3, V4, V5, V6, V7, false);
            Y[k] = V1;
            M[k] = V2;
            if (nLhs > 2) {
                D[k] = V3;
            }
            if (nLhs > 3) {
                H[k] = V4;
            }
            if (nLhs > 4) {
                MN[k] = V5;
            }
            if (nLhs > 5) {
                S[k] = V6 + (V7 / 1000);
            }
        }
        retval << ArrayOf(NLS_DOUBLE, outputDims, Y);
        retval << ArrayOf(NLS_DOUBLE, outputDims, M);
        if (nLhs > 2) {
            retval << ArrayOf(NLS_DOUBLE, outputDims, D);
        }
        if (nLhs > 3) {
            retval << ArrayOf(NLS_DOUBLE, outputDims, H);
        }
        if (nLhs > 4) {
            retval << ArrayOf(NLS_DOUBLE, outputDims, MN);
        }
        if (nLhs > 5) {
            retval << ArrayOf(NLS_DOUBLE, outputDims, S);
        }
    }
    return retval;
}
//=============================================================================
static std::vector<double>
getSparseDateValues(const ArrayOf& value)
{
    std::vector<double> values;
    if (value.dataClass() == NLS_DOUBLE) {
        const auto* mat = static_cast<const SparseDoubleMatrix*>(value.sparseDataPointer());
        values.reserve(mat->nonZeros());
        for (int outer = 0; outer < mat->outerSize(); ++outer) {
            for (SparseDoubleMatrix::InnerIterator it(*mat, outer); it; ++it) {
                values.push_back(it.value());
            }
        }
    } else {
        const auto* mat = static_cast<const SparseComplexMatrix*>(value.sparseDataPointer());
        values.reserve(mat->nonZeros());
        for (int outer = 0; outer < mat->outerSize(); ++outer) {
            for (SparseComplexMatrix::InnerIterator it(*mat, outer); it; ++it) {
                values.push_back(it.value().real());
            }
        }
    }
    return values;
}
//=============================================================================
static std::vector<double>
getDateValuesFromText(const ArrayOf& param1)
{
    std::vector<double> values;
    int pivotYear = 50;
    wstringVector dateAsString = param1.asWideStringVector(false);
    values.reserve(dateAsString.size());
    // Compile the auto-detect format list once instead of rebuilding it per string.
    std::shared_ptr<DateFormatCompiled> compiled = compileDateNumberFormat(L"", false, pivotYear);
    for (const std::wstring& strdate : dateAsString) {
        bool bParsed = false;
        double value = DateNumber(strdate, *compiled, bParsed);
        if (!bParsed) {
            Error(L"Nelson:time:dateConversionFailed", L"Failed to convert text to %s.",
                L"date number");
        }
        values.push_back(value);
    }
    return values;
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::datevecBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 6);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isRowVectorCharacterArray() || param1.isStringArray()
        || param1.isCellArrayOfCharacterVectors()) {
        std::vector<double> values = getDateValuesFromText(param1);
        indexType len = static_cast<indexType>(values.size());
        return datevecFromValues(nLhs, values.data(), len, Dimensions(len, 1));
    }
    if ((param1.dataClass() == NLS_DOUBLE) || (param1.dataClass() == NLS_DCOMPLEX)) {
        if (param1.isSparse()) {
            std::vector<double> values = getSparseDateValues(param1);
            indexType len = static_cast<indexType>(values.size());
            return datevecFromValues(nLhs, values.data(), len, Dimensions(1, len));
        } else {
            if (param1.dataClass() == NLS_DCOMPLEX) {
                param1.promoteType(NLS_DOUBLE);
            }
            indexType len = param1.elementCount();
            auto* ptd = (double*)param1.dataPointer();
            Dimensions dimParam1 = param1.dimensions();
            dimParam1.simplify();
            dimParam1.setDimensionLength(0, 1);
            Dimensions dim(param1.rows(), dimParam1.elementCount());
            return datevecFromValues(nLhs, ptd, len, dim);
        }
    }
    OverloadRequired("datevec");
    return {};
}
//=============================================================================
