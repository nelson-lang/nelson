//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <vector>
#include "meanBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "OverloadRequired.hpp"
#include "Mean.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
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
            Error(L"Nelson:statistics:invalidDimensionArg", L"Dimension argument must be %s.",
                L"a positive integer scalar or vector of positive integers");
        }
        dims.push_back((indexType)v);
    }
    return true;
}
//=============================================================================
// mean over several dimensions == successive single-dimension means, valid
// because each intermediate mean averages the same number of elements (this
// equal-weight property does not hold once NaNs are omitted).
static ArrayOf
meanOverDimensions(const ArrayOf& X, const std::vector<indexType>& dims, bool omitNaN,
    MEAN_OUT_TYPE outType, bool& needToOverload)
{
    if (omitNaN) {
        Error(L"Nelson:statistics:omitnanWithDimVector",
            L"The 'omitnan' option is not supported with a vector of dimensions.");
    }
    ArrayOf acc(X);
    for (indexType dm : dims) {
        acc = Mean(acc, dm, omitNaN, outType, needToOverload);
        if (needToOverload) {
            break;
        }
    }
    return acc;
}
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::meanBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isSparse()) {
        // a sparse input is averaged densely; the mean stays sparse
        ArrayOfVector denseArgs(argIn);
        denseArgs[0].makeDense();
        retval = meanBuiltin(nLhs, denseArgs);
        if (!retval.empty()) {
            retval[0].makeSparse();
        }
        return retval;
    }
    for (size_t k = 1; k < argIn.size(); ++k) {
        // 'omitmissing' and 'includemissing' are the 'omitnan' and 'includenan' flags
        if (argIn[k].isRowVectorCharacterArray() || argIn[k].isScalarStringArray()) {
            std::wstring flag = argIn[k].asWideString();
            if (flag == L"omitmissing" || flag == L"includemissing") {
                ArrayOfVector renamed(argIn);
                renamed[k]
                    = ArrayOf::characterArray(flag == L"omitmissing" ? L"omitnan" : L"includenan");
                return meanBuiltin(nLhs, renamed);
            }
        }
    }
    ArrayOf res;
    bool needToOverload = false;
    bool omitNaN = false;
    bool isAll = false;
    bool useVecDim = false;
    std::vector<indexType> vecDims;
    indexType dim = 0;
    MEAN_OUT_TYPE outType = MEAN_OUT_TYPE::DEFAULT;
    switch (argIn.size()) {
    case 1: {
        res = Mean(argIn[0], dim, omitNaN, outType, needToOverload);
    } break;
    case 2: {
        // M = mean(A, 'all');
        // M = mean(A, dim);
        // M = mean(A, 'default'), M = mean(A, 'double'), M = mean(A, 'native')
        // M = mean(A, 'includenan'), M = mean(A, 'omitnan')
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param2.isRowVectorCharacterArray() || (param2.isStringArray() && param2.isScalar())) {
            std::wstring s = param2.asWideString();
            if (s == L"omitnan") {
                omitNaN = true;
            } else if (s == L"includenan") {
                omitNaN = false;
            } else if (s == L"all") {
                isAll = true;
            } else if (s == L"default") {
                outType = MEAN_OUT_TYPE::DEFAULT;
            } else if (s == L"double") {
                outType = MEAN_OUT_TYPE::DOUBLE;
            } else if (s == L"native") {
                outType = MEAN_OUT_TYPE::NATIVE;
            } else {
                Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                    "second");
            }
        } else if (tryGetDimensionVector(param2, vecDims)) {
            useVecDim = true;
        } else {
            dim = param2.asScalarIndex(false);
        }

        if (isAll) {
            res = MeanAll(param1, omitNaN, outType, needToOverload);
        } else if (useVecDim) {
            res = meanOverDimensions(param1, vecDims, omitNaN, outType, needToOverload);
        } else {
            res = Mean(param1, dim, omitNaN, outType, needToOverload);
        }

    } break;
    case 3: {
        // M = mean(A, 'all', nanflag)
        // M = mean(A, 'all', outtype)
        // M = mean(A, dim, nanflag)
        // M = mean(A, dim, outtype)
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        if (param2.isRowVectorCharacterArray() || (param2.isStringArray() && param2.isScalar())) {
            std::wstring s = param2.asWideString();
            if (s == L"all") {
                isAll = true;
            } else {
                Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                    "second");
            }
        } else if (tryGetDimensionVector(param2, vecDims)) {
            useVecDim = true;
        } else {
            dim = param2.asScalarIndex(false);
        }
        if (param3.isRowVectorCharacterArray() || (param3.isStringArray() && param3.isScalar())) {
            std::wstring s = param3.asWideString();
            if (s == L"omitnan") {
                omitNaN = true;
            } else if (s == L"includenan") {
                omitNaN = false;
            } else if (s == L"default") {
                outType = MEAN_OUT_TYPE::DEFAULT;
            } else if (s == L"double") {
                outType = MEAN_OUT_TYPE::DOUBLE;
            } else if (s == L"native") {
                outType = MEAN_OUT_TYPE::NATIVE;
            } else {
                Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                    "third");
            }
        } else {
            Error(
                "Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.", "third");
        }
        if (isAll) {
            res = MeanAll(param1, omitNaN, outType, needToOverload);
        } else if (useVecDim) {
            res = meanOverDimensions(param1, vecDims, omitNaN, outType, needToOverload);
        } else {
            res = Mean(param1, dim, omitNaN, outType, needToOverload);
        }
    } break;
    case 4: {
        // M = mean(A, 'all', nanflag, outtype)
        // M = mean(A, 'all', outtype, nanflag)
        // M = mean(A, dim, nanflag, outtype)
        // M = mean(A, dim, outtype, nanflag)
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        ArrayOf param4 = argIn[3];
        if (param2.isRowVectorCharacterArray() || (param2.isStringArray() && param2.isScalar())) {
            std::wstring s = param2.asWideString();
            if (s == L"all") {
                isAll = true;
            } else {
                Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                    "second");
            }
        } else {
            dim = param2.asScalarIndex(false);
        }
        bool haveNaNChoice = false;
        bool haveTypeChoice = false;
        if (param3.isRowVectorCharacterArray() || (param3.isStringArray() && param3.isScalar())) {
            std::wstring s = param3.asWideString();
            if (s == L"omitnan") {
                omitNaN = true;
                haveNaNChoice = true;
            } else if (s == L"includenan") {
                omitNaN = false;
                haveNaNChoice = true;
            } else if (s == L"default") {
                outType = MEAN_OUT_TYPE::DEFAULT;
                haveTypeChoice = true;
            } else if (s == L"double") {
                outType = MEAN_OUT_TYPE::DOUBLE;
                haveTypeChoice = true;
            } else if (s == L"native") {
                outType = MEAN_OUT_TYPE::NATIVE;
                haveTypeChoice = true;
            } else {
                Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                    "third");
            }
        } else {
            Error(
                "Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.", "third");
        }
        if (param4.isRowVectorCharacterArray() || (param4.isStringArray() && param4.isScalar())) {
            std::wstring s = param4.asWideString();
            if (s == L"omitnan") {
                if (haveNaNChoice) {
                    Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                        "4th");
                }
                omitNaN = true;
            } else if (s == L"includenan") {
                if (haveNaNChoice) {
                    Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                        "4th");
                }
                omitNaN = false;
            } else if (s == L"default") {
                if (haveTypeChoice) {
                    Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                        "4th");
                }
                outType = MEAN_OUT_TYPE::DEFAULT;
            } else if (s == L"double") {
                if (haveTypeChoice) {
                    Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                        "4th");
                }
                outType = MEAN_OUT_TYPE::DOUBLE;
            } else if (s == L"native") {
                if (haveTypeChoice) {
                    Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                        "4th");
                }
                outType = MEAN_OUT_TYPE::NATIVE;
            } else {
                Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                    "4th");
            }
        } else {
            Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.", "4th");
        }
        if (isAll) {
            res = MeanAll(param1, omitNaN, outType, needToOverload);
        } else {
            res = Mean(param1, dim, omitNaN, outType, needToOverload);
        }
    } break;
    default:
        break;
    }
    if (needToOverload) {
        OverloadRequired("mean");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
