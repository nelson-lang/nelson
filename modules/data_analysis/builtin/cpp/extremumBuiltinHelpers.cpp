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
#include <unordered_set>
#include <vector>
#include "extremumBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "FindCommonType.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson::DataAnalysisGateway {
//=============================================================================
struct ExtremumOptions
{
    bool omitNaN = true;
    bool linear = false;
    bool all = false;
    bool hasDimension = false;
    indexType dim = 0;
    std::vector<indexType> vecDims;
};
//=============================================================================
static void
invalidArgument(size_t position);
static bool
isTextArgument(const ArrayOf& arg);
static bool
parseNanFlag(const std::wstring& flag, bool& omitNaN);
static void
parseReductionOption(const ArrayOf& arg, size_t position, ExtremumOptions& options);
static ArrayOf
linearIndicesAlongDimension(const Dimensions& dimsA, indexType dim, const ArrayOf& I);
static ArrayOfVector
reduceAlongDimensions(const ArrayOf& A, const std::vector<indexType>& dims, bool omitNaN,
    bool wantIndex, const ExtremumFunctions& functions, bool& needToOverload);
static ArrayOfVector
reduceBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, const ExtremumFunctions& functions);
static ArrayOfVector
binaryBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, const ExtremumFunctions& functions);
//=============================================================================
ArrayOfVector
extremumBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, const ExtremumFunctions& functions)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1, 5);
    if (argIn.size() == 1) {
        return reduceBuiltin(eval, nLhs, argIn, functionName, functions);
    }
    if (argIn.size() == 2) {
        return binaryBuiltin(eval, nLhs, argIn, functionName, functions);
    }
    // [] (0x0) as second argument is the reduction placeholder, even for A = [].
    if (argIn[1].dimensions().equals(Dimensions(0, 0))) {
        return reduceBuiltin(eval, nLhs, argIn, functionName, functions);
    }
    return binaryBuiltin(eval, nLhs, argIn, functionName, functions);
}
//=============================================================================
ArrayOfVector
reduceBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, const ExtremumFunctions& functions)
{
    bool wasFound = false;
    ArrayOfVector res = callOverloadedFunction(eval,
        NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), nLhs, argIn,
        functionName, ClassName(argIn[0]), argIn[0].dataClass(), wasFound);
    if (wasFound) {
        return res;
    }
    ExtremumOptions options;
    for (size_t k = 2; k < argIn.size(); ++k) {
        parseReductionOption(argIn[k], k + 1, options);
    }
    const ArrayOf& A = argIn[0];
    bool wantIndex = (nLhs > 1);
    bool needToOverload = false;
    ArrayOfVector retval;
    if (!options.vecDims.empty()) {
        if (wantIndex && !options.linear) {
            Error(L"Nelson:data_analysis:linearRequired",
                L"Specify 'linear' option when returning two outputs and the operating "
                L"dimension is a vector.");
        }
        retval = reduceAlongDimensions(
            A, options.vecDims, options.omitNaN, wantIndex, functions, needToOverload);
    } else if (options.all) {
        if (A.isEmpty() || A.isScalar()) {
            // reduce every dimension in turn: an empty keeps its empty shape
            std::vector<indexType> dims;
            for (indexType d = 1; d <= A.dimensions().length(); ++d) {
                dims.push_back(d);
            }
            retval = reduceAlongDimensions(
                A, dims, options.omitNaN, wantIndex, functions, needToOverload);
        } else {
            // flatten to a column: the index of the reduction is the linear index into A
            ArrayOf flat(A);
            Dimensions columnDims(A.elementCount(), 1);
            flat.reshape(columnDims);
            retval = functions.unary(options.omitNaN, flat, nLhs, needToOverload);
        }
    } else if (options.dim == 0) {
        retval = functions.unary(options.omitNaN, A, nLhs, needToOverload);
        if (wantIndex && options.linear && !needToOverload) {
            Dimensions dimsA(A.dimensions());
            indexType d = 0;
            while (d < dimsA.length() && dimsA[d] == 1) {
                d++;
            }
            retval[1] = linearIndicesAlongDimension(dimsA, d + 1, retval[1]);
        }
    } else {
        retval = functions.alongDimension(options.omitNaN, A, options.dim, nLhs, needToOverload);
        if (wantIndex && options.linear && !needToOverload) {
            retval[1] = linearIndicesAlongDimension(A.dimensions(), options.dim, retval[1]);
        }
    }
    if (needToOverload) {
        bool overloadWasFound = false;
        retval = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, argIn, functionName,
            ClassName(argIn[0]), argIn[0].dataClass(), overloadWasFound);
        if (!overloadWasFound) {
            OverloadRequired(functionName);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
binaryBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, const ExtremumFunctions& functions)
{
    if (nLhs > 1) {
        Error(L"Nelson:data_analysis:twoOutputsTwoInputs",
            L"Two output arguments are not supported for two input arrays.");
    }
    bool omitNaN = true;
    for (size_t k = 2; k < argIn.size(); ++k) {
        if (!isTextArgument(argIn[k])) {
            Error("Nelson:data_analysis:specifyingOperatingDimensionSupportedTwo",
                "Specifying the operating dimension is not supported for two input arrays.");
        }
        if (!parseNanFlag(argIn[k].asWideString(), omitNaN)) {
            invalidArgument(k + 1);
        }
    }
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;
    ArrayOfVector args;
    args << argIn[0];
    args << argIn[1];
    if (FindCommonType(args, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        ArrayOfVector res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn,
            functionName, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    ArrayOfVector retval;
    if (commonType <= NLS_CHAR) {
        NelsonType valueType = commonType;
        if (commonType == NLS_DOUBLE && isComplex) {
            valueType = NLS_DCOMPLEX;
        }
        if (commonType == NLS_SINGLE && isComplex) {
            valueType = NLS_SCOMPLEX;
        }
        ArrayOf A(argIn[0]);
        ArrayOf B(argIn[1]);
        A.promoteType(valueType);
        B.promoteType(valueType);
        bool needToOverload = false;
        retval << functions.binary(omitNaN, A, B, needToOverload);
        if (!needToOverload) {
            return retval;
        }
    }
    bool overloadWasFound = false;
    retval = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, argIn, functionName,
        commonTypeName, commonType, overloadWasFound);
    if (!overloadWasFound) {
        OverloadRequired(functionName);
    }
    return retval;
}
//=============================================================================
// reduce the dimensions one after the other; with wantIndex the linear index
// into A of the retained element is carried along the successive reductions
ArrayOfVector
reduceAlongDimensions(const ArrayOf& A, const std::vector<indexType>& dims, bool omitNaN,
    bool wantIndex, const ExtremumFunctions& functions, bool& needToOverload)
{
    ArrayOf acc(A);
    ArrayOf linear;
    bool haveLinear = false;
    for (indexType d : dims) {
        ArrayOfVector step
            = functions.alongDimension(omitNaN, acc, d, wantIndex ? 2 : 1, needToOverload);
        if (needToOverload) {
            return {};
        }
        if (wantIndex) {
            ArrayOf stepLinear = linearIndicesAlongDimension(acc.dimensions(), d, step[1]);
            if (!haveLinear) {
                linear = stepLinear;
                haveLinear = true;
            } else {
                // compose with the indices retained by the previous reductions
                const double* previous = (const double*)linear.dataPointer();
                double* current = (double*)stepLinear.dataPointer();
                indexType n = stepLinear.elementCount();
                for (indexType k = 0; k < n; ++k) {
                    current[k] = previous[(indexType)current[k] - 1];
                }
                linear = stepLinear;
            }
        }
        acc = step[0];
    }
    Dimensions outDims(acc.dimensions());
    outDims.simplify();
    acc.reshape(outDims);
    ArrayOfVector retval;
    retval << acc;
    if (wantIndex) {
        linear.reshape(outDims);
        retval << linear;
    }
    return retval;
}
//=============================================================================
// I holds 1-based positions along dimension dim (1-based) of an array of size
// dimsA; the result holds the matching linear indices into that array.
ArrayOf
linearIndicesAlongDimension(const Dimensions& dimsA, indexType dim, const ArrayOf& I)
{
    indexType workDim = dim - 1;
    indexType planesize = 1;
    for (indexType d = 0; d < workDim && d < dimsA.length(); d++) {
        planesize *= dimsA.dimensionLength(d);
    }
    indexType linesize = (workDim < dimsA.length()) ? dimsA.dimensionLength(workDim) : 1;
    indexType n = I.elementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, n);
    ArrayOf result(NLS_DOUBLE, I.dimensions(), ptr);
    ArrayOf positions(I);
    positions.promoteType(NLS_DOUBLE);
    const double* src = (const double*)positions.dataPointer();
    for (indexType line = 0; line < n; ++line) {
        indexType i = line / planesize;
        indexType j = line % planesize;
        indexType k = (indexType)src[line] - 1;
        ptr[line] = (double)(j + (k + i * linesize) * planesize + 1);
    }
    return result;
}
//=============================================================================
void
parseReductionOption(const ArrayOf& arg, size_t position, ExtremumOptions& options)
{
    if (isTextArgument(arg)) {
        std::wstring flag = arg.asWideString();
        if (parseNanFlag(flag, options.omitNaN)) {
            return;
        }
        if (flag == L"linear") {
            options.linear = true;
            return;
        }
        if (flag == L"all" && !options.hasDimension) {
            options.all = true;
            options.hasDimension = true;
            return;
        }
        invalidArgument(position);
    }
    if (options.hasDimension || !(arg.isNumeric() || arg.dataClass() == NLS_LOGICAL)
        || arg.isComplex() || arg.isSparse() || arg.isEmpty() || !arg.isVector()) {
        invalidArgument(position);
    }
    ArrayOf values(arg);
    values.promoteType(NLS_DOUBLE);
    const double* ptr = (const double*)values.dataPointer();
    indexType n = values.elementCount();
    for (indexType k = 0; k < n; ++k) {
        double v = ptr[k];
        if (v < 1 || v != std::floor(v) || !std::isfinite(v)) {
            Error(L"Nelson:data_analysis:invalidDimension",
                L"Dimension argument must be a positive integer scalar or vector of positive "
                L"integers.");
        }
    }
    options.hasDimension = true;
    if (n == 1) {
        options.dim = (indexType)ptr[0];
        return;
    }
    std::unordered_set<indexType> seen;
    for (indexType k = 0; k < n; ++k) {
        indexType d = (indexType)ptr[k];
        if (!seen.insert(d).second) {
            Error(L"Nelson:data_analysis:dimensionVectorUnique",
                L"Elements of a dimension vector must be unique.");
        }
        options.vecDims.push_back(d);
    }
}
//=============================================================================
bool
parseNanFlag(const std::wstring& flag, bool& omitNaN)
{
    if (flag == L"omitnan" || flag == L"omitmissing") {
        omitNaN = true;
        return true;
    }
    if (flag == L"includenan" || flag == L"includemissing") {
        omitNaN = false;
        return true;
    }
    return false;
}
//=============================================================================
bool
isTextArgument(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
}
//=============================================================================
void
invalidArgument(size_t position)
{
    switch (position) {
    case 3:
        Error("Nelson:data_analysis:invalidThirdArgument", "Invalid third argument.");
        break;
    case 4:
        Error("Nelson:data_analysis:invalid4thArgument", "Invalid 4th argument.");
        break;
    default:
        Error("Nelson:data_analysis:invalid5thArgument", "Invalid 5th argument.");
        break;
    }
}
//=============================================================================
} // namespace Nelson::DataAnalysisGateway
//=============================================================================
