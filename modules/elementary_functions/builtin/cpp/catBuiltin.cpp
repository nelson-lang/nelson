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
#include <vector>
#include "catBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Permute.hpp"
#include "ConcatenateDimensions.hpp"
#include "FunctionHandleConcatenation.hpp"
#include "ClassName.hpp"
#include "OverloadName.hpp"
#include "FunctionsInMemory.hpp"
#include "FunctionDef.hpp"
#include "FindCommonConcatenateType.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefConcatenationHelpers.hpp"
#include "StringConcatenationConversion.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
catAlongDimension(Evaluator* eval, indexType dim, const ArrayOfVector& arrays);
static ArrayOf
catByBlockCopy(indexType dim, const ArrayOfVector& parts, NelsonType allocType);
static ArrayOf
catByPermute(Evaluator* eval, indexType dim, const ArrayOfVector& arrays);
static ArrayOf
catAllEmptyByReshape(indexType dim, const ArrayOfVector& arrays);
static ArrayOf
catFunctionHandles(Evaluator* eval, indexType dim, const ArrayOfVector& arrays,
    const ArrayOfVector& operands, const ArrayOf& anyHandle);
static FunctionDefPtr
findInheritedCatMethod(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
// cat(dim, A, B, ...): dim 1 / 2 reuse the vertcat / horzcat operators (type,
// empty and class rules); dim >= 3 moves dim to the front with permute,
// concatenates there, then permutes back.
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::catBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;

    // Delegate to an "@<class>/cat" method (e.g. gpuArray) when a data argument has
    // one. The leading dimension argument is a plain double, so the generic
    // dispatch cannot find it.
    for (size_t k = 1; k < argIn.size(); ++k) {
        const ArrayOf& candidate = argIn[k];
        if (candidate.isHandle() || candidate.isClassType()) {
            std::string overloadName = getOverloadFunctionName(ClassName(candidate), "cat");
            FunctionDefPtr overload = nullptr;
            if (lookupOverloadFunctionCached(eval->getContext(), overloadName, overload)
                && overload != nullptr) {
                return overload->evaluateFunction(eval, argIn, nLhs);
            }
        }
    }
    FunctionDefPtr inheritedCat = findInheritedCatMethod(eval, argIn);
    if (inheritedCat != nullptr) {
        return inheritedCat->evaluateFunction(eval, argIn, nLhs);
    }

    ArrayOf dimArg = argIn[0];
    if (!dimArg.isScalar() || dimArg.isEmpty()) {
        Error(L"Nelson:elementary_functions:dimensionArgumentPositiveIntegerScalar",
            L"%s must be a positive integer scalar.", L"Dimension argument");
    }
    double dimValue = dimArg.asDoubleScalar();
    if (!std::isfinite(dimValue) || dimValue < 1.0 || dimValue != std::floor(dimValue)) {
        Error(L"Nelson:elementary_functions:dimensionArgumentPositiveIntegerScalar",
            L"%s must be a positive integer scalar.", L"Dimension argument");
    }
    indexType dim = (indexType)dimValue;

    if (argIn.size() == 1) {
        retval << ArrayOf::empty();
        return retval;
    }

    ArrayOfVector data;
    data.reserve(argIn.size() - 1);
    for (size_t k = 1; k < argIn.size(); ++k) {
        data.push_back(argIn[k]);
    }

    if (data.size() == 1) {
        retval << data[0];
        return retval;
    }
    if (dim == 1) {
        retval << eval->vertcatOperator(data);
        return retval;
    }
    if (dim == 2) {
        retval << eval->horzcatOperator(data);
        return retval;
    }
    retval << catAlongDimension(eval, dim, data);
    return retval;
}
//=============================================================================
// static helper functions
//=============================================================================
// A cat method a classdef subclass inherits from a superclass (the exact
// "@<class>/cat" lookup above does not walk the inheritance chain).
FunctionDefPtr
findInheritedCatMethod(Evaluator* eval, const ArrayOfVector& argIn)
{
    for (size_t k = 1; k < argIn.size(); ++k) {
        const ArrayOf& candidate = argIn[k];
        if (!candidate.isHandle() && !candidate.isClassType()) {
            continue;
        }
        const std::string className
            = candidate.isHandle() ? candidate.handleClassName() : candidate.classType();
        std::string resolvedName;
        FunctionDefPtr method = nullptr;
        if (!className.empty() && classdefLoadClass(className)
            && classdefResolveMethodFunction(
                className, "cat", resolvedName, eval->getClassdefAccessContext())
            && eval->getContext()->lookupFunction(resolvedName, method)) {
            return method;
        }
    }
    return nullptr;
}
//=============================================================================
static ArrayOf
catAlongDimension(Evaluator* eval, indexType dim, const ArrayOfVector& inputs)
{
    // An object converts the other operands before any is dropped (a 0x0
    // string goes through the class constructor too).
    ArrayOfVector converted;
    const ArrayOfVector& arrays
        = ClassdefConcatenationHelpers::prepareCatArguments(eval, inputs, converted) ? converted
                                                                                     : inputs;
    // Only 0x0 operands are dropped: any other empty operand must have the
    // dimensions of the others (cat(3, zeros(1, 0), [1 2]) is an error).
    // When every operand is 0x0 they are all kept and stacked along dim
    // (cat(3, [], []) is 0x0x2).
    ArrayOfVector operands;
    operands.reserve(arrays.size());
    for (const auto& a : arrays) {
        if (!(a.is2D() && a.rows() == 0 && a.columns() == 0)) {
            operands.push_back(a);
        }
    }
    bool allEmpty = operands.empty();
    if (allEmpty) {
        operands = arrays;
    }
    // Next to a string array, a dropped 0x0 operand still has to convert.
    for (const auto& a : arrays) {
        if (a.isStringArray()) {
            checkStringConcatenationOperands(arrays);
            break;
        }
    }
    for (const auto& a : arrays) {
        if (a.isFunctionHandle()) {
            return catFunctionHandles(eval, dim, arrays, operands, a);
        }
    }
    if (operands.size() == 1) {
        return operands[0];
    }

    // The fast path is a direct block copy for the plain array classes; struct /
    // class / handle / sparse and any unresolved combination fall back to the
    // permute + vertcat path, which reuses all of the operator's type rules.
    NelsonType commonType = NLS_DOUBLE;
    bool isSparse = false;
    bool isComplex = false;
    std::string typeName;
    bool resolved = FindCommonConcatenateType(operands, commonType, isSparse, isComplex, typeName);
    bool blockCopyable = resolved && !isSparse && commonType != NLS_STRUCT_ARRAY
        && commonType != NLS_CLASS_ARRAY && commonType != NLS_CLASSDEF_ARRAY
        && commonType != NLS_HANDLE && commonType != NLS_GO_HANDLE
        && commonType != NLS_STRING_ARRAY;
    if (!blockCopyable) {
        if (allEmpty && !isSparse) {
            if (commonType == NLS_STRUCT_ARRAY) {
                // raises the field mismatch error of the operator
                eval->vertcatOperator(operands);
            }
            return catAllEmptyByReshape(dim, operands);
        }
        return catByPermute(eval, dim, operands);
    }

    checkComplexIntegerConcatenation(operands, commonType);
    NelsonType allocType = commonType;
    if (isComplex) {
        if (commonType == NLS_SINGLE) {
            allocType = NLS_SCOMPLEX;
        } else if (commonType == NLS_DOUBLE) {
            allocType = NLS_DCOMPLEX;
        }
    }
    ArrayOfVector promoted;
    promoted.reserve(operands.size());
    for (const auto& a : operands) {
        ArrayOf part(a);
        part.promoteType(allocType);
        promoted.push_back(part);
    }
    return catByBlockCopy(dim, promoted, allocType);
}
//=============================================================================
// Direct N-D concatenation: the column-major layout splits into (inner,
// along-dim, outer); each operand contributes contiguous runs of inner*L
// elements per outer slab. No permutation, so the data is touched once.
static ArrayOf
catByBlockCopy(indexType dim, const ArrayOfVector& parts, NelsonType allocType)
{
    indexType nbDims = dim;
    for (const auto& a : parts) {
        nbDims = std::max(nbDims, (indexType)a.dimensions().length());
    }
    auto axisLength = [](Dimensions d, indexType axis) -> indexType {
        return (axis < (indexType)d.length()) ? (indexType)d[(indexType)axis] : (indexType)1;
    };
    // reference sizes come from the first operand; the along-dim axis accumulates
    std::vector<indexType> refSize(nbDims, 1);
    Dimensions firstDims = parts[0].dimensions();
    for (indexType i = 0; i < nbDims; ++i) {
        refSize[i] = axisLength(firstDims, i);
    }
    indexType alongTotal = 0;
    for (const auto& a : parts) {
        Dimensions d = a.dimensions();
        for (indexType i = 0; i < nbDims; ++i) {
            if (i == dim - 1) {
                continue;
            }
            if (axisLength(d, i) != refSize[i]) {
                raiseConcatenationDimensionMismatch();
            }
        }
        alongTotal += axisLength(d, dim - 1);
    }
    std::vector<indexType> resSize(refSize);
    resSize[dim - 1] = alongTotal;
    Dimensions resDims(resSize);
    resDims.simplify();

    indexType inner = 1;
    for (indexType i = 0; i < dim - 1; ++i) {
        inner *= resSize[i];
    }
    indexType outer = 1;
    for (indexType i = dim; i < nbDims; ++i) {
        outer *= resSize[i];
    }
    indexType elementTotal = resDims.elementCount();
    void* dstPtr = ArrayOf::allocateArrayOf(allocType, elementTotal, stringVector(), true);
    indexType offset = 0;
    for (auto a : parts) {
        indexType L = axisLength(a.dimensions(), dim - 1);
        indexType block = inner * L;
        for (indexType o = 0; o < outer; ++o) {
            indexType srcIndex = o * block;
            indexType dstIndex = o * (inner * alongTotal) + offset * inner;
            a.copyElements(srcIndex, dstPtr, dstIndex, block);
        }
        offset += L;
    }
    return ArrayOf(allocType, resDims, dstPtr, false);
}
//=============================================================================
// Fallback for struct / class / string and other exotic combinations:
// move `dim` to the front, concatenate there (reusing every operator rule),
// then move it back. Once permuted, an empty operand could be dropped by the
// operator, so the sizes are checked after its conversion errors
// (cat(3, zeros(0, 1), o) with o of size 1x1x2 is an error).
static ArrayOf
catByPermute(Evaluator* eval, indexType dim, const ArrayOfVector& arrays)
{
    indexType nbDims = dim;
    for (const auto& a : arrays) {
        nbDims = std::max(nbDims, (indexType)a.dimensions().length());
    }
    std::vector<indexType> forward;
    forward.reserve(nbDims);
    forward.push_back(dim);
    for (indexType i = 1; i <= nbDims; ++i) {
        if (i != dim) {
            forward.push_back(i);
        }
    }
    ArrayOfVector permuted;
    permuted.reserve(arrays.size());
    for (const auto& a : arrays) {
        bool needToOverload = false;
        ArrayOf pa = Permute(a, forward, needToOverload);
        if (needToOverload) {
            Error(L"Nelson:elementary_functions:catCombinationArgumentsSupported",
                L"cat: this combination of arguments is not supported.");
        }
        permuted.push_back(pa);
    }
    ArrayOf stacked = eval->vertcatOperator(permuted);
    for (const auto& a : arrays) {
        checkConcatenationDimensions(arrays[0].dimensions(), a.dimensions(), dim - 1);
    }
    std::vector<indexType> inverse(nbDims, 0);
    for (indexType i = 0; i < (indexType)forward.size(); ++i) {
        inverse[forward[i] - 1] = i + 1;
    }
    bool needToOverload = false;
    ArrayOf result = Permute(stacked, inverse, needToOverload);
    if (needToOverload) {
        Error(L"Nelson:elementary_functions:catCombinationArgumentsSupported",
            L"cat: this combination of arguments is not supported.");
    }
    return result;
}
//=============================================================================
static ArrayOf
catAllEmptyByReshape(indexType dim, const ArrayOfVector& arrays)
{
    // every operand is 0x0: the result keeps the first operand (struct fields,
    // class) with the operands stacked along dim.
    std::vector<indexType> resSize(dim, 1);
    resSize[0] = 0;
    resSize[1] = 0;
    resSize[dim - 1] = (indexType)arrays.size();
    Dimensions resDims(resSize);
    ArrayOf result(arrays[0]);
    result.reshape(resDims);
    return result;
}
//=============================================================================
// A sparse or missing operand follows the conversion rules of horzcat and
// vertcat (all the arrays, 0x0 ones included). Any other nonempty operand of
// another class cannot become a function handle, before any size rule:
// cat(3, zeros(0, 2), @sin, 5) names double. Function handles cannot form
// nonscalar arrays. The operands left once the 0x0 ones are dropped must agree
// like any cat operands, the result keeps the function handle class and may not
// exceed 1 along any dimension: cat(3, @sin, zeros(1, 1, 0)) is @sin,
// cat(3, zeros(0, 1), function_handle.empty) is a 0x1 function handle,
// cat(3, @sin, @cos) is an error.
static ArrayOf
catFunctionHandles(Evaluator* eval, indexType dim, const ArrayOfVector& arrays,
    const ArrayOfVector& operands, const ArrayOf& anyHandle)
{
    for (const auto& a : arrays) {
        if (a.isSparse() || a.dataClass() == NLS_MISSING_ARRAY) {
            // raises the conversion error of the operator
            eval->horzcatOperator(arrays);
            break;
        }
    }
    checkFunctionHandleConcatenationOperands(operands);
    indexType axis = dim - 1;
    Dimensions dims(operands[0].dimensions());
    for (size_t k = 1; k < operands.size(); ++k) {
        Dimensions dimsK(operands[k].dimensions());
        checkConcatenationDimensions(dims, dimsK, axis);
        dims[axis] = dims.dimensionLength(axis) + dimsK.dimensionLength(axis);
    }
    bool nonScalar = false;
    for (indexType k = 0; k < dims.length(); ++k) {
        nonScalar = nonScalar || dims[k] > 1;
    }
    const ArrayOf* handle = nullptr;
    for (const auto& a : operands) {
        if (!a.isEmpty()) {
            nonScalar = nonScalar || handle != nullptr;
            handle = &a;
        }
    }
    if (nonScalar) {
        raiseNonScalarFunctionHandles();
    }
    if (handle != nullptr) {
        return *handle;
    }
    dims.simplify();
    return ArrayOf(NLS_FUNCTION_HANDLE, dims, nullptr, false, anyHandle.fieldNames());
}
//=============================================================================
