//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "FindCommonType.hpp"
#include "IEEEFP.hpp"
#include "IntegerCompatibilityHelpers.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "ClassdefParser.hpp"
#include "OverloadName.hpp"
#include "Context.hpp"
#include "ErrorID.hpp"
#include "ConcatenateDimensions.hpp"
#include "FunctionHandleConcatenation.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct BinaryOperatorCommonType
{
    std::string typeName = NLS_UNKNOWN_STR;
    NelsonType type = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;
};
//=============================================================================
enum class BinaryOperatorSparseMode
{
    None,
    BothOperands,
    RightOperandWhenLeftIsSparse
};
//=============================================================================
inline NelsonType
promotedBinaryNumericType(const BinaryOperatorCommonType& commonType)
{
    NelsonType promotedType = commonType.type;
    if (promotedType == NLS_CHAR) {
        promotedType = NLS_DOUBLE;
    }
    if (promotedType == NLS_DOUBLE && commonType.isComplex) {
        promotedType = NLS_DCOMPLEX;
    }
    if (promotedType == NLS_SINGLE && commonType.isComplex) {
        promotedType = NLS_SCOMPLEX;
    }
    return promotedType;
}
//=============================================================================
inline void
checkBinaryIntegerCompatibility(const ArrayOf& otherValue)
{
    bool isCompatible = isScalarDoubleOrChar(otherValue);
    if (!isCompatible) {
        Error(L"Nelson:interpreter:integerClassMismatch",
            L"Integers can only be combined with integers of the same class, or scalar doubles.");
    }
}
//=============================================================================
inline void
promoteBinaryCharOperands(const BinaryOperatorCommonType& commonType, ArrayOf& A, ArrayOf& B)
{
    if (commonType.type <= NLS_CHAR && (A.dataClass() == NLS_CHAR || B.dataClass() == NLS_CHAR)) {
        NelsonType targetType = promotedBinaryNumericType(commonType);
        if (A.dataClass() != targetType) {
            A.promoteType(targetType);
        }
        if (B.dataClass() != targetType) {
            B.promoteType(targetType);
        }
    }
}
//=============================================================================
inline void
checkBinaryIntegralPowerExponent(const ArrayOf& B)
{
    if (B.dataClass() != NLS_CHAR) {
        auto* ptrB = (double*)B.dataPointer();
        indexType elementCount = B.elementCount();
        bool allIntegerValue = IsIntegerForm(ptrB, elementCount);
        if (!allIntegerValue) {
            Error(L"Nelson:interpreter:positiveIntegralPowerExpected",
                L"Positive integral powers expected.");
        }
    }
}
//=============================================================================
// When the plain overload-name lookup fails for a classdef operand, the operator
// may be defined by a superclass. Resolve it along the inheritance chain (which
// the flat overload-name lookup does not walk) and invoke it. Returns true and
// fills result on success.
inline bool
tryCallClassdefInheritedOverload(
    Evaluator* eval, const ArrayOfVector& args, const std::string& functionName, ArrayOf& result)
{
    if (eval == nullptr || !eval->withOverload) {
        return false;
    }
    std::string previousClassName;
    for (const ArrayOf& operand : args) {
        if (!operand.isClassType() && !operand.isHandle()) {
            continue;
        }
        const std::string className
            = operand.isHandle() ? operand.handleClassName() : operand.classType();
        if (className == previousClassName) {
            continue;
        }
        previousClassName = className;
        std::string resolvedName;
        if (!className.empty() && classdefLoadClass(className)
            && classdefResolveMethodFunction(
                className, functionName, resolvedName, eval->getClassdefAccessContext())) {
            FunctionDef* funcDef = nullptr;
            if (eval->getContext() != nullptr
                && eval->getContext()->lookupFunction(resolvedName, funcDef)
                && funcDef != nullptr) {
                ArrayOfVector retval = funcDef->evaluateFunction(eval, args, 1);
                if (!retval.empty()) {
                    result = retval[0];
                    return true;
                }
            }
        }
    }
    return false;
}
//=============================================================================
// Operator overload lookup shared by every operator: the exact-class overload
// first, then (only when it missed) a method inherited from a superclass.
inline ArrayOf
callOperatorOverload(Evaluator* eval, const ArrayOfVector& args, const std::string& functionName,
    const std::string& commonTypeName, NelsonType commonType, bool& wasFound)
{
    ArrayOf res = callOverloadedFunction(eval,
        NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), args, functionName,
        commonTypeName, commonType, wasFound);
    if (!wasFound) {
        wasFound = tryCallClassdefInheritedOverload(eval, args, functionName, res);
    }
    return res;
}
//=============================================================================
inline ArrayOf
callOperatorOverload(
    Evaluator* eval, const ArrayOf& A, const std::string& functionName, bool& wasFound)
{
    ArrayOfVector args;
    args << A;
    return callOperatorOverload(eval, args, functionName, ClassName(A), A.dataClass(), wasFound);
}
//=============================================================================
inline bool
tryCallBinaryOperatorOverload(Evaluator* eval, const ArrayOfVector& args,
    const std::string& functionName, BinaryOperatorCommonType& commonType, ArrayOf& result)
{
    if (FindCommonType(args, commonType.type, commonType.isSparse, commonType.isComplex,
            commonType.typeName)) {
        bool overloadWasFound = false;
        result = callOperatorOverload(
            eval, args, functionName, commonType.typeName, commonType.type, overloadWasFound);
        return overloadWasFound;
    }
    return false;
}
//=============================================================================
inline ArrayOf
callBinaryOperatorOverloadOrRequire(Evaluator* eval, const ArrayOfVector& args,
    const std::string& functionName, const BinaryOperatorCommonType& commonType)
{
    bool overloadWasFound = false;
    ArrayOf result = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, args, functionName,
        commonType.typeName, commonType.type, overloadWasFound);
    if (!overloadWasFound) {
        OverloadRequired(functionName);
    }
    return result;
}
//=============================================================================
inline void
promoteBinaryLogicalOperands(const BinaryOperatorCommonType& commonType, ArrayOf& A, ArrayOf& B)
{
    // logical operands are arithmetic doubles (true * true is 1 of class double)
    if (commonType.type == NLS_LOGICAL && !commonType.isSparse) {
        A.promoteType(NLS_DOUBLE);
        B.promoteType(NLS_DOUBLE);
    }
}
//=============================================================================
inline void
prepareBinaryNumericOperands(const BinaryOperatorCommonType& commonType, ArrayOf& A, ArrayOf& B)
{
    promoteBinaryLogicalOperands(commonType, A, B);
    if (A.dataClass() != B.dataClass()) {
        if (A.isIntegerType()) {
            checkBinaryIntegerCompatibility(B);
        } else if (B.isIntegerType()) {
            checkBinaryIntegerCompatibility(A);
        } else if (commonType.type <= NLS_CHAR) {
            NelsonType targetType = promotedBinaryNumericType(commonType);
            A.promoteType(targetType);
            B.promoteType(targetType);
        }
    }
    promoteBinaryCharOperands(commonType, A, B);
}
//=============================================================================
inline void
prepareBinaryPowerOperands(const BinaryOperatorCommonType& commonType, ArrayOf& A, ArrayOf& B)
{
    promoteBinaryLogicalOperands(commonType, A, B);
    if (A.dataClass() != B.dataClass()) {
        if (A.isIntegerType()) {
            checkBinaryIntegerCompatibility(B);
            checkBinaryIntegralPowerExponent(B);
            A.promoteType(commonType.type);
            B.promoteType(commonType.type);
        } else if (B.isIntegerType()) {
            checkBinaryIntegerCompatibility(A);
            A.promoteType(commonType.type);
            B.promoteType(commonType.type);
        } else if (commonType.type <= NLS_CHAR) {
            NelsonType targetType = promotedBinaryNumericType(commonType);
            A.promoteType(targetType);
            B.promoteType(targetType);
        }
    }
    promoteBinaryCharOperands(commonType, A, B);
}
//=============================================================================
inline void
applyBinaryOperatorSparseMode(BinaryOperatorSparseMode sparseMode,
    const BinaryOperatorCommonType& commonType, ArrayOf& A, ArrayOf& B)
{
    if (!commonType.isSparse) {
        return;
    }
    switch (sparseMode) {
    case BinaryOperatorSparseMode::BothOperands: {
        A.makeSparse();
        B.makeSparse();
    } break;
    case BinaryOperatorSparseMode::RightOperandWhenLeftIsSparse: {
        if (A.isSparse()) {
            B.makeSparse();
        }
    } break;
    case BinaryOperatorSparseMode::None:
    default: {
    } break;
    }
}
//=============================================================================
// A nonempty operand of another class cannot become a function handle.
// Function handles cannot form nonscalar arrays. Next to a single handle the
// 2-D empty operands of any class are dropped (an N-D one must agree); empty
// operands alone keep the size of their plain concatenation, which may not
// exceed 1 along any dimension. axis is the 0-based concatenated dimension.
inline ArrayOf
concatenateFunctionHandles(const ArrayOfVector& args, indexType axis)
{
    checkFunctionHandleConcatenationOperands(args);
    const ArrayOf* handle = nullptr;
    const ArrayOf* emptyHandle = nullptr;
    for (const ArrayOf& arg : args) {
        if (!arg.isEmpty()) {
            if (handle != nullptr) {
                raiseNonScalarFunctionHandles();
            }
            handle = &arg;
        } else if (emptyHandle == nullptr && arg.isFunctionHandle()) {
            emptyHandle = &arg;
        }
    }
    if (handle != nullptr) {
        for (const ArrayOf& arg : args) {
            if (arg.isEmpty()) {
                checkEmptyOperandDimensions(arg.dimensions(), handle->dimensions(), axis);
            }
        }
        return *handle;
    }
    Dimensions dims;
    allEmptyOperandsDimensions(args, axis, dims);
    for (indexType k = 0; k < dims.length(); ++k) {
        if (dims[k] > 1) {
            raiseNonScalarFunctionHandles();
        }
    }
    return ArrayOf(NLS_FUNCTION_HANDLE, dims, nullptr, false, emptyHandle->fieldNames());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
