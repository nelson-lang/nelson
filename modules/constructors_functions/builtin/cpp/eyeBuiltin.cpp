//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "eyeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Eye.hpp"
#include "StringToClass.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassName.hpp"
#include "OverloadName.hpp"
#include "FunctionDef.hpp"
#include "Context.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Route eye(..., 'ClassName') / eye(..., 'like', obj) to @<class>/eye for a
// non-built-in class (see constructors_functions/zerosBuiltin.cpp).
static ArrayOfVector
tryClassConstructor(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool& delegated);
//=============================================================================
static void
extractDestClassAndSparse(
    const ArrayOfVector& argIn, sizeType& nRhs, NelsonType& destClass, bool& bIsSparse)
{
    if (nRhs == 0) {
        return;
    }
    ArrayOf lastarg = argIn[nRhs - 1];
    if (lastarg.isRowVectorCharacterArray()) {
        destClass = StringToClass(lastarg.asUtf8String());
        nRhs--;
        return;
    }
    if (nRhs >= 2) {
        ArrayOf secondLast = argIn[nRhs - 2];
        if (secondLast.isRowVectorCharacterArray() || secondLast.isScalarStringArray()) {
            std::wstring arg = secondLast.asWideString();
            if (arg == L"like") {
                bIsSparse = argIn[nRhs - 1].isSparse();
                destClass = argIn[nRhs - 1].dataClass();
                nRhs -= 2;
                return;
            } else {
                // report a 1-based argument index to the user
                int humanArgIndex = static_cast<int>(nRhs - 2) + 1;
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (humanArgIndex));
            }
        }
    }
}
//=============================================================================
static void
parseDimensions(const ArrayOfVector& argIn, sizeType nRhs, indexType& n, indexType& m)
{
    if (nRhs == 0) {
        n = 1;
        m = 1;
        return;
    }
    if (nRhs == 1) {
        const ArrayOf& a = argIn[0];
        if (a.isEmpty() && (a.isNumeric() || a.dataClass() == NLS_LOGICAL)) {
            n = 0;
            m = 0;
            return;
        }
        if (a.isScalar()) {
            n = a.asScalarIndex(true, true, true);
            m = n;
            return;
        } else if (a.isRowVector()) {
            if (a.elementCount() == 2) {
                ArrayOf dimVector(a);
                indexType* pIndex = dimVector.indexPointer();
                n = pIndex[0];
                m = pIndex[1];
                delete[] pIndex;
                return;
            } else {
                Error(L"Nelson:constructors_functions:nDimensionalArraysSupported",
                    L"N-dimensional arrays are not supported.");
            }
        } else {
            Error(L"Nelson:constructors_functions:sizeVectorRowVectorReal",
                L"Size vector should be a row vector with real elements.");
        }
    } else if (nRhs == 2) {
        n = argIn[0].isEmpty() ? 0 : argIn[0].asScalarIndex(true, true, true);
        m = argIn[1].isEmpty() ? 0 : argIn[1].asScalarIndex(true, true, true);
        return;
    } else {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
}
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::eyeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool bIsSparse = false;
    NelsonType destClass = NLS_DOUBLE;
    indexType n = 1;
    indexType m = 1;
    nargoutcheck(nLhs, 0, 1);
    bool delegated = false;
    ArrayOfVector delegatedResult = tryClassConstructor(eval, nLhs, argIn, delegated);
    if (delegated) {
        return delegatedResult;
    }
    ArrayOfVector retval(1);
    sizeType nRhs = argIn.size();

    extractDestClassAndSparse(argIn, nRhs, destClass, bIsSparse);

    parseDimensions(argIn, nRhs, n, m);

    retval << Eye(n, m, destClass, bIsSparse);
    return retval;
}
//=============================================================================
static ArrayOfVector
tryClassConstructor(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool& delegated)
{
    delegated = false;
    if (eval == nullptr || argIn.empty()) {
        return {};
    }
    std::string className;
    ArrayOfVector dimArgs;
    ArrayOf last = argIn[argIn.size() - 1];
    if (argIn.size() >= 2) {
        ArrayOf prev = argIn[argIn.size() - 2];
        if ((prev.isRowVectorCharacterArray() || prev.isScalarStringArray())
            && prev.asWideString() == L"like") {
            ClassName(last, className);
            for (size_t k = 0; k + 2 < argIn.size(); k++) {
                dimArgs.push_back(argIn[k]);
            }
            // keep the prototype so @gpuArray/eye can match its class
            dimArgs.push_back(last);
        }
    }
    if (className.empty() && last.isRowVectorCharacterArray()) {
        className = last.asUtf8String();
        for (size_t k = 0; k + 1 < argIn.size(); k++) {
            dimArgs.push_back(argIn[k]);
        }
    }
    if (className.empty()) {
        return {};
    }
    std::string overloadName = getOverloadFunctionName(className, "eye");
    FunctionDefPtr funcDef = nullptr;
    if (eval->getContext()->lookupFunction(overloadName, funcDef) && funcDef != nullptr) {
        delegated = true;
        return funcDef->evaluateFunction(eval, dimArgs, nLhs);
    }
    return {};
}
//=============================================================================
