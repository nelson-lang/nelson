//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "infBuiltin.hpp"
#include "Error.hpp"
#include "StringHelpers.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Inf.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassName.hpp"
#include "OverloadName.hpp"
#include "FunctionDef.hpp"
#include "Context.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Route inf(..., 'ClassName') / inf(..., 'like', obj) to @<class>/inf(dims...) for a
// non-built-in class (see zerosBuiltin.cpp).
static bool
tryClassConstructor(Evaluator* eval, int nLhs, const std::string& className,
    const ArrayOfVector& dimArgs, ArrayOfVector& result);
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::infBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    // Inf/NaN are floating-point constructors: only 'single' and 'double' are allowed.
    NelsonType cl = NLS_DOUBLE;
    if (argIn.empty()) {
        retval << Inf();
        return retval;
    }
    sizeType nRhs = argIn.size();
    bool bCheckClassName = true;
    if (static_cast<int>(nRhs) - 2 >= 0) {
        ArrayOf Arg = argIn[argIn.size() - 2];
        if (Arg.isRowVectorCharacterArray() || Arg.isScalarStringArray()) {
            std::wstring paramstr = Arg.asWideString();
            if (StringHelpers::iequals(paramstr, L"like")) {
                ArrayOf lastArg = argIn[argIn.size() - 1];
                if (lastArg.isSparse()) {
                    Error(L"Nelson:constructors_functions:supportedTypeExpectedLastArgument",
                        L"A supported type expected at last argument.");
                }
                switch (lastArg.dataClass()) {
                case NLS_DOUBLE:
                case NLS_DCOMPLEX:
                    cl = NLS_DOUBLE;
                    break;
                case NLS_SINGLE:
                case NLS_SCOMPLEX:
                    cl = NLS_SINGLE;
                    break;
                default: {
                    // Object/handle prototype: the class is not forwarded because
                    // Inf/NaN are floating-point only; the overload picks the type.
                    std::string className;
                    ClassName(lastArg, className);
                    ArrayOfVector dimArgs;
                    for (size_t k = 0; k + 2 < argIn.size(); k++) {
                        dimArgs.push_back(argIn[k]);
                    }
                    ArrayOfVector result;
                    if (tryClassConstructor(eval, nLhs, className, dimArgs, result)) {
                        return result;
                    }
                    Error(L"Nelson:constructors_functions:supportedTypeExpectedLastArgument",
                        L"A supported type expected at last argument.");
                } break;
                }
                nRhs = nRhs - 2;
                bCheckClassName = false;
            } else {
                Error(L"Nelson:constructors_functions:likeExpectedN2Argument",
                    L"\'like\' expected at n - 2 argument.");
            }
        }
    }
    ArrayOf lastArg = argIn[argIn.size() - 1];
    if ((lastArg.isRowVectorCharacterArray() || lastArg.isScalarStringArray()) && bCheckClassName) {
        std::string requestedType = lastArg.asUtf8String();
        // a class with a constructor overload wins (route before the built-in check)
        ArrayOfVector dimArgs;
        for (size_t k = 0; k + 1 < argIn.size(); k++) {
            dimArgs.push_back(argIn[k]);
        }
        ArrayOfVector result;
        if (tryClassConstructor(eval, nLhs, requestedType, dimArgs, result)) {
            return result;
        }
        if (requestedType == "double") {
            cl = NLS_DOUBLE;
        } else if (requestedType == "single") {
            cl = NLS_SINGLE;
        } else {
            Error(L"Nelson:constructors_functions:supportedTypeExpectedLastArgument",
                L"A supported type expected at last argument.");
        }
        nRhs--;
    }
    if (nRhs == 0) {
        Dimensions dims(1, 1);
        retval << Inf(dims, cl);
        return retval;
    }
    Dimensions dims;
    if (nRhs == 1) {
        if ((argIn[0].isNumeric() || argIn[0].dataClass() == NLS_LOGICAL) && !argIn[0].isSparse()) {
            if (argIn[0].isEmpty()) {
                dims[0] = 0;
                dims[1] = 0;
            } else if (argIn[0].isRowVector()) {
                if (argIn[0].elementCount() < Nelson::maxDims) {
                    ArrayOf dimVector(argIn[0]);
                    auto* ptrValues = static_cast<indexType*>(dimVector.indexPointer());
                    ompIndexType elementCount = dimVector.elementCount();
                    for (ompIndexType k = 0; k < elementCount; k++) {
                        dims[k] = static_cast<indexType>(ptrValues[k]);
                    }
                    if (dims.length() == 1) {
                        dims[1] = dims[0];
                    }
                } else {
                    RaiseError(resolveMessageTemplate(
                                   L"Nelson:constructors_functions:tooManyDimensionsCurrent",
                                   L"Too many dimensions! Current limit is")
                            + L" " + std::to_wstring(Nelson::maxDims) + L".",
                        L"Nelson:constructors:tooManyDimensions");
                }
            } else {
                Error(L"Nelson:expectedRow", L"%s must be a row vector.", L"Argument #1");
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"numeric value");
        }
    } else {
        for (sizeType k = 0; k < nRhs; k++) {
            dims[k] = argIn[k].isEmpty() ? 0 : argIn[k].asScalarIndex(true, true, true);
        }
        if (dims.length() == 1) {
            dims[1] = dims[0];
        }
    }
    retval << Inf(dims, cl);
    return retval;
}
//=============================================================================
static bool
tryClassConstructor(Evaluator* eval, int nLhs, const std::string& className,
    const ArrayOfVector& dimArgs, ArrayOfVector& result)
{
    if (eval == nullptr) {
        return false;
    }
    std::string overloadName = getOverloadFunctionName(className, "inf");
    FunctionDefPtr funcDef = nullptr;
    if (eval->getContext()->lookupFunction(overloadName, funcDef) && funcDef != nullptr) {
        result = funcDef->evaluateFunction(eval, dimArgs, nLhs);
        return true;
    }
    return false;
}
//=============================================================================
