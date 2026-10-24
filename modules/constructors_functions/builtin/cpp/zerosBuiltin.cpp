//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zerosBuiltin.hpp"
#include "Error.hpp"
#include "StringHelpers.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Zeros.hpp"
#include "StringToClass.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassName.hpp"
#include "OverloadName.hpp"
#include "FunctionDef.hpp"
#include "Context.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Route zeros(..., 'ClassName') / zeros(..., 'like', obj) to @<class>/zeros(dims...)
// for a non-built-in class. Keeps the core free of any specific object type.
static bool
tryClassConstructor(Evaluator* eval, int nLhs, const std::string& className,
    const ArrayOfVector& dimArgs, ArrayOfVector& result);
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::zerosBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    NelsonType cl = NLS_DOUBLE;
    if (argIn.empty()) {
        retval << Zeros(cl);
    } else {
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
                    case NLS_LOGICAL:
                        cl = NLS_LOGICAL;
                        break;
                    case NLS_INT8:
                        cl = NLS_INT8;
                        break;
                    case NLS_UINT8:
                        cl = NLS_UINT8;
                        break;
                    case NLS_INT16:
                        cl = NLS_INT16;
                        break;
                    case NLS_UINT16:
                        cl = NLS_UINT16;
                        break;
                    case NLS_INT32:
                        cl = NLS_INT32;
                        break;
                    case NLS_UINT32:
                        cl = NLS_UINT32;
                        break;
                    case NLS_INT64:
                        cl = NLS_INT64;
                        break;
                    case NLS_UINT64:
                        cl = NLS_UINT64;
                        break;
                    case NLS_DOUBLE:
                        cl = NLS_DOUBLE;
                        break;
                    case NLS_SINGLE:
                        cl = NLS_SINGLE;
                        break;
                    case NLS_DCOMPLEX:
                        cl = NLS_DCOMPLEX;
                        break;
                    case NLS_SCOMPLEX:
                        cl = NLS_SCOMPLEX;
                        break;
                    default: {
                        // object/handle prototype: route to @<class>/zeros(dims,
                        // prototype) so the overload can match its class
                        std::string className;
                        ClassName(lastArg, className);
                        ArrayOfVector dimArgs;
                        for (size_t k = 0; k + 2 < argIn.size(); k++) {
                            dimArgs.push_back(argIn[k]);
                        }
                        dimArgs.push_back(lastArg);
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
        if ((lastArg.isRowVectorCharacterArray() || lastArg.isScalarStringArray())
            && bCheckClassName) {
            std::string requestedType = lastArg.asUtf8String();
            // a class with a constructor overload wins (route before StringToClass,
            // which errors on names that are not built-in types)
            ArrayOfVector dimArgs;
            for (size_t k = 0; k + 1 < argIn.size(); k++) {
                dimArgs.push_back(argIn[k]);
            }
            ArrayOfVector result;
            if (tryClassConstructor(eval, nLhs, requestedType, dimArgs, result)) {
                return result;
            }
            cl = StringToClass(requestedType);
            nRhs--;
            if (cl > NLS_LOGICAL) {
                Error(L"Nelson:constructors_functions:supportedTypeExpectedLastArgument",
                    L"A supported type expected at last argument.");
            }
        }
        if (nRhs == 0) {
            retval << Zeros(cl);
            return retval;
        }
        Dimensions dims;
        if (nRhs == 1) {
            if ((argIn[0].isNumeric() || argIn[0].dataClass() == NLS_LOGICAL)
                && !argIn[0].isSparse()) {
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
        retval << Zeros(dims, cl);
    }
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
    std::string overloadName = getOverloadFunctionName(className, "zeros");
    FunctionDefPtr funcDef = nullptr;
    if (eval->getContext()->lookupFunction(overloadName, funcDef) && funcDef != nullptr) {
        result = funcDef->evaluateFunction(eval, dimArgs, nLhs);
        return true;
    }
    return false;
}
//=============================================================================
