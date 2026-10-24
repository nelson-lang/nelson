//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "randBuiltin.hpp"
#include <cmath>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Rand.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassName.hpp"
#include "OverloadName.hpp"
#include "FunctionDef.hpp"
#include "Context.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Route rand(..., 'ClassName') / rand(..., 'like', obj) to @<class>/rand for a
// non-built-in class (see constructors_functions/zerosBuiltin.cpp).
static bool
tryClassConstructor(Evaluator* eval, int nLhs, const std::string& className,
    const ArrayOfVector& dimArgs, ArrayOfVector& result);
//=============================================================================
ArrayOfVector
Nelson::RandomGateway::randBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    NelsonType cl = NLS_DOUBLE;
    if (argIn.size() == 0) {
        retval << Rand(cl);
    } else {
        sizeType nRhs = argIn.size();
        bool bCheckClassName = true;
        if ((int)nRhs - 2 >= 0) {
            ArrayOf Arg = argIn[argIn.size() - 2];
            if (Arg.isRowVectorCharacterArray() || Arg.isScalarStringArray()) {
                std::wstring paramstr = Arg.asWideString();
                if (paramstr == L"like") {
                    ArrayOf lastArg = argIn[argIn.size() - 1];
                    switch (lastArg.dataClass()) {
                    case NLS_SCOMPLEX:
                        cl = NLS_SCOMPLEX;
                        break;
                    case NLS_DCOMPLEX:
                        cl = NLS_DCOMPLEX;
                        break;
                    case NLS_DOUBLE:
                        cl = NLS_DOUBLE;
                        break;
                    case NLS_SINGLE:
                        cl = NLS_SINGLE;
                        break;
                    default: {
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
                        Error(L"Nelson:types:singleOrDoubleExpected",
                            L"\'single\' or \'double\' expected at last argument.");
                    } break;
                    }
                    nRhs = nRhs - 2;
                    bCheckClassName = false;
                } else {
                    Error(L"Nelson:random:invalidLike", L"\'like\' expected at n - 2 argument.");
                }
            }
        }
        ArrayOf lastArg = argIn[argIn.size() - 1];
        if (lastArg.isRowVectorCharacterArray() && bCheckClassName) {
            std::wstring paramstr = lastArg.asWideString();
            if (paramstr == L"double") {
                cl = NLS_DOUBLE;
                nRhs--;
            } else if (paramstr == L"single") {
                cl = NLS_SINGLE;
                nRhs--;
            } else {
                std::string requestedType = lastArg.asUtf8String();
                ArrayOfVector dimArgs;
                for (size_t k = 0; k + 1 < argIn.size(); k++) {
                    dimArgs.push_back(argIn[k]);
                }
                ArrayOfVector result;
                if (tryClassConstructor(eval, nLhs, requestedType, dimArgs, result)) {
                    return result;
                }
                Error(L"Nelson:types:singleOrDoubleExpected",
                    L"\'single\' or \'double\' expected at last argument.");
            }
        }
        if (nRhs == 0) {
            retval << Rand(cl);
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
                        ArrayOf dimVector = argIn[0];
                        dimVector.promoteType(NLS_DOUBLE);
                        double* ptrValues = (double*)dimVector.dataPointer();
                        ompIndexType elementCount = argIn[0].elementCount();
                        for (ompIndexType k = 0; k < elementCount; k++) {
                            if (!std::isfinite(ptrValues[k])
                                || std::floor(ptrValues[k]) != ptrValues[k]) {
                                Error(L"Nelson:integerValueExpected",
                                    L"A real integer value scalar expected.");
                            }
                            dims[k] = (ptrValues[k] > 0) ? (indexType)ptrValues[k] : 0;
                        }
                        if (dims.length() == 1) {
                            dims[1] = dims[0];
                        }
                    } else {
                        Error(L"Nelson:random:tooManyDimensions",
                            L"Too many dimensions! Current limit is %d.", (int)Nelson::maxDims);
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
                ArrayOf param = argIn[k];
                indexType idx = param.isEmpty() ? 0 : param.asScalarIndex(true, true, true);
                dims[k] = idx;
            }
            if (dims.length() == 1) {
                dims[1] = dims[0];
            }
        }
        retval << Rand(dims, cl);
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
    std::string overloadName = getOverloadFunctionName(className, "rand");
    FunctionDefPtr funcDef = nullptr;
    if (eval->getContext()->lookupFunction(overloadName, funcDef) && funcDef != nullptr) {
        result = funcDef->evaluateFunction(eval, dimArgs, nLhs);
        return true;
    }
    return false;
}
//=============================================================================
