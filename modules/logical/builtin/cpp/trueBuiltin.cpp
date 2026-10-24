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
#include "trueBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "LogicalConstructors.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassName.hpp"
#include "OverloadName.hpp"
#include "FunctionDef.hpp"
#include "Context.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Route true(..., 'ClassName') / true(..., 'like', obj) to @<class>/true for a
// non-built-in class (see constructors_functions/zerosBuiltin.cpp).
static ArrayOfVector
tryClassConstructor(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool& delegated);
//=============================================================================
ArrayOfVector
Nelson::LogicalGateway::trueBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool bIsSparse = false;
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    bool delegated = false;
    ArrayOfVector delegatedResult = tryClassConstructor(eval, nLhs, argIn, delegated);
    if (delegated) {
        return delegatedResult;
    }
    if (argIn.empty()) {
        Dimensions dim(1, 1);
        retval << TrueConstructor(dim, bIsSparse);
    } else {
        Dimensions dim;
        indexType idxMax = argIn.size();
        if ((static_cast<double>(argIn.size()) - 2.) >= 0.) {
            indexType pos = argIn.size() - 2;
            if (argIn[pos].isRowVectorCharacterArray() || argIn[pos].isScalarStringArray()) {
                std::wstring arg = argIn[pos].asWideString();
                if (arg.compare(L"like") == 0) {
                    ArrayOf arg = argIn[pos + 1];
                    if (arg.dataClass() != NLS_LOGICAL) {
                        Error(L"Nelson:logical:invalidLike",
                            L"Input following \'like\' is not a logical array.");
                    }
                    bIsSparse = arg.isSparse();
                    idxMax = argIn.size() - 2;
                    if (idxMax == 0) {
                        dim[0] = 1;
                        dim[1] = 1;
                    }
                } else {
                    Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (pos + 1));
                }
            }
        }
        if (idxMax > 0) {
            // true(2, 'logical') names the class explicitly
            ArrayOf last = argIn[idxMax - 1];
            if (last.isRowVectorCharacterArray() || last.isScalarStringArray()) {
                if (last.asWideString() != L"logical") {
                    Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.",
                        static_cast<int>(idxMax));
                }
                idxMax--;
            }
        }
        if (idxMax > 0) {
            dim = LogicalConstructorDimensions(argIn, idxMax);
        } else {
            dim[0] = 1;
            dim[1] = 1;
        }
        retval << TrueConstructor(dim, bIsSparse);
    }
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
    std::string overloadName = getOverloadFunctionName(className, "true");
    FunctionDefPtr funcDef = nullptr;
    if (eval->getContext()->lookupFunction(overloadName, funcDef) && funcDef != nullptr) {
        delegated = true;
        return funcDef->evaluateFunction(eval, dimArgs, nLhs);
    }
    return {};
}
//=============================================================================
