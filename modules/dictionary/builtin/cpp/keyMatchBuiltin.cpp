//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "keyMatchBuiltin.hpp"
#include "keyHashBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassName.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DictionaryGateway::keyMatchBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf res;
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;

    if (FindCommonType(argIn, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn, "keyMatch",
            commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    // keys match only if they have the same class and equal values
    // (NaN keys are considered equal, as isequaln does).
    ArrayOfVector retval;
    std::string classA;
    std::string classB;
    ClassName(argIn[0], classA);
    ClassName(argIn[1], classB);
    if (classA != classB) {
        retval << ArrayOf::logicalScalar(false);
        return retval;
    }
    FunctionDefPtr funcDef = nullptr;
    if (eval != nullptr && eval->getContext()->lookupFunction("isequaln", funcDef)
        && funcDef != nullptr) {
        ArrayOfVector args;
        args << argIn[0];
        args << argIn[1];
        ArrayOfVector resIsEqualN = funcDef->evaluateFunction(eval, args, 1);
        bool match = !resIsEqualN.empty() && resIsEqualN[0].isLogical() && resIsEqualN[0].isScalar()
            && (resIsEqualN[0].asLogicalScalar() != 0u);
        retval << ArrayOf::logicalScalar(match);
        return retval;
    }
    // fallback: compare hashes (keyHash normalizes -0.0 to +0.0)
    ArrayOfVector h1 = keyHashBuiltin(eval, 1, argIn[0]);
    ArrayOfVector h2 = keyHashBuiltin(eval, 1, argIn[1]);
    retval << ArrayOf::logicalScalar(h1[0].asUint64Scalar() == h2[0].asUint64Scalar());
    return retval;
}
//=============================================================================
