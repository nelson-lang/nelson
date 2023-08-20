//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isequalBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsEqual.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequalBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2);

    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;
    std::string commonTypeName = NLS_UNKNOWN_STR;

    ArrayOf res;
    if (FindCommonType(argIn, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn, "isequal",
            commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    if (isSparse
        && (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX && commonType != NLS_LOGICAL)) {
        Error(_("Attempt to convert to unimplemented sparse type"), "Nelson:UnableToConvert");
    }

    bool needToOverload = false;
    bool result = IsEqual(eval, argIn, commonType, isSparse, isComplex, needToOverload);
    if (needToOverload) {
        OverloadRequired("isequal");
    }
    return ArrayOf::logicalConstructor(result);
}
//=============================================================================
