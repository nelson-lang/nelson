//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isequalnBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsEqualNaN.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequalnBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2);

    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;

    ArrayOf res;
    if (FindCommonType(argIn, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn, "isequaln",
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
    bool result = IsEqualNaN(eval, argIn, commonType, isSparse, isComplex, needToOverload);
    if (needToOverload) {
        OverloadRequired("isequaln");
    }
    return ArrayOf::logicalConstructor(result);
}
//=============================================================================
