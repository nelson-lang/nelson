//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isequaltoBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsEqualTo.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequaltoBuiltin(
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
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn, "isequalto",
            commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    if (isSparse
        && (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX && commonType != NLS_LOGICAL)) {
        Error(_("Attempt to convert to unimplemented sparse type"), "Nelson:UnableToConvert");
    }

    bool result = false;
    bool needToOverload = false;

    /*
    switch (commonType) {
    case NLS_STRUCT_ARRAY: {
        result = structIsEqualTo(eval, argIn, needToOverload);
    } break;
    case NLS_CELL_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE: {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, argIn, "isequalto",
            commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        } else {
            OverloadRequired("isequalto");
        }
    } break;
    default: {
        result = IsEqualTo(argIn, needToOverload);
    } break;
    }
    */
    result = IsEqualTo(eval, argIn, needToOverload);
    if (needToOverload) {
        OverloadRequired("isequalto");
    }
    return ArrayOf::logicalConstructor(result);
}
//=============================================================================
static bool
structIsEqualTo(Evaluator* eval, const ArrayOfVector& argIn, bool& needOverload)
{
    /*    needOverload = false;
    ArrayOf firstElement = argIn[0];
    NelsonType commmonType = NLS_STRUCT_ARRAY;
    Dimensions commonDimensions = argIn[0].getDimensions();
    stringVector commonFieldNames = argIn[0].getFieldNames();
    bool commonSparse = argIn[0].isSparse();
    for (auto k = 1; k < argIn.size(); ++k) {
        if ((argIn[k].getDataClass() != commmonType)
            || (!argIn[k].getDimensions().equals(commonDimensions))) {
            return false;
        }
        stringVector currentFieldNames = argIn[k].getFieldNames();

        bool equal = std::equal(commonFieldNames.begin(), commonFieldNames.end(),
            currentFieldNames.begin(),
            currentFieldNames.end());
        if (!equal) {
            return false;
        }
        for (auto name : commonFieldNames) {
            ArrayOfVector args;
            args << firstElement.getField(name);
            args << argIn[k].getField(name);
            if (!IsEqualTo(args, needOverload)) {
                return false;
            }
        }
    }
    */
    return true;
}
//=============================================================================
