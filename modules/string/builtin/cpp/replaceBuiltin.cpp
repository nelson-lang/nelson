//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "replaceBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "StringReplace.hpp"
#include "StringPattern.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// A missing element of a string array is a non character array (empty double).
// Any text transformation must leave those elements as <missing> in the output.
static void
preserveStringMissing(const ArrayOf& STR, ArrayOf& res)
{
    if (!STR.isStringArray() || !res.isStringArray()) {
        return;
    }
    auto* strElements = (ArrayOf*)STR.dataPointer();
    indexType nbStr = STR.elementCount();
    indexType nbRes = res.elementCount();
    if (STR.isScalar()) {
        if (!strElements[0].isCharacterArray()) {
            res.ensureSingleOwner();
            auto* resElements = (ArrayOf*)res.dataPointer();
            for (indexType k = 0; k < nbRes; k++) {
                resElements[k] = ArrayOf::empty();
            }
        }
        return;
    }
    if (nbStr != nbRes || !STR.dimensions().equals(res.dimensions())) {
        return;
    }
    res.ensureSingleOwner();
    auto* resElements = (ArrayOf*)res.dataPointer();
    for (indexType k = 0; k < nbStr; k++) {
        if (!strElements[k].isCharacterArray()) {
            resElements[k] = ArrayOf::empty();
        }
    }
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::replaceBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 3, 3);
    if (IsPatternObject(argIn[1])) {
        bool needToOverload = false;
        ArrayOf res = PatternReplace(
            argIn[0], PatternExpression(argIn[1]), argIn[2].asWideString(), false, needToOverload);
        if (needToOverload) {
            Error(L"Nelson:string:invalidInputArgumentSCell",
                L"Invalid input argument(s): cell or string expected.");
        }
        preserveStringMissing(argIn[0], res);
        retval << res;
        return retval;
    }
    bool needToOverload;
    ArrayOf res = Replace(argIn[0], argIn[1], argIn[2], needToOverload);
    if (needToOverload) {
        Error(L"Nelson:string:invalidInputArgumentSCell",
            L"Invalid input argument(s): cell or string expected.");
    } else {
        preserveStringMissing(argIn[0], res);
        retval << res;
    }
    return retval;
}
//=============================================================================
