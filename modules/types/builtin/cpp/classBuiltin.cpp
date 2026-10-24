//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "classBuiltin.hpp"
#include "ClassdefParser.hpp"
#include "ClassName.hpp"
#include "ClassToString.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::classBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    if (argIn.size() == 1) {
        std::string str
            = argIn[0].isSparse() ? ClassToString(argIn[0].dataClass()) : ClassName(argIn[0]);
        retval << ArrayOf::characterArray(str);
        return retval;
    }
    // Old-style (pre-classdef) object constructor:
    //   obj = class(s, 'ClassName', parent1, parent2, ...)
    // Tags the structure s as an object of class 'ClassName'; the object's
    // methods are resolved from the @ClassName directory. Parent arguments name
    // classes the new object also derives from. The multi-input form is only
    // valid from within the ClassName constructor.
    const stringVector callers = eval->getCallers(true);
    // getCallers returns the chain from outermost to the immediate caller, so the
    // function that called class() is the last entry.
    const bool insideNamedConstructor = argIn[1].isRowVectorCharacterArray() && !callers.empty()
        && (callers.back() == argIn[1].asUtf8String());
    if (!insideNamedConstructor) {
        Error("Nelson:types:classOutsideConstructor",
            "When called from outside a class constructor, the CLASS function must be called with "
            "one input.");
    }
    if (argIn[0].dataClass() != NLS_STRUCT_ARRAY) {
        Error("Nelson:types:structExpected", "First argument must be a structure array.");
    }
    for (size_t k = 1; k < argIn.size(); ++k) {
        if (!argIn[k].isRowVectorCharacterArray()) {
            Error("Nelson:types:invalidClassName", "%s must be character vectors.",
                "Class name and parent class names");
        }
    }
    std::string className = argIn[1].asUtf8String();
    ArrayOf object = argIn[0];
    object.promoteType(NLS_CLASS_ARRAY, object.fieldNames());
    object.setClassType(className);
    retval << object;
    return retval;
}
//=============================================================================
