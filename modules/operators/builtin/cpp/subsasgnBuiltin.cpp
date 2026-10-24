//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "subsasgnBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::subsasgnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 1, 1);

    if (!argIn[1].isStruct()) {
        Error("Nelson:operators:invalidSubsStruct",
            "Second argument must be a structure with two fields whose names are 'type' and "
            "'subs'.");
    }
    stringVector fieldnames = argIn[1].fieldNames();
    bool isSupportedStruct = (fieldnames.size() == 2)
        && ((fieldnames[0] == "type" && fieldnames[1] == "subs")
            || (fieldnames[0] == "subs" && fieldnames[1] == "type"));
    if (!isSupportedStruct) {
        Error("Nelson:operators:invalidSubsStruct",
            "Second argument must be a structure with two fields whose names are 'type' and "
            "'subs'.");
    }

    ArrayOfVector typeFields = argIn[1].fieldAsList("type");
    ArrayOfVector subsFields = argIn[1].fieldAsList("subs");

    ArrayOf A(argIn[0]);
    ArrayOf B = argIn[2];

    if (typeFields.size() > 1) {
        Error("Nelson:operators:indexChainingNotSupported", "No support for index chaining.");
    }

    // builtin('subsasgn', obj, S, val) on a classdef/handle object with a '.' step
    // must do the default property assignment, not re-enter the object's overloaded
    // subsasgn (infinite recursion when that subsasgn falls back to
    // builtin('subsasgn', ...)). Same suppression as on the subsref read side.
    if (eval != nullptr && (A.isClassType() || A.isHandle()) && typeFields.size() == 1
        && typeFields[0].asUtf8String() == ".") {
        stringVector subtypes;
        ArrayOfVector subsindices;
        subtypes.push_back(".");
        subsindices.push_back(subsFields[0]);
        bool haveFunction = false;
        bool savedWithOverload = eval->withOverload;
        eval->withOverload = false;
        ArrayOf assigned;
        try {
            assigned = eval->bytecodeAssignClass(A, subtypes, subsindices, B, haveFunction);
        } catch (const NelsonException&) {
            eval->withOverload = savedWithOverload;
            throw;
        }
        eval->withOverload = savedWithOverload;
        if (haveFunction) {
            retval.push_back(assigned);
            return retval;
        }
        // no default property assignment applied (e.g. no such property): fall
        // through so the generic handling below raises a consistent error.
    }

    for (indexType k = 0; k < typeFields.size(); k++) {
        std::string typeStr = typeFields[k].asUtf8String();
        ArrayOf subsField = subsFields[k];

        std::string name;
        ArrayOf index;
        bool isIndex = false;
        if ((subsField.isStringArray() && subsField.isScalar())
            || subsField.isRowVectorCharacterArray()) {
            name = subsField.asUtf8String();
            isIndex = false;
        } else if (subsField.isCell() && subsField.isVector()) {
            isIndex = true;
            index = subsField;
        } else {
            Error("Nelson:operators:invalidSubsStruct:subsFieldACellArrayString",
                "'subs' field must be a cell array or string.");
        }

        ArrayOfVector indexAsVector;

        if (isIndex) {
            ArrayOf* elements = (ArrayOf*)index.dataPointer();
            for (indexType q = 0; q < index.elementCount(); q++) {
                indexAsVector.push_back(elements[q]);
            }
        }
        if (typeStr == "()") {
            if (indexAsVector.size() == 1) {
                A.setVectorSubset(indexAsVector[0], B);
            } else {
                A.setNDimSubset(indexAsVector, B);
            }
        } else if (typeStr == ".") {
            if (!A.isStruct()) {
                Error("Nelson:operators:invalidSubsStruct:firstArgumentAStruct",
                    "First argument must be a struct.");
            }
            A.setField(name, B);
        } else if ((typeStr == "{}")) {
            if (indexAsVector.size() == 1) {
                A.setVectorContents(indexAsVector[0], B);
            } else {
                A.setNDimContents(indexAsVector, B);
            }
        } else {
            Error("Nelson:operators:invalidSubsType", "%s expected.",
                "Illegal indexing structure argument: type '.', '{}' or '()'");
        }
    }
    retval.push_back(A);
    return retval;
}
//=============================================================================
