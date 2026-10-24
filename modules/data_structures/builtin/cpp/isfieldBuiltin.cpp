//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isfieldBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ArrayOfView.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
hasField(const stringVector& fieldnames, const std::string& name);
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::isfieldBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    // class, classdef or handle objects without an 'isfield' overload are not struct:
    // returns false (a class-defined overload is dispatched before reaching this builtin)
    if (!param1.isStruct()) {
        retval << ArrayOf::logicalScalar(false);
        return retval;
    }
    stringVector fieldnames = param1.fieldNames();
    if (param2.isRowVectorCharacterArray()) {
        retval << ArrayOf::logicalScalar(hasField(fieldnames, param2.asUtf8String()));
        return retval;
    }
    if (param2.isCell() || param2.isStringArray()) {
        // one answer per element, in the shape of the queried names
        Dimensions dims2 = param2.dimensions();
        if (param2.isCell() && dims2.elementCount() == 0) {
            retval << ArrayOf::logicalScalar(false);
            return retval;
        }
        ArrayOfView<ArrayOf> elements(param2, ArrayOfViewAssumeValidated {});
        logical* res = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dims2.elementCount(), stringVector(), false));
        ompIndexType elementCount = dims2.elementCount();
        for (ompIndexType k = 0; k < elementCount; ++k) {
            res[k] = elements[k].isRowVectorCharacterArray()
                && hasField(fieldnames, elements[k].asUtf8String());
        }
        retval << ArrayOf(NLS_LOGICAL, dims2, res);
        return retval;
    }
    retval << ArrayOf::logicalScalar(false);
    return retval;
}
//=============================================================================
static bool
hasField(const stringVector& fieldnames, const std::string& name)
{
    for (const auto& fieldname : fieldnames) {
        if (fieldname == name) {
            return true;
        }
    }
    return false;
}
//=============================================================================
