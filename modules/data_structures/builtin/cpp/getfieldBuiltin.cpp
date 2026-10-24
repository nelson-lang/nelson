//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getfieldBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
indexStep(ArrayOf& value, const ArrayOf& subscripts);
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::getfieldBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2);
    ArrayOf param1 = argIn[0];
    if (param1.isClassType() || param1.isHandle()) {
        OverloadRequired("getfield");
    }
    // getfield(S, 'field1', ..., 'fieldN') follows the chain of field names
    // S.field1. ... .fieldN; a cell argument holds the subscripts of an
    // indexing step: getfield(S, {i, j}, 'field', {k}) is S(i, j).field(k).
    ArrayOf current = param1;
    for (size_t k = 1; k < argIn.size(); ++k) {
        if (argIn[k].isCell()) {
            current = indexStep(current, argIn[k]);
            continue;
        }
        if (!current.isStruct()) {
            Error(L"Nelson:data_structures:wrongTypeArgument1Struct",
                L"Wrong type for argument #%d: %s expected.", 1, L"struct");
        }
        std::wstring fieldname = argIn[k].asWideString();
        if (current.isScalar()) {
            current = current.field(wstring_to_utf8(fieldname));
        } else {
            ArrayOfVector rv = current.fieldAsList(wstring_to_utf8(fieldname));
            if (rv.size() == 0) {
                Error(L"Nelson:data_structures:wrongTypeArgument1Struct",
                    L"Wrong type for argument #%d: %s expected.", 1, L"struct");
            }
            current = rv[0];
        }
    }
    retval << current;
    return retval;
}
//=============================================================================
// value(subscripts{:}); an empty cell leaves the value unchanged.
static ArrayOf
indexStep(ArrayOf& value, const ArrayOf& subscripts)
{
    ArrayOfVector indices;
    const auto* elements = static_cast<const ArrayOf*>(subscripts.dataPointer());
    for (indexType k = 0; k < subscripts.elementCount(); ++k) {
        indices.push_back(elements[k]);
    }
    if (indices.empty()) {
        return value;
    }
    if (indices.size() == 1) {
        return value.vectorSubset(indices[0]);
    }
    return value.nDimSubset(indices);
}
//=============================================================================
