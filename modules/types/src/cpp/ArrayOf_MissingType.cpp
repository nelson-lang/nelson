//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isMissing() const
{
    if (dp) {
        return (dp->dataClass == NLS_MISSING_ARRAY);
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isNdArrayMissing() const
{
    if (dp) {
        return (dp->dataClass == NLS_MISSING_ARRAY) && !is2D();
    }
    return false;
}
//=============================================================================
ArrayOf
ArrayOf::missingScalar()
{
    Dimensions dim;
    dim.makeScalar();
    double val = std::nan("");
    return ArrayOf(NLS_MISSING_ARRAY, dim, static_cast<const void*>(&val), sizeof(double));
}
//=============================================================================
void
ArrayOf::convertMissingAssignmentValue(ArrayOf& rightData) const
{
    const bool isDestinationMissing = (dataClass() == NLS_MISSING_ARRAY);
    if (rightData.isMissing() == isDestinationMissing) {
        return;
    }
    if (isDestinationMissing) {
        Error("Nelson:types:assignmentNotConvertible",
            "Unable to perform assignment because value of type '%s' is not convertible to "
            "'%s'.",
            ClassName(rightData), std::string("missing"));
    }
    switch (dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        rightData.promoteType(NLS_DOUBLE);
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        rightData.promoteType(NLS_SINGLE);
    } break;
    case NLS_STRING_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE: {
    } break;
    case NLS_STRUCT_ARRAY: {
        Error(L"Nelson:types:unlikeTypesAssignment",
            L"Assignment between unlike types is not allowed.");
    } break;
    default: {
        Error("Nelson:types:assignmentNotConvertible",
            "Unable to perform assignment because value of type '%s' is not convertible to "
            "'%s'.",
            std::string("missing"), ClassName(*this));
    } break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
