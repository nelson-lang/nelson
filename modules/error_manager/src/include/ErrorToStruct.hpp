//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "NelsonException.hpp"
#include "StackTraceToStruct.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline ArrayOf
ErrorToStruct(const NelsonException& e)
{
    Dimensions dimsRes(1, 1);
    stringVector fieldnames;
    fieldnames.reserve(3);
    fieldnames.push_back("message");
    fieldnames.push_back("identifier");
    fieldnames.push_back("stack");
    ArrayOf* elementRes = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dimsRes.elementCount(), fieldnames, false));
    ArrayOf res = ArrayOf(NLS_STRUCT_ARRAY, dimsRes, elementRes, false, fieldnames);

    ArrayOf stack = StackTraceToStruct(e.trace());
    ArrayOfVector messageVector(ArrayOf::characterArray(e.message()));
    res.setFieldAsList(fieldnames[0], messageVector);
    ArrayOfVector identifierVector(ArrayOf::characterArray(e.identifier()));
    res.setFieldAsList(fieldnames[1], identifierVector);
    ArrayOfVector stackVector(stack);
    res.setFieldAsList(fieldnames[2], stackVector);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
