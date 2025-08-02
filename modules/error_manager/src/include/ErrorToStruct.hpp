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
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline ArrayOf
ErrorToStruct(const Exception& e)
{
    Dimensions dimsRes(1, 1);
    stringVector fieldnames;
    fieldnames.reserve(3);
    fieldnames.push_back("message");
    fieldnames.push_back("identifier");
    fieldnames.push_back("stack");
    ArrayOf* elementRes = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dimsRes.getElementCount(), fieldnames, false));
    ArrayOf res = ArrayOf(NLS_STRUCT_ARRAY, dimsRes, elementRes, false, fieldnames);

    ArrayOf stack;
    std::vector<PositionScript> trace = e.getTrace();
    stringVector traceFieldnames(3);
    traceFieldnames[0] = "file";
    traceFieldnames[1] = "name";
    traceFieldnames[2] = "line";
    Dimensions dims(trace.size(), 1);
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), traceFieldnames, false));
    stack = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, traceFieldnames);
    ArrayOfVector files(trace.size());
    ArrayOfVector names(trace.size());
    ArrayOfVector lines(trace.size());
    for (auto& k : trace) {
        files.push_back(ArrayOf::characterArrayConstructor(k.getFilename()));
        names.push_back(ArrayOf::characterArrayConstructor(k.getFunctionName()));
        lines.push_back(ArrayOf::doubleConstructor(k.getLine()));
    }
    if (!trace.empty()) {
        stack.setFieldAsList(traceFieldnames[0], files);
        stack.setFieldAsList(traceFieldnames[1], names);
        stack.setFieldAsList(traceFieldnames[2], lines);
    }
    ArrayOfVector messageVector(ArrayOf::characterArrayConstructor(e.getMessage()));
    res.setFieldAsList(fieldnames[0], messageVector);
    ArrayOfVector identifierVector(ArrayOf::characterArrayConstructor(e.getIdentifier()));
    res.setFieldAsList(fieldnames[1], identifierVector);
    ArrayOfVector stackVector(stack);
    res.setFieldAsList(fieldnames[2], stackVector);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
