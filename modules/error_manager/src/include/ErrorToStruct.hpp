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

    ArrayOf stack;
    const std::vector<PositionScript>& trace = e.trace();
    stringVector traceFieldnames(3);
    traceFieldnames[0] = "file";
    traceFieldnames[1] = "name";
    traceFieldnames[2] = "line";
    Dimensions dims(trace.size(), 1);
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), traceFieldnames, false));
    stack = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, traceFieldnames);
    ArrayOfVector files;
    ArrayOfVector names;
    ArrayOfVector lines;
    files.reserve(trace.size());
    names.reserve(trace.size());
    lines.reserve(trace.size());
    for (const auto& k : trace) {
        files.push_back(ArrayOf::characterArray(k.getFilename()));
        names.push_back(ArrayOf::characterArray(k.getFunctionName()));
        lines.push_back(ArrayOf::doubleScalar(k.getLine()));
    }
    if (!trace.empty()) {
        stack.setFieldAsList(traceFieldnames[0], files);
        stack.setFieldAsList(traceFieldnames[1], names);
        stack.setFieldAsList(traceFieldnames[2], lines);
    }
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
