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
#include <vector>
#include "ArrayOf.hpp"
#include "PositionScript.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// The stack field of an error: one {file, name, line} element per frame shown to users.
inline ArrayOf
StackTraceToStruct(const std::vector<PositionScript>& trace)
{
    stringVector traceFieldnames = { "file", "name", "line" };
    ArrayOfVector files;
    ArrayOfVector names;
    ArrayOfVector lines;
    for (const auto& position : trace) {
        if (!position.isShownInStacks()) {
            continue;
        }
        files.push_back(ArrayOf::characterArray(position.getFilename()));
        names.push_back(ArrayOf::characterArray(position.getFunctionName()));
        lines.push_back(ArrayOf::doubleScalar(position.getLine()));
    }
    Dimensions dims(files.size(), 1);
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), traceFieldnames, false));
    ArrayOf stack = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, traceFieldnames);
    if (!files.empty()) {
        stack.setFieldAsList(traceFieldnames[0], files);
        stack.setFieldAsList(traceFieldnames[1], names);
        stack.setFieldAsList(traceFieldnames[2], lines);
    }
    return stack;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
