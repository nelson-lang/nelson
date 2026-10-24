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
#include <string>
#include <unordered_set>
//=============================================================================
namespace Nelson {
//=============================================================================
// Builtin (non-classdef) types supporting the static empty-array constructor
// `TypeName.empty` / `TypeName.empty(sz)`, backed by the `__empty__` builtin.
inline bool
isBuiltinEmptyTypeName(const std::string& name)
{
    static const std::unordered_set<std::string> builtinTypeNames = { "double", "single", "int8",
        "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "logical", "char",
        "string", "cell", "struct", "missing", "function_handle" };
    return builtinTypeNames.find(name) != builtinTypeNames.end();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
