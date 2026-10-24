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
#include <cstdint>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>
#include "ArrayOf.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class BytecodeChunk;
class FunctionDef;
//=============================================================================
using ConstantValue = std::variant<ArrayOf, std::string, FunctionDef*, BytecodeChunk*>;
//=============================================================================
class NLSINTERPRETER_IMPEXP ConstantPool
{
public:
    uint16_t
    addDouble(double v);
    uint16_t
    addSingle(float v);
    uint16_t
    addString(const std::string& s);
    uint16_t
    addStringObj(const std::string& s);
    uint16_t
    addArray(const ArrayOf& value);
    uint16_t
    addName(const std::string& name);
    uint16_t
    addFunction(FunctionDef* fdef);
    uint16_t
    addAnonymousChunk(BytecodeChunk* chunk);

    const ConstantValue&
    get(uint16_t idx) const;

    size_t
    size() const
    {
        return entries_.size();
    }

private:
    uint16_t
    addValue(const ConstantValue& value);

    std::vector<ConstantValue> entries_;
    std::unordered_map<double, uint16_t> doubleIdx_;
    std::unordered_map<std::string, uint16_t> nameIdx_;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
