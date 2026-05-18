//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include <stdexcept>
#include "ConstantPool.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
uint16_t
ConstantPool::addValue(const ConstantValue& value)
{
    if (entries_.size() > std::numeric_limits<uint16_t>::max()) {
        throw std::overflow_error("bytecode constant pool exceeds uint16_t range");
    }
    entries_.push_back(value);
    return static_cast<uint16_t>(entries_.size() - 1);
}
//=============================================================================
uint16_t
ConstantPool::addDouble(double v)
{
    auto found = doubleIdx_.find(v);
    if (found != doubleIdx_.end()) {
        return found->second;
    }
    uint16_t idx = addValue(ArrayOf::doubleConstructor(v));
    doubleIdx_[v] = idx;
    return idx;
}
//=============================================================================
uint16_t
ConstantPool::addSingle(float v)
{
    return addValue(ArrayOf::singleConstructor(v));
}
//=============================================================================
uint16_t
ConstantPool::addString(const std::string& s)
{
    return addValue(ArrayOf::characterArrayConstructor(s));
}
//=============================================================================
uint16_t
ConstantPool::addStringObj(const std::string& s)
{
    return addValue(ArrayOf::stringArrayConstructor(s));
}
//=============================================================================
uint16_t
ConstantPool::addArray(const ArrayOf& value)
{
    return addValue(value);
}
//=============================================================================
uint16_t
ConstantPool::addName(const std::string& name)
{
    auto found = nameIdx_.find(name);
    if (found != nameIdx_.end()) {
        return found->second;
    }
    uint16_t idx = addValue(name);
    nameIdx_[name] = idx;
    return idx;
}
//=============================================================================
uint16_t
ConstantPool::addFunction(FunctionDef* fdef)
{
    return addValue(fdef);
}
//=============================================================================
uint16_t
ConstantPool::addAnonymousChunk(BytecodeChunk* chunk)
{
    return addValue(chunk);
}
//=============================================================================
const ConstantValue&
ConstantPool::get(uint16_t idx) const
{
    if (idx >= entries_.size()) {
        throw std::out_of_range("bytecode constant pool index out of range");
    }
    return entries_[idx];
}
//=============================================================================
} // namespace Nelson
//=============================================================================
