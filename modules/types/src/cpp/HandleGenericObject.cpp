//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HandleGenericObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
HandleGenericObject::HandleGenericObject(const std::string& _category, void* _ptr, bool isScoped)
{
    if (_category.empty()) {
        Error(_W("handle must have a type."));
    }
    if (_ptr == nullptr) {
        Error(_W("handle must have a pointer."));
    }
    this->category = _category;
    this->ptr = _ptr;
    this->_isScoped = isScoped;
}
//=============================================================================
std::string
HandleGenericObject::getCategory()
{
    return this->category;
}
//=============================================================================
void
HandleGenericObject::setPointer(void* _ptr)
{
    this->ptr = _ptr;
}
//=============================================================================
void*
HandleGenericObject::getPointer()
{
    return this->ptr;
}
//=============================================================================
bool
HandleGenericObject::isScoped()
{
    return this->_isScoped;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
