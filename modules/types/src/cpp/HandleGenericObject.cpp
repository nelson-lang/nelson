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
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
HandleGenericObject::HandleGenericObject(const std::string& _category, void* _ptr, bool isScoped)
{
    if (_category.empty()) {
        Error(L"Nelson:types:handleExpected", L"handle must have a %s.", L"type");
    }
    if (_ptr == nullptr) {
        Error(L"Nelson:types:handleExpected", L"handle must have a %s.", L"pointer");
    }
    this->category_ = _category;
    this->ptr = _ptr;
    this->_isScoped = isScoped;
}
//=============================================================================
std::string
HandleGenericObject::category()
{
    return this->category_;
}
//=============================================================================
void
HandleGenericObject::setPointer(void* _ptr)
{
    this->ptr = _ptr;
}
//=============================================================================
void*
HandleGenericObject::pointer()
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
