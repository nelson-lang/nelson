//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FevalFutureObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FevalFutureObject::FevalFutureObject(
    std::future<std::tuple<ArrayOfVector, Exception>> f, const std::wstring& functionName)
    : HandleGenericObject(std::wstring(FEVALFUTURE_CATEGORY_STR), this, false)
{
    propertiesNames = { L"ID", L"Function", L"Error" };
    this->future = std::move(f);
    this->functionName = functionName;
    wasReaded = false;
}
//=============================================================================
void
FevalFutureObject::display(Interface* io)
{
#define BLANKS_AT_BOL std::wstring(L"   ")

    if (io) { }
}
//=============================================================================
FevalFutureObject::~FevalFutureObject() { }
//=============================================================================
std::tuple<ArrayOfVector, Exception>
FevalFutureObject::get(bool& valid)
{
    if (wasReaded) {
        valid = true;
        return content;
    }

    if (future.valid()) {
        content = future.get();
        wasReaded = true;
        valid = true;
        return content;
    }
    valid = false;
    return std::make_tuple<ArrayOfVector, Exception>(ArrayOfVector(), Exception());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
