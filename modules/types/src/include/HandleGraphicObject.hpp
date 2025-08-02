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
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSTYPES_IMPEXP HandleGenericObject
{
private:
    std::wstring category;
    void* ptr;
    bool _isScoped;

public:
    HandleGenericObject(const std::wstring& _category, void* _ptr, bool isScoped);
    virtual ~HandleGenericObject() = default;
    ;
    std::wstring
    getCategory();
    void
    setPointer(void* _ptr);
    void*
    getPointer();
    bool
    isScoped();
    virtual bool
    isProperty(const std::wstring& propertyName)
    {
        return false;
    };
    virtual bool
    isMethod(const std::wstring& methodName)
    {
        return false;
    };
};
//=============================================================================
} // namespace Nelson
//=============================================================================
