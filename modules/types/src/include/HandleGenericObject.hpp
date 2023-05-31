//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSTYPES_IMPEXP HandleGenericObject
{
private:
    std::string category;
    void* ptr;
    bool _isScoped;

public:
    HandleGenericObject(const std::string& _category, void* _ptr, bool isScoped);
    virtual ~HandleGenericObject() = default;
    ;
    std::string
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
