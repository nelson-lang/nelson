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
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include "Types.hpp"
#include "nlsTypes_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSTYPES_IMPEXP HandleGenericObject
{
    friend class ArrayOf;

private:
    std::string category;
    void* ptr;
    bool _isScoped;

public:
    HandleGenericObject(const std::string& _category, void* _ptr, bool isScoped);

    virtual ~HandleGenericObject() = default;

    virtual std::string
    getClassName()
    {
        return this->category;
    }
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

    virtual bool
    invokeMethod(Interface* io, const ArrayOfVector& args, int nLhs, const std::string& methodName,
        ArrayOfVector& results)
    {
        return false;
    }

    virtual bool
    invokeProperty(const std::string& methodName, ArrayOfVector& results)
    {
        return false;
    }

    virtual wstringVector
    getProperties()
    {
        return {};
    }
    virtual wstringVector
    getMethods()
    {
        return {};
    }
    virtual size_t
    keyHash()
    {
        return reinterpret_cast<size_t>(getPointer());
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
