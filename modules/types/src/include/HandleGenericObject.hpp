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
class Evaluator;
//=============================================================================
/**
 * Base wrapper for native objects stored in Nelson handle arrays.
 *
 * Derived classes provide category_-specific cleanup, property lookup, method
 * dispatch, and native pointer access. The handle registry owns instances
 * through HandleState unless a derived class changes that policy.
 */
class NLSTYPES_IMPEXP HandleGenericObject
{
    friend class ArrayOf;

private:
    std::string category_;
    void* ptr;
    /* Scoped handles are cleared when their owning execution scope is left. */
    bool _isScoped;
    /* Finalization state read by HandleState at the last-reference point.
       Both flags live in the tail padding after _isScoped so the object layout
       (and the ABI of every module deriving from this class) is unchanged. */
    /* The object owes a user-level destructor when its last reference goes
       away (see PendingHandleFinalizers). */
    bool _finalizerRequired = false;
    /* The destructor protocol already started: it never runs twice. */
    bool _finalizerStarted = false;

public:
    HandleGenericObject(const std::string& _category, void* _ptr, bool isScoped);

    virtual ~HandleGenericObject() = default;

    /**
     * Hook called when the handle is explicitly deleted.
     */
    virtual void
    onDelete(Evaluator* eval)
    {
    }

    /**
     * Hook called when the handle is cleared from the workspace.
     */
    virtual void
    onClear(Evaluator* eval)
    {
    }

    /**
     * Return whether HandleState may share ownership through HandleRef.
     */
    virtual bool
    useSharedHandleOwnership() const
    {
        return true;
    }

    /**
     * Return whether the object must outlive its last handle reference so
     * that an interpreter can run its user-level destructor at the next safe
     * point (see PendingHandleFinalizers).
     */
    bool
    wantsDeferredFinalization() const
    {
        return _finalizerRequired && !_finalizerStarted;
    }

    void
    setFinalizerRequired(bool required)
    {
        _finalizerRequired = required;
    }

    bool
    finalizerStarted() const
    {
        return _finalizerStarted;
    }

    /**
     * Record that the destructor protocol started (delete(obj) or the last
     * reference going away); later attempts are no-ops.
     */
    void
    markFinalizerStarted()
    {
        _finalizerStarted = true;
    }

    /**
     * Return whether invalidating the handle also destroys the wrapped object.
     */
    virtual bool
    destroyOnInvalidate() const
    {
        return true;
    }

    std::string
    categoryName() const
    {
        return category_;
    }

    virtual void*
    nativePointer()
    {
        return ptr;
    }

    /**
     * Return the class name exposed for method and property dispatch.
     */
    virtual std::string
    className()
    {
        return this->category_;
    }
    std::string
    category();
    void
    setPointer(void* _ptr);
    void*
    pointer();
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

    /**
     * Dispatch a method call implemented by the native object.
     */
    virtual bool
    invokeMethod(Interface* io, const ArrayOfVector& args, int nLhs, const std::string& methodName,
        ArrayOfVector& results)
    {
        return false;
    }

    /**
     * Read a property or property-like method implemented by the native object.
     */
    virtual bool
    invokeProperty(const std::string& methodName, ArrayOfVector& results)
    {
        return false;
    }

    /**
     * Return the list of exposed property names.
     */
    virtual wstringVector
    properties()
    {
        return {};
    }
    /**
     * Return the list of exposed method names.
     */
    virtual wstringVector
    methods()
    {
        return {};
    }
    /**
     * Return a stable key used by handle lookup helpers.
     */
    virtual size_t
    keyHash()
    {
        return reinterpret_cast<size_t>(pointer());
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
