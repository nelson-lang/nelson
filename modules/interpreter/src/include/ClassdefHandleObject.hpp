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
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct NLSINTERPRETER_IMPEXP ClassdefDynamicPropertyAttributes
{
    bool abortSet = false;
    bool dependent = false;
    std::string getAccess = "public";
    bool getObservable = false;
    ArrayOf getMethod;
    bool hidden = false;
    bool nonCopyable = false;
    bool setObservable = false;
    std::string setAccess = "public";
    ArrayOf setMethod;
    bool transient = false;
    std::string validationExpression;
};
//=============================================================================
// Value of a WeakHandle property: the targets are not kept alive by the owner.
struct NLSINTERPRETER_IMPEXP ClassdefWeakHandleSlot
{
    Dimensions dims;
    std::string className;
    std::vector<std::weak_ptr<HandleState>> targets;
};
//=============================================================================
class NLSINTERPRETER_IMPEXP ClassdefHandleObject : public HandleGenericObject
{
private:
    std::unordered_map<std::string, ArrayOf> _properties;
    std::unordered_map<std::string, ClassdefWeakHandleSlot> _weakHandleProperties;
    // the class declares at least one WeakHandle property
    bool _hasWeakHandleProperties = false;
    std::vector<std::string> _dynamicProperties;
    std::unordered_map<std::string, ClassdefDynamicPropertyAttributes> _dynamicPropertyAttributes;
    std::vector<nelson_handle> _listenerHandles;
    // Strong references for listeners created with addlistener: their lifetime
    // is bound to this source object, not to the value returned to the caller.
    std::vector<HandleRef> _boundListenerRefs;

    bool
    isWeakHandleProperty(const std::string& propertyName);

    void
    storeWeakHandleProperty(const std::string& propertyName, const ArrayOf& value);

public:
    explicit ClassdefHandleObject(const std::string& className);

    // The destructor protocol (ObjectBeingDestroyed + user delete chain)
    // started, whether triggered by delete(obj) or by the last reference going
    // away: it never runs twice and isvalid reports false from then on.
    bool
    deleteInvoked() const;

    void
    markDeleteInvoked();

    void
    reserveStoredProperties(size_t count);

    bool
    isProperty(const std::wstring& propertyName) override;

    bool
    isMethod(const std::wstring& methodName) override;

    bool
    invokeProperty(const std::string& propertyName, ArrayOfVector& results) override;

    wstringVector
    properties() override;

    wstringVector
    methods() override;

    bool
    hasProperty(const std::string& propertyName);

    bool
    hasDynamicProperty(const std::string& propertyName) const;

    void
    addDynamicProperty(const std::string& propertyName);

    bool
    removeDynamicProperty(const std::string& propertyName);

    stringVector
    dynamicProperties() const;

    ClassdefDynamicPropertyAttributes
    dynamicPropertyAttributes(const std::string& propertyName) const;

    bool
    setDynamicPropertyHidden(const std::string& propertyName, bool value);

    bool
    setDynamicPropertyNonCopyable(const std::string& propertyName, bool value);

    bool
    setDynamicPropertyAbortSet(const std::string& propertyName, bool value);

    bool
    setDynamicPropertyDependent(const std::string& propertyName, bool value);

    bool
    setDynamicPropertyGetObservable(const std::string& propertyName, bool value);

    bool
    setDynamicPropertyGetAccess(const std::string& propertyName, const std::string& value);

    bool
    setDynamicPropertyGetMethod(const std::string& propertyName, const ArrayOf& value);

    bool
    setDynamicPropertySetObservable(const std::string& propertyName, bool value);

    bool
    setDynamicPropertySetAccess(const std::string& propertyName, const std::string& value);

    bool
    setDynamicPropertySetMethod(const std::string& propertyName, const ArrayOf& value);

    bool
    setDynamicPropertyTransient(const std::string& propertyName, bool value);

    bool
    setDynamicPropertyValidationExpression(
        const std::string& propertyName, const std::string& value);

    bool
    isDynamicPropertyObservableEvent(
        const std::string& propertyName, const std::string& eventName) const;

    bool
    hasStoredProperty(const std::string& propertyName) const;

    ArrayOf
    getProperty(const std::string& propertyName);

    void
    setProperty(const std::string& propertyName, const ArrayOf& value);

    void
    setStoredProperty(const std::string& propertyName, const ArrayOf& value);

    void
    copyStoredPropertiesFrom(const ClassdefHandleObject& source, bool copyNonCopyable = true);

    bool
    hasEvent(const std::string& eventName);

    void
    addListener(nelson_handle listenerHandle, bool bindToSource = false);

    void
    removeListener(nelson_handle listenerHandle);

    void
    notifyEvent(Evaluator* eval, nelson_handle sourceHandle, const std::string& eventName,
        const ArrayOf& eventData);

    void
    notifyDynamicPropertyEvent(Evaluator* eval, nelson_handle sourceHandle,
        const std::string& propertyName, const std::string& eventName);

    void
    notifyPropertyEvent(Evaluator* eval, nelson_handle sourceHandle,
        const std::string& propertyName, const std::string& eventName);

    void
    deleteListeners();

    void
    onDelete(Evaluator* eval) override;

    ArrayOf
    toStruct();
};
//=============================================================================
class NLSINTERPRETER_IMPEXP ClassdefListenerObject : public HandleGenericObject
{
private:
    nelson_handle _sourceHandle = 0;
    std::string _eventName;
    ArrayOf _callback;
    bool _enabled = true;

public:
    ClassdefListenerObject(
        nelson_handle sourceHandle, const std::string& eventName, const ArrayOf& callback);

    nelson_handle
    sourceHandle() const;

    const std::string&
    eventName() const;

    const ArrayOf&
    callback() const;

    bool
    enabled() const;

    bool
    matches(nelson_handle sourceHandle, const std::string& eventName) const;

    void
    detachFromSource();

    void
    onDelete(Evaluator* eval) override;
};
//=============================================================================
class NLSINTERPRETER_IMPEXP ClassdefDynamicPropertyObject : public HandleGenericObject
{
private:
    nelson_handle _sourceHandle = 0;
    std::string _definingClassName;
    std::string _propertyName;

public:
    ClassdefDynamicPropertyObject(nelson_handle sourceHandle, const std::string& definingClassName,
        const std::string& propertyName);

    const std::string&
    propertyName() const;

    bool
    isProperty(const std::wstring& propertyName) override;

    bool
    invokeProperty(const std::string& propertyName, ArrayOfVector& results) override;

    bool
    setProperty(const std::string& propertyName, const ArrayOf& value);

    wstringVector
    properties() override;

    bool
    removeFromSource();

    bool
    removeFromSource(Evaluator* eval);

    void
    onDelete(Evaluator* eval) override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
