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
#include <unordered_map>
#include <vector>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP ClassdefHandleObject : public HandleGenericObject
{
private:
    std::unordered_map<std::string, ArrayOf> _properties;
    std::vector<nelson_handle> _listenerHandles;

public:
    explicit ClassdefHandleObject(const std::string& className);

    bool
    isProperty(const std::wstring& propertyName) override;

    bool
    isMethod(const std::wstring& methodName) override;

    bool
    invokeProperty(const std::string& propertyName, ArrayOfVector& results) override;

    wstringVector
    getProperties() override;

    wstringVector
    getMethods() override;

    bool
    hasProperty(const std::string& propertyName);

    bool
    hasStoredProperty(const std::string& propertyName) const;

    ArrayOf
    getProperty(const std::string& propertyName);

    void
    setProperty(const std::string& propertyName, const ArrayOf& value);

    bool
    hasEvent(const std::string& eventName);

    void
    addListener(nelson_handle listenerHandle);

    void
    removeListener(nelson_handle listenerHandle);

    void
    notifyEvent(Evaluator* eval, nelson_handle sourceHandle, const std::string& eventName,
        const ArrayOf& eventData);

    void
    notifyPropertyEvent(Evaluator* eval, nelson_handle sourceHandle,
        const std::string& propertyName, const std::string& eventName);

    void
    deleteListeners();

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
};
//=============================================================================
} // namespace Nelson
//=============================================================================
