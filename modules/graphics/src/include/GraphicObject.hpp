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
#include <string>
#include <map>
#include "nlsGraphics_exports.h"
#include "GOProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GraphicObject
{
    //=============================================================================
    struct MapComparator
    {
        bool
        operator()(const std::string& s1, const std::string& s2) const
        {
            std::string str1(s1.length(), ' ');
            std::string str2(s2.length(), ' ');
            std::transform(s1.begin(), s1.end(), str1.begin(), tolower);
            std::transform(s2.begin(), s2.end(), str2.begin(), tolower);
            return str1 < str2;
        }
    };
    //=============================================================================
    std::map<std::string, GOProperty*, MapComparator> properties;
    unsigned refCount;
    std::string goType = "";

public:
    GraphicObject();
    virtual ~GraphicObject();
    void
    reference();
    void
    dereference();
    unsigned
    referenceCount();
    virtual void
    registerProperties() {};
    virtual void
    refreshProperties() {};
    void
    setType(const std::string& objectType);
    [[nodiscard]] bool
    isType(const std::string& name) const;
    std::string
    getType();
    void
    addProperty(const std::string& propertyName, GOProperty* goPropertyPtr);
    GOProperty*
    searchProperty(const std::string& name);
    std::wstring
    displayProperties();
    stringVector
    getPropertiesName();
    void
    setPropertyAsStringValue(const std::string& propertyName, const std::string& value);
    void
    setPropertyAsScalarDoubleValue(const std::string& propertyName, double value);
    void
    setPropertyAsVector2DValue(const std::string& propertyName, double x1, double x2);
    void
    setPropertyAsVector3DValue(const std::string& propertyName, double x1, double x2, double x3);
    void
    setPropertyAsVector4DValue(
        const std::string& propertyName, double x1, double x2, double x3, double x4);
    void
    setPropertyAsScalarLogicalValue(const std::string& propertyName, bool value);
    void
    setPropertyAsOnOffSwitchValue(const std::string& propertyName, const std::string& value);
    void
    setPropertyAsArrayOfValue(const std::string& propertyName, ArrayOf value);
    void
    setPropertyAsColorValue(const std::string& propertyName, double R, double G, double B);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
