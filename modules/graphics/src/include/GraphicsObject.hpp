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
#include <unordered_map>
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
#include "GOGenericProperty.hpp"
#include "RenderInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOAxis;
class GOFigure;
//=============================================================================
class NLSGRAPHICS_IMPEXP GraphicsObject
{
private:
    std::unordered_map<std::wstring, bool> m_properties_writable;
    std::unordered_map<std::wstring, bool> m_properties_visible;
    std::unordered_map<std::wstring, GOGenericProperty*> m_properties;
    wstringVector m_property_names_order;
    wstringVector m_visible_property_names_order;
    unsigned ref_count;

public:
    GraphicsObject();
    virtual ~GraphicsObject();
    void
    reference()
    {
        ref_count++;
    }
    void
    dereference()
    {
        ref_count--;
    }
    unsigned
    referenceCount()
    {
        return ref_count;
    }
    virtual void
    registerProperties() {};
    virtual void
    updateState() {};
    virtual std::vector<double>
    getLimits()
    {
        return {};
    };

    virtual std::wstring
    getType()
    {
        return L"Graphics_Object";
    }

    bool
    hasChanged(const std::vector<std::wstring>& names);
    bool
    hasChanged(const std::wstring& name);
    void
    toManual(const std::wstring& name);
    void
    toAuto(const std::wstring& name);
    bool
    isType(const std::wstring& name);
    void
    clearChanged(const std::vector<std::wstring>& names);
    void
    clearChanged(const std::wstring& name);
    void
    clearAllChanged();
    void
    registerProperty(GOGenericProperty* prop, const std::wstring& name, bool writable = true,
        bool visible = true);
    void
    sortProperties();
    GOGenericProperty*
    findProperty(const std::wstring& name, bool raiseError = true);
    bool
    isWritable(const std::wstring& name);
    bool
    isVisible(const std::wstring& name);
    wstringVector
    getFieldnames();
    wstringVector
    getVisibleFieldnames();
    bool
    haveProperty(const std::wstring& name);
    double
    findScalarDoubleProperty(const std::wstring& name);
    unsigned
    findGoProperty(const std::wstring& name);
    std::vector<double>
    findVectorDoubleProperty(const std::wstring& name);
    ArrayOf
    findArrayOfProperty(const std::wstring& name);
    std::wstring
    findStringProperty(const std::wstring& name);
    wstringVector
    findStringVectorProperty(const std::wstring& name);
    bool
    stringCheck(const std::wstring& name, const std::wstring& value);
    void
    setRestrictedStringDefault(const std::wstring& name, const std::wstring& value);
    void
    setRestrictedStringSetDefault(const std::wstring& name, const wstringVector& values);
    void
    setRestrictedStringScalarDefault(
        const std::wstring& name, const std::wstring& value, double scalar);
    void
    setRestrictedStringColorDefault(
        const std::wstring& name, const std::wstring& value, double r, double g, double b);
    void
    setVectorDoubleDefault(const std::wstring& name, std::vector<double> defaultValues);
    void
    setTwoVectorDefault(const std::wstring& name, double x, double y);
    void
    setThreeVectorDefault(const std::wstring& name, double x, double y, double z);
    void
    setFourVectorDefault(const std::wstring& name, double x, double y, double z, double w);
    void
    setStringDefault(const std::wstring& name, const std::wstring& value);
    void
    setScalarDoubleDefault(const std::wstring& name, double value);
    void
    setScalarPositiveIntegerValueDefault(const std::wstring& name, double value);
    void
    setGoProperty(const std::wstring& name, int64 value);
    bool
    isAuto(const std::wstring& mode);
    virtual void
    paintMe(RenderInterface& gc)
        = 0;
    GOAxis*
    getParentAxis();
    GOFigure*
    getParentFigure();
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
