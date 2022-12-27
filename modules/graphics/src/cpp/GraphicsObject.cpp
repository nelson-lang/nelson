//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicsObject.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAxis.hpp"
#include "GOList.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GORestrictedStringColorProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorThreeDoubleProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GORestrictedStringScalarProperty.hpp"
#include "GORestrictedStringVectorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GraphicsObject::GraphicsObject() { ref_count = 1; }
//=============================================================================
GraphicsObject::~GraphicsObject()
{
    GOGObjectsProperty* hp = (GOGObjectsProperty*)findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    GraphicsObject* gp;
    std::vector<int64> my_children(hp->data());
    for (int i = 0; i < my_children.size(); i++) {
        int64 handle = my_children[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            gp = findGraphicsObject(handle);
            gp->dereference();
            if (gp->referenceCount() <= 0) {
                freeGraphicsObject(handle);
                delete gp;
            }
        }
    }
}
//=============================================================================
bool
GraphicsObject::isWritable(const std::wstring& name)
{
    std::wstring lower(name);
    std::transform(lower.begin(), lower.end(), lower.begin(), tolower);
    if (m_properties_writable.count(lower) == 0) {
        return false;
    }
    return m_properties_writable[lower];
}
//=============================================================================
GOGenericProperty*
GraphicsObject::findProperty(const std::wstring& name, bool raiseError)
{
    std::wstring asLowerName(name);
    std::transform(asLowerName.begin(), asLowerName.end(), asLowerName.begin(), tolower);
    GOGenericProperty** hp = m_properties.findSymbol(asLowerName);
    if (hp) {
        return (*hp);
    } else {
        if (raiseError) {
            Error(_W("Invalid property:") + L" " + name);
        }
        hp = nullptr;
    }
    return nullptr;
}
//=============================================================================
void
GraphicsObject::setGoProperty(const std::wstring& name, int64 value)
{
    GOGObjectsProperty* hp = (GOGObjectsProperty*)findProperty(name);
    if (hp) {
        std::vector<int64> newval;
        newval.push_back(value);
        hp->data(newval);
    }
}
//=============================================================================
GOAxis*
GraphicsObject::getParentAxis()
{
    GOGObjectsProperty* parent = (GOGObjectsProperty*)findProperty(GO_PARENT_PROPERTY_NAME_STR);
    if (parent->data().empty()) {
        return nullptr;
    }
    unsigned parent_handle = parent->data()[0];
    GraphicsObject* fp = findGraphicsObject(parent_handle);
    GOStringProperty* name = (GOStringProperty*)fp->findProperty(GO_TYPE_PROPERTY_NAME_STR);
    if (!name) {
        return nullptr;
    }
    if (!name->isEqual(GO_PROPERTY_VALUE_AXES_STR)) {
        return nullptr;
    }
    return (GOAxis*)fp;
}
//=============================================================================
GOFigure*
GraphicsObject::getParentFigure()
{
    GOAxis* hp;
    if (stringCheck(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AXES_STR)) {
        hp = (GOAxis*)this;
    } else {
        hp = getParentAxis();
    }
    GOGObjectsProperty* parent = (GOGObjectsProperty*)hp->findProperty(GO_PARENT_PROPERTY_NAME_STR);
    if (parent->data().empty()) {
        return nullptr;
    }
    unsigned parent_handle = parent->data()[0];
    return findGOFigure(parent_handle);
}
//=============================================================================
std::wstring
GraphicsObject::findStringProperty(const std::wstring& name)
{
    GOStringProperty* sp = (GOStringProperty*)findProperty(name);
    return (sp->data());
}
//=============================================================================
std::vector<double>
GraphicsObject::findVectorDoubleProperty(const std::wstring& name)
{
    GOVectorProperty* sp = (GOVectorProperty*)findProperty(name);
    return (sp->data());
}
//=============================================================================
ArrayOf
GraphicsObject::findArrayOfProperty(const std::wstring& name)
{
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(name);
    return (hp->data());
}
//=============================================================================
double
GraphicsObject::findScalarDoubleProperty(const std::wstring& name)
{
    GOScalarProperty* sp = (GOScalarProperty*)findProperty(name);
    return (sp->data());
}
//=============================================================================
unsigned
GraphicsObject::findGoProperty(const std::wstring& name)
{
    GOGObjectsProperty* sp = (GOGObjectsProperty*)findProperty(name);
    if (sp->data().empty()) {
        return 0;
    }
    return (sp->data()[0]);
}
//=============================================================================
void
GraphicsObject::registerProperty(GOGenericProperty* hp, const std::wstring& name, bool iwritable)
{
    std::wstring lower(name);
    std::transform(lower.begin(), lower.end(), lower.begin(), tolower);

    if (!m_properties.findSymbol(lower)) {
        m_property_names_order.push_back(name);
        m_properties_writable[lower] = iwritable;
    }
    m_properties.insertSymbol(lower, hp);
}
//=============================================================================
void
GraphicsObject::sortProperties()
{
    std::sort(m_property_names_order.begin(), m_property_names_order.end());
}
//=============================================================================
wstringVector
GraphicsObject::getFieldnames()
{
    return m_property_names_order;
}
//=============================================================================
void
GraphicsObject::setRestrictedStringScalarDefault(
    const std::wstring& name, const std::wstring& value, double sval)
{
    GORestrictedStringScalarProperty* hp = (GORestrictedStringScalarProperty*)findProperty(name);
    if (!hp) {
        Error(_W("Restricted string/scalar default failed lookup of: ") + L"<" + name + L">");
    }
    hp->data(value);
    hp->scalar(sval);
}
//=============================================================================
void
GraphicsObject::setRestrictedStringColorDefault(
    const std::wstring& name, const std::wstring& value, double red, double green, double blue)
{
    GORestrictedStringColorProperty* hp = (GORestrictedStringColorProperty*)findProperty(name);
    if (!hp) {
        Error(_W("Restricted string/color default failed lookup of: ") + L"<" + name + L">");
    }
    hp->data(value);
    hp->colorSpec(red, green, blue);
}
//=============================================================================
void
GraphicsObject::setRestrictedStringDefault(const std::wstring& name, const std::wstring& value)
{
    GORestrictedStringProperty* hp = (GORestrictedStringProperty*)findProperty(name);
    if (!hp) {
        Error(_W("Set restricted string default failed lookup of: ") + L"<" + name + L">");
    }
    hp->data(value);
}
//=============================================================================
void
GraphicsObject::setRestrictedStringSetDefault(const std::wstring& name, const std::wstring& values)
{
    GORestrictedStringVectorProperty* hp = (GORestrictedStringVectorProperty*)findProperty(name);
    std::vector<std::wstring> data;
    Tokenize(values, data, L"|");
    ((GOStringVector*)hp)->data(data);
}
//=============================================================================
void
GraphicsObject::setThreeVectorDefault(const std::wstring& name, double x, double y, double z)
{
    GOThreeVectorProperty* hp = (GOThreeVectorProperty*)findProperty(name);
    hp->value(x, y, z);
}
//=============================================================================
void
GraphicsObject::setFourVectorDefault(
    const std::wstring& name, double x, double y, double z, double w)
{
    GOFourVectorProperty* hp = (GOFourVectorProperty*)findProperty(name);
    hp->value(x, y, z, w);
}
//=============================================================================
void
GraphicsObject::setTwoVectorDefault(const std::wstring& name, double x, double y)
{
    GOTwoVectorProperty* hp = (GOTwoVectorProperty*)findProperty(name);
    hp->value(x, y);
}
//=============================================================================
void
GraphicsObject::setStringDefault(const std::wstring& name, const std::wstring& value)
{
    GOStringProperty* hp = (GOStringProperty*)findProperty(name);
    hp->data(value);
}
//=============================================================================
void
GraphicsObject::setScalarDoubleDefault(const std::wstring& name, double value)
{
    GOScalarProperty* hp = (GOScalarProperty*)findProperty(name);
    hp->data(value);
}
//=============================================================================
bool
GraphicsObject::isAuto(const std::wstring& mode)
{
    GOAutoManualProperty* hp = (GOAutoManualProperty*)findProperty(mode);
    return hp->isEqual(GO_PROPERTY_VALUE_AUTO_STR);
}
//=============================================================================
bool
GraphicsObject::stringCheck(const std::wstring& name, const std::wstring& value)
{
    GOStringProperty* hp = (GOStringProperty*)findProperty(name);
    return hp->isEqual(value);
}
//=============================================================================
bool
GraphicsObject::isType(const std::wstring& name)
{
    return getType() == name;
}
//=============================================================================
void
GraphicsObject::toManual(const std::wstring& name)
{
    GOAutoManualProperty* qp = (GOAutoManualProperty*)findProperty(name);
    qp->data(GO_PROPERTY_VALUE_MANUAL_STR);
}
//=============================================================================
bool
GraphicsObject::hasChanged(const std::vector<std::wstring>& names)
{
    for (int i = 0; i < names.size(); i++) {
        GOGenericProperty* hp = findProperty(names[i]);
        if (hp->isModified()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
GraphicsObject::hasChanged(const std::wstring& name)
{
    std::vector<std::wstring> names;
    names.push_back(name);
    return hasChanged(names);
}
//=============================================================================
void
GraphicsObject::clearAllChanged()
{
    std::vector<std::wstring> names = m_properties.getAllSymbols();
    clearChanged(names);
}
//=============================================================================
void
GraphicsObject::clearChanged(const std::vector<std::wstring>& names)
{
    for (int i = 0; i < names.size(); i++) {
        clearChanged(names[i]);
    }
}
//=============================================================================
void
GraphicsObject::clearChanged(const std::wstring& name)
{
    GOGenericProperty* hp = findProperty(name);
    hp->clearModified();
}
//=============================================================================
}
//=============================================================================
