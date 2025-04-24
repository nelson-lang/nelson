//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "nlsBuildConfig.h"
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
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ParallelSort.hpp"
#include "StringHelpers.hpp"
#include "GOUIControl.h"
//=============================================================================
namespace Nelson {
//=============================================================================
GraphicsObject::GraphicsObject() { ref_count = 1; }
//=============================================================================
GraphicsObject::~GraphicsObject()
{
    GOGObjectsProperty* hp
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> my_children(hp->data());
    for (indexType i = 0; i < my_children.size(); i++) {
        int64 handle = my_children[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            GraphicsObject* gp = findGraphicsObject(handle, false);
            if (gp) {
                gp->dereference();
                if (gp->referenceCount() <= 0) {
                    if (gp->findStringProperty(GO_TYPE_PROPERTY_NAME_STR)
                        == GO_PROPERTY_VALUE_UICONTROL_STR) {
                        ((GOUIControl*)gp)->hide();
                    }
                    freeGraphicsObject(handle);
                    delete gp;
                }
            }
        }
    }
}
//=============================================================================
bool
GraphicsObject::isWritable(const std::wstring& name)
{
    if (m_properties_writable.count(name) == 0) {
        return false;
    }
    return m_properties_writable[name];
}
//=============================================================================
GOGenericProperty*
GraphicsObject::findProperty(const std::wstring& name, bool raiseError)
{
    if (m_properties.empty()) {
        return nullptr;
    }
    if (m_property_names_order.empty()) {
        return nullptr;
    }
    std::unordered_map<std::wstring, GOGenericProperty*>::const_iterator got
        = m_properties.find(name);
    GOGenericProperty* hp = (got != m_properties.end()) ? (got->second) : nullptr;
    if (hp) {
        return hp;
    } else {
        if (raiseError) {
            std::wstring lowerName(name);
            std::wstring proposedName;
            StringHelpers::to_lower(lowerName);
            for (auto element : m_property_names_order) {
                std::wstring lowerElement(element);
                StringHelpers::to_lower(lowerElement);
                if (lowerElement == lowerName) {
                    proposedName = element;
                    break;
                }
            }
            std::wstring msg;
            msg = fmt::sprintf(_W("Unrecognized property '%s' for class '%s'."), name, getType());
            if (!proposedName.empty()) {
                msg = msg + L"\n" + _W("Did you mean:") + L" " + proposedName;
            }
            Error(msg);
        }
        hp = nullptr;
    }
    return nullptr;
}
//=============================================================================
void
GraphicsObject::setGoProperty(const std::wstring& name, int64 value)
{
    GOGObjectsProperty* hp = static_cast<GOGObjectsProperty*>(findProperty(name));
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
    GOGObjectsProperty* parent
        = static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR));
    if (parent->data().empty()) {
        return nullptr;
    }
    unsigned parent_handle = parent->data()[0];
    GraphicsObject* fp = findGraphicsObject(parent_handle);
    GOStringProperty* name
        = static_cast<GOStringProperty*>(fp->findProperty(GO_TYPE_PROPERTY_NAME_STR));
    if (!name) {
        return nullptr;
    }
    if (name->isEqual(GO_PROPERTY_VALUE_HGGROUP_STR)) {
        GOGObjectsProperty* hgGroupParent
            = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_PARENT_PROPERTY_NAME_STR));
        if (hgGroupParent) {
            fp = findGraphicsObject(hgGroupParent->data()[0]);
            name = static_cast<GOStringProperty*>(fp->findProperty(GO_TYPE_PROPERTY_NAME_STR));
            if (!name) {
                return nullptr;
            }
        }
    }
    if (!name->isEqual(GO_PROPERTY_VALUE_AXES_STR)) {
        return nullptr;
    }
    return static_cast<GOAxis*>(fp);
}
//=============================================================================
GOFigure*
GraphicsObject::getParentFigure()
{
    GOAxis* hp = nullptr;
    if (stringCheck(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AXES_STR)) {
        hp = static_cast<GOAxis*>(this);
    } else {
        hp = getParentAxis();
    }
    if (!hp) {
        return nullptr;
    }
    GOGObjectsProperty* parent
        = static_cast<GOGObjectsProperty*>(hp->findProperty(GO_PARENT_PROPERTY_NAME_STR));
    if (!parent || parent->data().empty()) {
        return nullptr;
    }
    unsigned parent_handle = parent->data()[0];
    return findGOFigure(parent_handle);
}
//=============================================================================
std::wstring
GraphicsObject::findStringProperty(const std::wstring& name)
{
    GOStringProperty* sp = static_cast<GOStringProperty*>(findProperty(name));
    return (sp->data());
}
//=============================================================================
wstringVector
GraphicsObject::findStringVectorProperty(const std::wstring& name)
{
    GOStringVectorProperty* sp = static_cast<GOStringVectorProperty*>(findProperty(name));
    return (sp->data());
}
//=============================================================================
std::vector<double>
GraphicsObject::findVectorDoubleProperty(const std::wstring& name)
{
    GOVectorProperty* sp = static_cast<GOVectorProperty*>(findProperty(name));
    return (sp->data());
}
//=============================================================================
ArrayOf
GraphicsObject::findArrayOfProperty(const std::wstring& name)
{
    GOArrayOfProperty* hp = static_cast<GOArrayOfProperty*>(findProperty(name));
    return (hp->data());
}
//=============================================================================
double
GraphicsObject::findScalarDoubleProperty(const std::wstring& name)
{
    GOScalarProperty* sp = static_cast<GOScalarProperty*>(findProperty(name));
    return (sp->data());
}
//=============================================================================
unsigned
GraphicsObject::findGoProperty(const std::wstring& name)
{
    GOGObjectsProperty* sp = static_cast<GOGObjectsProperty*>(findProperty(name));
    if (sp->data().empty()) {
        return 0;
    }
    return (sp->data()[0]);
}
//=============================================================================
void
GraphicsObject::registerProperty(GOGenericProperty* hp, const std::wstring& name, bool iwritable)
{
    std::unordered_map<std::wstring, GOGenericProperty*>::const_iterator got
        = m_properties.find(name);

    if (got == m_properties.end()) {
        m_property_names_order.push_back(name);
        m_properties_writable[name] = iwritable;
    }
    m_properties[name] = hp;
}
//=============================================================================
void
GraphicsObject::sortProperties()
{
    parallelSort(m_property_names_order);
}
//=============================================================================
bool
GraphicsObject::haveProperty(const std::wstring& name)
{
    return (std::find(m_property_names_order.begin(), m_property_names_order.end(), name)
        != m_property_names_order.end());
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
    GORestrictedStringScalarProperty* hp
        = static_cast<GORestrictedStringScalarProperty*>(findProperty(name));
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
    GORestrictedStringColorProperty* hp
        = static_cast<GORestrictedStringColorProperty*>(findProperty(name));
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
    GORestrictedStringProperty* hp = static_cast<GORestrictedStringProperty*>(findProperty(name));
    if (!hp) {
        Error(_W("Set restricted string default failed lookup of: ") + L"<" + name + L">");
    }
    hp->data(value);
}
//=============================================================================
void
GraphicsObject::setRestrictedStringSetDefault(const std::wstring& name, const wstringVector& values)
{
    GORestrictedStringVectorProperty* hp
        = static_cast<GORestrictedStringVectorProperty*>(findProperty(name));
    ((GOStringVectorProperty*)hp)->data(values);
}
//=============================================================================
void
GraphicsObject::setThreeVectorDefault(const std::wstring& name, double x, double y, double z)
{
    GOThreeVectorProperty* hp = static_cast<GOThreeVectorProperty*>(findProperty(name));
    hp->value(x, y, z);
}
//=============================================================================
void
GraphicsObject::setFourVectorDefault(
    const std::wstring& name, double x, double y, double z, double w)
{
    GOFourVectorProperty* hp = static_cast<GOFourVectorProperty*>(findProperty(name));
    hp->value(x, y, z, w);
}
//=============================================================================
void
GraphicsObject::setVectorDoubleDefault(const std::wstring& name, std::vector<double> defaultValues)
{
    GOVectorProperty* hp = static_cast<GOVectorProperty*>(findProperty(name));
    hp->data(defaultValues);
}
//=============================================================================
void
GraphicsObject::setTwoVectorDefault(const std::wstring& name, double x, double y)
{
    GOTwoVectorProperty* hp = static_cast<GOTwoVectorProperty*>(findProperty(name));
    hp->value(x, y);
}
//=============================================================================
void
GraphicsObject::setStringDefault(const std::wstring& name, const std::wstring& value)
{
    findProperty(name)->set(ArrayOf::stringArrayConstructor(value));
}
//=============================================================================
void
GraphicsObject::setScalarDoubleDefault(const std::wstring& name, double value)
{
    GOScalarProperty* hp = static_cast<GOScalarProperty*>(findProperty(name));
    hp->data(value);
}
//=============================================================================
void
GraphicsObject::setScalarPositiveIntegerValueDefault(const std::wstring& name, double value)
{
    GOScalarPositiveIntegerValueProperty* hp
        = static_cast<GOScalarPositiveIntegerValueProperty*>(findProperty(name));
    hp->data(value);
}
//=============================================================================
bool
GraphicsObject::isAuto(const std::wstring& mode)
{
    GOAutoManualProperty* hp = static_cast<GOAutoManualProperty*>(findProperty(mode));
    return hp->isEqual(GO_PROPERTY_VALUE_AUTO_STR);
}
//=============================================================================
bool
GraphicsObject::stringCheck(const std::wstring& name, const std::wstring& value)
{
    GOStringProperty* hp = static_cast<GOStringProperty*>(findProperty(name));
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
    GOAutoManualProperty* qp = static_cast<GOAutoManualProperty*>(findProperty(name));
    qp->data(GO_PROPERTY_VALUE_MANUAL_STR);
}
//=============================================================================
void
GraphicsObject::toAuto(const std::wstring& name)
{
    GOAutoManualProperty* qp = static_cast<GOAutoManualProperty*>(findProperty(name));
    qp->data(GO_PROPERTY_VALUE_AUTO_STR);
}
//=============================================================================

bool
GraphicsObject::hasChanged(const std::vector<std::wstring>& names)
{
    for (indexType i = 0; i < names.size(); i++) {
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
    std::vector<std::wstring> names;
    names.resize(m_properties.size());
    indexType k = 0;
    for (auto& it : m_properties) {
        names[k++] = it.first;
    }
    clearChanged(names);
}
//=============================================================================
void
GraphicsObject::clearChanged(const std::vector<std::wstring>& names)
{
    for (indexType i = 0; i < names.size(); i++) {
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
