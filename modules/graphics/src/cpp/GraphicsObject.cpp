//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
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
static bool
isReplaceableInternalObjectReferenceProperty(const std::wstring& name)
{
    return name == GO_TITLE_PROPERTY_NAME_STR || name == GO_SUBTITLE_PROPERTY_NAME_STR
        || name == GO_X_LABEL_PROPERTY_NAME_STR || name == GO_Y_LABEL_PROPERTY_NAME_STR
        || name == GO_Z_LABEL_PROPERTY_NAME_STR || name == GO_LAYOUT_PROPERTY_NAME_STR;
}
//=============================================================================
static bool
isReplaceableInternalObject(GraphicsObject* gp)
{
    if (!gp) {
        return false;
    }
    return gp->isType(GO_PROPERTY_VALUE_TEXT_STR) || gp->isType(L"layoutoptions");
}
//=============================================================================
static void
deleteReplacedInternalObjectReference(int64 oldHandle, int64 newHandle)
{
    if (oldHandle == newHandle || oldHandle < HANDLE_OFFSET_OBJECT) {
        return;
    }
    GraphicsObject* oldObject = findGraphicsObject(oldHandle, false);
    if (isReplaceableInternalObject(oldObject)) {
        deleteGraphicsObject(oldHandle, false, false);
    }
}
//=============================================================================
GraphicsObject::GraphicsObject() { ref_count = 1; }
//=============================================================================
GraphicsObject::~GraphicsObject()
{
    GOGObjectsProperty* hp = nullptr;
    auto childrenIt = m_properties.find(GO_CHILDREN_PROPERTY_NAME_STR);
    if (childrenIt != m_properties.end()) {
        hp = static_cast<GOGObjectsProperty*>(childrenIt->second);
    }
    if (hp) {
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
    for (auto& it : m_properties) {
        delete it.second;
    }
    m_properties.clear();
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
bool
GraphicsObject::isVisible(const std::wstring& name)
{
    if (m_properties_visible.count(name) == 0) {
        return false;
    }
    return m_properties_visible[name];
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
            msg = fmt::format(_W("Unrecognized property '{0}' for class '{1}'."), name, getType());
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
    GOGObjectsProperty* hp = static_cast<GOGObjectsProperty*>(findProperty(name, false));
    if (hp) {
        int64 oldValue = hp->data().empty() ? 0 : hp->data()[0];
        std::vector<int64> newval;
        newval.push_back(value);
        hp->data(newval);
        if (isReplaceableInternalObjectReferenceProperty(name)) {
            deleteReplacedInternalObjectReference(oldValue, value);
        }
    }
}
//=============================================================================
GOAxis*
GraphicsObject::getParentAxis()
{
    GOGObjectsProperty* parent
        = static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty()) {
        return nullptr;
    }
    int64 parent_handle = parent->data()[0];
    if (parent_handle < HANDLE_OFFSET_OBJECT) {
        return nullptr;
    }
    GraphicsObject* fp = findGraphicsObject(parent_handle, false);
    if (!fp) {
        return nullptr;
    }
    GOStringProperty* name
        = static_cast<GOStringProperty*>(fp->findProperty(GO_TYPE_PROPERTY_NAME_STR));
    if (!name) {
        return nullptr;
    }
    if (name->isEqual(GO_PROPERTY_VALUE_HGGROUP_STR)) {
        GOGObjectsProperty* hgGroupParent = static_cast<GOGObjectsProperty*>(
            fp->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (hgGroupParent && !hgGroupParent->data().empty()
            && hgGroupParent->data()[0] >= HANDLE_OFFSET_OBJECT) {
            fp = findGraphicsObject(hgGroupParent->data()[0], false);
            if (!fp) {
                return nullptr;
            }
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
    GraphicsObject* current = this;
    int guard = 0;
    while (current && guard < 64) {
        GOGObjectsProperty* parent = static_cast<GOGObjectsProperty*>(
            current->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (!parent || parent->data().empty()) {
            return nullptr;
        }
        int64 parent_handle = parent->data()[0];
        if (parent_handle == HANDLE_ROOT_OBJECT || parent_handle == 0) {
            return nullptr;
        }
        if (parent_handle < HANDLE_OFFSET_OBJECT) {
            return findGOFigure(parent_handle);
        }
        current = findGraphicsObject(parent_handle, false);
        ++guard;
    }
    return nullptr;
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
GraphicsObject::registerProperty(
    GOGenericProperty* hp, const std::wstring& name, bool iwritable, bool visible)
{
    std::unordered_map<std::wstring, GOGenericProperty*>::const_iterator got
        = m_properties.find(name);

    if (got == m_properties.end()) {
        m_property_names_order.push_back(name);
        if (visible) {
            m_visible_property_names_order.push_back(name);
        }
        m_properties_writable[name] = iwritable;
        m_properties_visible[name] = visible;
    } else if (got->second != hp) {
        delete got->second;
    }
    m_properties[name] = hp;
}
//=============================================================================
void
GraphicsObject::sortProperties()
{
    parallelSort(m_visible_property_names_order);
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
wstringVector
GraphicsObject::getVisibleFieldnames()
{
    return m_visible_property_names_order;
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
