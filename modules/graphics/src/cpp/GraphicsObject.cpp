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
#include <algorithm>
#include <cwctype>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "nlsBuildConfig.h"
#include "GraphicsObject.hpp"
#include "GOReferenceSurfaceMask.hpp"
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
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
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
        || name == GO_Z_LABEL_PROPERTY_NAME_STR || name == GO_LABEL_PROPERTY_NAME_STR
        || name == GO_LAYOUT_PROPERTY_NAME_STR;
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
static std::wstring
canonicalGraphicsPropertyName(const std::wstring& name, const wstringVector& names)
{
    // Case-insensitive scan without per-candidate allocations: it runs for every
    // user-facing (non-canonical) property access.
    auto equalsCaseInsensitive = [](const std::wstring& a, const std::wstring& b) {
        if (a.size() != b.size()) {
            return false;
        }
        for (size_t k = 0; k < a.size(); ++k) {
            if (towlower(a[k]) != towlower(b[k])) {
                return false;
            }
        }
        return true;
    };
    for (const auto& element : names) {
        if (equalsCaseInsensitive(element, name)) {
            return element;
        }
    }
    return name;
}
//=============================================================================
// Shortest unambiguous leading-substring match against the property names, used only for
// explicit writes (set/construction). An exact case-insensitive match always wins. With
// several candidates it resolves only if one "base" property is a leading substring of all
// the others ('pos' -> Position, also a prefix of PositionConstraint). Otherwise the name
// is left unchanged so the caller raises the usual error.
static std::wstring
partialGraphicsPropertyName(const std::wstring& name, const wstringVector& names)
{
    if (name.empty()) {
        return name;
    }
    auto isPrefixCaseInsensitive = [](const std::wstring& prefix, const std::wstring& full) {
        if (prefix.size() > full.size()) {
            return false;
        }
        for (size_t k = 0; k < prefix.size(); ++k) {
            if (towlower(prefix[k]) != towlower(full[k])) {
                return false;
            }
        }
        return true;
    };
    std::wstring exact = canonicalGraphicsPropertyName(name, names);
    if (exact != name) {
        return exact;
    }
    for (const auto& element : names) {
        if (element.size() == name.size() && isPrefixCaseInsensitive(name, element)) {
            return element;
        }
    }
    std::vector<const std::wstring*> matches;
    for (const auto& element : names) {
        if (isPrefixCaseInsensitive(name, element)) {
            matches.push_back(&element);
        }
    }
    if (matches.empty()) {
        return name;
    }
    if (matches.size() == 1) {
        return *matches[0];
    }
    const std::wstring* shortest = matches[0];
    for (const auto* m : matches) {
        if (m->size() < shortest->size()) {
            shortest = m;
        }
    }
    for (const auto* m : matches) {
        if (!isPrefixCaseInsensitive(*shortest, *m)) {
            return name;
        }
    }
    return *shortest;
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
    auto found = m_properties_writable.find(name);
    if (found == m_properties_writable.end()) {
        std::wstring canonicalName = canonicalGraphicsPropertyName(name, m_property_names_order);
        found = m_properties_writable.find(canonicalName);
        if (found == m_properties_writable.end()) {
            return false;
        }
    }
    return found->second;
}
//=============================================================================
void
GraphicsObject::setPropertyWritable(const std::wstring& name, bool writable)
{
    if (m_properties_writable.count(name) != 0) {
        m_properties_writable[name] = writable;
    }
}
//=============================================================================
bool
GraphicsObject::isVisible(const std::wstring& name)
{
    auto found = m_properties_visible.find(name);
    if (found == m_properties_visible.end()) {
        std::wstring canonicalName = canonicalGraphicsPropertyName(name, m_property_names_order);
        found = m_properties_visible.find(canonicalName);
        if (found == m_properties_visible.end()) {
            return false;
        }
    }
    return found->second;
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
    // Fast path: internal callers pass the canonical GO_* name, so an exact hash lookup skips
    // the case-insensitive scan. Paint and update do hundreds of lookups per frame.
    std::unordered_map<std::wstring, GOGenericProperty*>::const_iterator got
        = m_properties.find(name);
    if (got != m_properties.end()) {
        return got->second;
    }
    std::wstring canonicalName = canonicalGraphicsPropertyName(name, m_property_names_order);
    got = m_properties.find(canonicalName);
    GOGenericProperty* hp = (got != m_properties.end()) ? (got->second) : nullptr;
    if (hp) {
        return hp;
    } else {
        if (raiseError) {
            std::wstring proposedName = canonicalName == name ? L"" : canonicalName;
            std::wstring msg;
            msg = formatI18nMessage(
                resolveMessageTemplate(L"Nelson:graphics:unrecognizedPropertySClass",
                    L"Unrecognized property '%s' for class '%s'."),
                name, getType());
            if (!proposedName.empty()) {
                msg = msg + L"\n"
                    + formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:graphics:didYouMeanS", L"Did you mean: %s"),
                        proposedName);
            }
            RaiseError(msg, L"Nelson:graphics:unrecognizedProperty");
        }
        hp = nullptr;
    }
    return nullptr;
}
//=============================================================================
std::wstring
GraphicsObject::resolvePartialPropertyName(const std::wstring& name)
{
    if (m_property_names_order.empty()) {
        return name;
    }
    return partialGraphicsPropertyName(name, m_property_names_order);
}
//=============================================================================
std::wstring
GraphicsObject::resolvePartialPropertyName(const std::wstring& name, const wstringVector& names)
{
    if (names.empty()) {
        return name;
    }
    return partialGraphicsPropertyName(name, names);
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
    // A transform may be nested inside another transform, so walk the whole ancestor chain.
    int guard = 0;
    while ((name->isEqual(GO_PROPERTY_VALUE_HGGROUP_STR)
               || name->isEqual(GO_PROPERTY_VALUE_HGTRANSFORM_STR)
               || name->isEqual(GO_PROPERTY_VALUE_BUBBLE_CLOUD_STR)
               || name->isEqual(GO_PROPERTY_VALUE_HEATMAP_STR)
               || name->isEqual(GO_PROPERTY_VALUE_PARALLEL_PLOT_STR))
        && guard < 64) {
        ++guard;
        GOGObjectsProperty* hgGroupParent = static_cast<GOGObjectsProperty*>(
            fp->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (!hgGroupParent || hgGroupParent->data().empty()
            || hgGroupParent->data()[0] < HANDLE_OFFSET_OBJECT) {
            break;
        }
        fp = findGraphicsObject(hgGroupParent->data()[0], false);
        if (!fp) {
            return nullptr;
        }
        name = static_cast<GOStringProperty*>(fp->findProperty(GO_TYPE_PROPERTY_NAME_STR));
        if (!name) {
            return nullptr;
        }
    }
    if (!name->isEqual(GO_PROPERTY_VALUE_AXES_STR)
        && !name->isEqual(GO_PROPERTY_VALUE_POLAR_AXES_STR)) {
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
        if (parent_handle == HANDLE_ROOT_OBJECT) {
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
    GOGenericProperty* gp = findProperty(name);
    if (GOStringVectorProperty* sv = dynamic_cast<GOStringVectorProperty*>(gp)) {
        return (sv->data());
    }
    // Some objects (text) register String as a GOStringProperty of newline-joined lines.
    if (GOStringProperty* sp = dynamic_cast<GOStringProperty*>(gp)) {
        wstringVector lines;
        const std::wstring joined = sp->data();
        size_t start = 0;
        for (;;) {
            size_t pos = joined.find(L'\n', start);
            if (pos == std::wstring::npos) {
                lines.push_back(joined.substr(start));
                break;
            }
            lines.push_back(joined.substr(start, pos - start));
            start = pos + 1;
        }
        return lines;
    }
    return {};
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
int64
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
    if (m_properties.find(name) != m_properties.end()) {
        return true;
    }
    std::wstring canonicalName = canonicalGraphicsPropertyName(name, m_property_names_order);
    return (std::find(m_property_names_order.begin(), m_property_names_order.end(), canonicalName)
        != m_property_names_order.end());
}
//=============================================================================
wstringVector
GraphicsObject::fieldnames()
{
    return m_property_names_order;
}
//=============================================================================
void
GraphicsObject::applyReferenceSurfaceMask(wstringVector& visibleNames)
{
    const wstringVector* hidden = referenceSurfaceHiddenProperties(getType());
    if (!hidden) {
        return;
    }
    visibleNames.erase(std::remove_if(visibleNames.begin(), visibleNames.end(),
                           [hidden](const std::wstring& name) {
                               return std::find(hidden->begin(), hidden->end(), name)
                                   != hidden->end();
                           }),
        visibleNames.end());
}
//=============================================================================
wstringVector
GraphicsObject::getVisibleFieldnames()
{
    if (isType(GO_PROPERTY_VALUE_TEXT_STR)) {
        return {
            GO_COLOR_PROPERTY_NAME_STR,
            GO_COLOR_MODE_PROPERTY_NAME_STR,
            GO_POSITION_PROPERTY_NAME_STR,
            GO_STRING_PROPERTY_NAME_STR,
            GO_INTERPRETER_PROPERTY_NAME_STR,
            GO_EXTENT_PROPERTY_NAME_STR,
            GO_ROTATION_PROPERTY_NAME_STR,
            GO_FONT_NAME_PROPERTY_NAME_STR,
            GO_FONT_UNITS_PROPERTY_NAME_STR,
            GO_FONT_SIZE_PROPERTY_NAME_STR,
            GO_FONT_ANGLE_PROPERTY_NAME_STR,
            GO_FONT_WEIGHT_PROPERTY_NAME_STR,
            GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR,
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR,
            GO_EDGE_COLOR_PROPERTY_NAME_STR,
            GO_LINE_STYLE_PROPERTY_NAME_STR,
            GO_LINE_WIDTH_PROPERTY_NAME_STR,
            GO_BACKGROUND_COLOR_PROPERTY_NAME_STR,
            GO_MARGIN_PROPERTY_NAME_STR,
            GO_EDITING_PROPERTY_NAME_STR,
            GO_CLIPPING_PROPERTY_NAME_STR,
            GO_FONT_SMOOTHING_PROPERTY_NAME_STR,
            GO_INTERACTIONS_PROPERTY_NAME_STR,
            GO_UNITS_PROPERTY_NAME_STR,
            GO_CHILDREN_PROPERTY_NAME_STR,
            GO_PARENT_PROPERTY_NAME_STR,
            GO_VISIBLE_PROPERTY_NAME_STR,
            GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR,
            GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR,
            GO_CONTEXT_MENU_PROPERTY_NAME_STR,
            GO_BUSY_ACTION_PROPERTY_NAME_STR,
            GO_BEING_DELETED_PROPERTY_NAME_STR,
            GO_INTERRUPTIBLE_PROPERTY_NAME_STR,
            GO_CREATE_FCN_PROPERTY_NAME_STR,
            GO_DELETE_FCN_PROPERTY_NAME_STR,
            GO_TYPE_PROPERTY_NAME_STR,
            GO_TAG_PROPERTY_NAME_STR,
            GO_USER_DATA_PROPERTY_NAME_STR,
            GO_SELECTED_PROPERTY_NAME_STR,
            GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR,
            GO_HIT_TEST_PROPERTY_NAME_STR,
            GO_PICKABLE_PARTS_PROPERTY_NAME_STR,
            GO_AFFECT_AUTO_LIMITS_PROPERTY_NAME_STR,
            GO_SERIES_INDEX_PROPERTY_NAME_STR,
        };
    }
    wstringVector names = m_visible_property_names_order;
    applyReferenceSurfaceMask(names);
    return names;
}
//=============================================================================
void
GraphicsObject::setRestrictedStringScalarDefault(
    const std::wstring& name, const std::wstring& value, double sval)
{
    GORestrictedStringScalarProperty* hp
        = static_cast<GORestrictedStringScalarProperty*>(findProperty(name));
    if (!hp) {
        Error(L"Nelson:graphics:defaultLookupFailed",
            L"Restricted string default failed lookup of: <%s>", name);
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
        Error(L"Nelson:graphics:defaultLookupFailed",
            L"Restricted string default failed lookup of: <%s>", name);
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
        Error(L"Nelson:graphics:defaultLookupFailed",
            L"Restricted string default failed lookup of: <%s>", name);
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
    // The property may be a GOThreeVectorProperty or a sibling fixed-3 type such as
    // GOColorProperty. Both derive from GOVectorProperty, so set through that base to avoid an
    // invalid downcast (UBSan type confusion).
    auto* hp = static_cast<GOVectorProperty*>(findProperty(name));
    hp->data(std::vector<double> { x, y, z });
}
//=============================================================================
void
GraphicsObject::setFourVectorDefault(
    const std::wstring& name, double x, double y, double z, double w)
{
    auto* hp = static_cast<GOVectorProperty*>(findProperty(name));
    hp->data(std::vector<double> { x, y, z, w });
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
    auto* hp = static_cast<GOVectorProperty*>(findProperty(name));
    hp->data(std::vector<double> { x, y });
}
//=============================================================================
void
GraphicsObject::setStringDefault(const std::wstring& name, const std::wstring& value)
{
    findProperty(name)->set(ArrayOf::stringArray(value));
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
bool
GraphicsObject::hasAnyChanged()
{
    for (auto& it : m_properties) {
        if (it.second->isModified()) {
            return true;
        }
    }
    return false;
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
} // namespace Nelson
//=============================================================================
