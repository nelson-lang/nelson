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
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObjectDisplay.hpp"
#include "GraphicsObject.hpp"
#include "StringHelpers.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "GORoot.hpp"
#include "ClassToString.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include <algorithm>
#include <cmath>
#include <cwctype>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isIntegerValue(double value)
{
    return std::isfinite(value) && std::fabs(std::round(value) - value) < 1e-12;
}
//=============================================================================
static std::wstring
formatNumber(double value)
{
    if (std::fabs(value) < 1e-12) {
        value = 0;
    }
    if (isIntegerValue(value)) {
        return fmt::format(L"{:.0f}", value);
    }
    return fmt::format(L"{:.4f}", value);
}
//=============================================================================
static std::wstring
formatDimensions(const Dimensions& dims)
{
    indexType length = std::max<indexType>(dims.length(), 2);
    std::wstring text;
    for (indexType k = 0; k < length; ++k) {
        if (k > 0) {
            text += L"\u00D7";
        }
        text += std::to_wstring(dims.dimensionLength(k));
    }
    return text;
}
//=============================================================================
static bool
isRealNumeric(NelsonType type)
{
    return type == NLS_DOUBLE || type == NLS_SINGLE || type == NLS_LOGICAL || IS_INTEGER_TYPE(type);
}
//=============================================================================
static std::wstring
formatNumericArray(ArrayOf value)
{
    if (value.isEmpty()) {
        return L"[]";
    }
    NelsonType originalType = value.dataClass();
    value.promoteType(NLS_DOUBLE);
    auto* data = static_cast<const double*>(value.dataPointer());
    indexType count = value.elementCount();
    if (count == 1) {
        return formatNumber(data[0]);
    }

    const indexType previewCount = std::min<indexType>(count, 8);
    std::wstring text = L"[";
    for (indexType k = 0; k < previewCount; ++k) {
        if (k > 0) {
            text += L" ";
        }
        text += formatNumber(data[k]);
    }
    if (count > previewCount) {
        text += L" \u2026 ]";
        text += L" (" + formatDimensions(value.dimensions()) + L" " + ClassToStringW(originalType)
            + L")";
    } else {
        text += L"]";
    }
    return text;
}
//=============================================================================
static std::wstring
formatCharacterArray(const ArrayOf& value)
{
    std::wstring text = value.asWideString();
    if (text == GO_PROPERTY_VALUE_ON_STR || text == GO_PROPERTY_VALUE_OFF_STR) {
        return text;
    }
    return L"'" + text + L"'";
}
//=============================================================================
static std::wstring
formatPropertyValue(GOGenericProperty* hp)
{
    ArrayOf value = hp->get();
    NelsonType type = value.dataClass();
    if (isRealNumeric(type)) {
        return formatNumericArray(value);
    }
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return formatCharacterArray(value);
    }
    return hp->toWideString();
}
//=============================================================================
static std::wstring
getPropertyValue(GraphicsObject* fp, const std::wstring& name)
{
    GOGenericProperty* hp = fp->findProperty(name, false);
    if (hp) {
        try {
            return formatPropertyValue(hp);
        } catch (...) {
            return hp->toWideString();
        }
    }
    return L"";
}
//=============================================================================
static bool
hasName(const wstringVector& names, const std::wstring& name)
{
    return std::find(names.begin(), names.end(), name) != names.end();
}
//=============================================================================
static std::wstring
getObjectType(GraphicsObject* fp)
{
    GOGenericProperty* hpType = fp->findProperty(GO_TYPE_PROPERTY_NAME_STR, false);
    if (!hpType) {
        return L"graphics_object";
    }
    std::wstring type = hpType->get().asWideString();
    if (!type.empty()) {
        type[0] = static_cast<wchar_t>(std::towupper(type[0]));
    }
    return type;
}
//=============================================================================
static std::wstring
getDisplayObjectType(GraphicsObject* fp)
{
    if (fp->isType(GO_PROPERTY_VALUE_ROOT_STR)) {
        return L"Graphics Root";
    }
    return getObjectType(fp);
}
//=============================================================================
static std::wstring
getObjectString(GraphicsObject* fp)
{
    GOGenericProperty* hpString = fp->findProperty(GO_STRING_PROPERTY_NAME_STR, false);
    if (!hpString) {
        return L"";
    }
    ArrayOf value = hpString->get();
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return value.asWideString();
    }
    return hpString->toWideString();
}
//=============================================================================
static std::wstring
getObjectTag(GraphicsObject* fp)
{
    GOGenericProperty* hpTag = fp->findProperty(GO_TAG_PROPERTY_NAME_STR, false);
    if (!hpTag) {
        return L"";
    }
    ArrayOf value = hpTag->get();
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return value.asWideString();
    }
    return L"";
}
//=============================================================================
static std::wstring
getObjectTypeLower(GraphicsObject* fp)
{
    std::wstring type = getObjectType(fp);
    std::transform(type.begin(), type.end(), type.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    return type;
}
//=============================================================================
static wstringVector
filterPropertyNames(const wstringVector& visibleNames, const wstringVector& requestedNames)
{
    wstringVector filtered;
    for (const std::wstring& name : requestedNames) {
        if (hasName(visibleNames, name)) {
            filtered.push_back(name);
        }
    }
    return filtered;
}
//=============================================================================
static wstringVector
getCompactPropertyNames(GraphicsObject* fp, const wstringVector& visibleNames)
{
    std::wstring type = getObjectTypeLower(fp);
    if (type == L"root") {
        return filterPropertyNames(visibleNames,
            { GO_CURRENT_FIGURE_PROPERTY_NAME_STR, GO_SCREEN_PIXELS_PER_INCH_PROPERTY_NAME_STR,
                GO_SCREEN_SIZE_PROPERTY_NAME_STR, GO_MONITOR_POSITIONS_PROPERTY_NAME_STR,
                GO_UNITS_PROPERTY_NAME_STR });
    }
    if (type == L"figure") {
        return filterPropertyNames(visibleNames,
            { GO_NUMBER_PROPERTY_NAME_STR, GO_NAME_PROPERTY_NAME_STR, GO_COLOR_PROPERTY_NAME_STR,
                GO_POSITION_PROPERTY_NAME_STR, GO_UNITS_PROPERTY_NAME_STR });
    }
    if (type == L"axes") {
        return filterPropertyNames(visibleNames,
            { GO_X_LIM_PROPERTY_NAME_STR, GO_Y_LIM_PROPERTY_NAME_STR, GO_X_SCALE_PROPERTY_NAME_STR,
                GO_Y_SCALE_PROPERTY_NAME_STR, GO_GRID_LINE_STYLE_PROPERTY_NAME_STR,
                GO_POSITION_PROPERTY_NAME_STR, GO_UNITS_PROPERTY_NAME_STR });
    }
    if (type == L"line") {
        return filterPropertyNames(visibleNames,
            { GO_COLOR_PROPERTY_NAME_STR, GO_LINE_STYLE_PROPERTY_NAME_STR,
                GO_LINE_WIDTH_PROPERTY_NAME_STR, GO_MARKER_PROPERTY_NAME_STR,
                GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR });
    }
    if (type == L"text") {
        return filterPropertyNames(visibleNames,
            { GO_STRING_PROPERTY_NAME_STR, GO_POSITION_PROPERTY_NAME_STR,
                GO_FONT_SIZE_PROPERTY_NAME_STR, GO_COLOR_PROPERTY_NAME_STR,
                GO_VISIBLE_PROPERTY_NAME_STR });
    }
    if (type == L"subplottext") {
        return filterPropertyNames(visibleNames,
            { GO_STRING_PROPERTY_NAME_STR, GO_FONT_SIZE_PROPERTY_NAME_STR,
                GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_FONT_NAME_PROPERTY_NAME_STR,
                GO_COLOR_PROPERTY_NAME_STR, GO_INTERPRETER_PROPERTY_NAME_STR });
    }
    return filterPropertyNames(visibleNames,
        { GO_TYPE_PROPERTY_NAME_STR, GO_PARENT_PROPERTY_NAME_STR, GO_CHILDREN_PROPERTY_NAME_STR,
            GO_POSITION_PROPERTY_NAME_STR, GO_COLOR_PROPERTY_NAME_STR, GO_VISIBLE_PROPERTY_NAME_STR,
            GO_USER_DATA_PROPERTY_NAME_STR });
}
//=============================================================================
static std::wstring
getRootPropertyValue(GraphicsObject* fp, const std::wstring& name)
{
    if (name != GO_CURRENT_FIGURE_PROPERTY_NAME_STR) {
        return getPropertyValue(fp, name);
    }
    GOGenericProperty* hp = fp->findProperty(name, false);
    if (!hp) {
        return L"";
    }
    ArrayOf value = hp->get();
    if (value.isEmpty()) {
        return L"[0\u00D70 GraphicsPlaceholder]";
    }
    return hp->toWideString();
}
//=============================================================================
static std::wstring
propertyLines(GraphicsObject* fp, const wstringVector& names)
{
    size_t width = 0;
    for (const std::wstring& name : names) {
        width = std::max(width, name.length());
    }
    std::wstring text;
    for (const std::wstring& name : names) {
        std::wstring value = fp->isType(GO_PROPERTY_VALUE_ROOT_STR) ? getRootPropertyValue(fp, name)
                                                                    : getPropertyValue(fp, name);
        std::wstring padding(width > name.length() ? width - name.length() : 0, L' ');
        text += L"    " + padding + name;
        if (!value.empty()) {
            text += L": " + value;
        }
        text += L"\n";
    }
    return text;
}
//=============================================================================
static void
displayScalarGraphicsObject(Interface* io, GraphicsObject* fp, int64 handle)
{
    wstringVector names = fp->getVisibleFieldnames();
    std::wstring type = getDisplayObjectType(fp);
    GOGenericProperty* hpNumber = fp->findProperty(GO_NUMBER_PROPERTY_NAME_STR, false);
    io->outputMessage(L"\n");
    if (fp->isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)) {
        io->outputMessage(L"  "
            + formatI18nMessage(Nelson::resolveMessageTemplate(
                                    L"Nelson:graphics:withProperties", L"%s with properties:"),
                L"Text (" + getObjectString(fp) + L")")
            + L"\n\n");
    } else if (hpNumber) {
        std::wstring displayToken = hpNumber->toWideString();
        if (displayToken == L"[]") {
            displayToken = getObjectTag(fp);
        }
        if (displayToken.empty()) {
            io->outputMessage(L"  "
                + formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:graphics:withProperties", L"%s with properties:"),
                    type)
                + L"\n\n");
        } else {
            io->outputMessage(L"  "
                + formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:graphics:withProperties", L"%s with properties:"),
                    type + L" (" + displayToken + L")")
                + L"\n\n");
        }
    } else {
        io->outputMessage(L"  "
            + formatI18nMessage(Nelson::resolveMessageTemplate(
                                    L"Nelson:graphics:withProperties", L"%s with properties:"),
                type)
            + L"\n\n");
    }

    wstringVector compactNames = getCompactPropertyNames(fp, names);
    if (compactNames.empty()) {
        compactNames = names;
    }
    io->outputMessage(propertyLines(fp, compactNames));
    io->outputMessage(L"\n");
    if (fp->isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)) {
        io->outputMessage(L"  " + _W("Use GET to show all properties") + L"\n");
        return;
    }
    std::wstring expandedText = L"\n" + propertyLines(fp, names);
    io->outputExpandableText(L"  " + _W("Show all properties"), expandedText,
        L"  " + _W("Show all properties") + L"\n", L"graphics_object", handle);
}
//=============================================================================
void
graphicsObjectDisplay(Interface* io, const Dimensions& dims, nelson_handle* ptrGO)
{
    if (dims.isScalar()) {
        if (ptrGO != nullptr) {
            int64 handle = (int64)ptrGO[0];
            GraphicsObject* fp = nullptr;
            if (isUnassignedGraphicsObject(handle)) {
                io->outputMessage("\n");
                io->outputMessage(L"  " + _W("Unassigned graphics_object") + L"\n");
                return;
            }
            if (isDeletedGraphicsObject(handle)) {
                io->outputMessage("\n");
                io->outputMessage(L"  " + _W("Deleted graphics_object") + L"\n");
                return;
            }
            if (handle == HANDLE_ROOT_OBJECT) {
                fp = getGraphicsRootObject();
            } else if (handle >= HANDLE_OFFSET_OBJECT) {
                fp = findGraphicsObject(handle);
            } else {
                fp = (GraphicsObject*)findGOFigure(handle);
            }
            if (!fp) {
                Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
            }
            displayScalarGraphicsObject(io, fp, handle);
        }
    } else {
        if (dims.isColumnVector()) {
            io->outputMessage("\n");
            for (size_t k = 0; k < dims.elementCount(); ++k) {
                int64 handle = (int64)ptrGO[k];
                GraphicsObject* fp = nullptr;
                if (isUnassignedGraphicsObject(handle)) {
                    io->outputMessage(L"  " + _W("Unassigned graphics_object") + L"\n");
                } else if (isDeletedGraphicsObject(handle)) {
                    io->outputMessage(L"  " + _W("Deleted graphics_object") + L"\n");
                } else {
                    if (handle == HANDLE_ROOT_OBJECT) {
                        fp = getGraphicsRootObject();
                    } else if (handle >= HANDLE_OFFSET_OBJECT) {
                        fp = findGraphicsObject(handle);
                    } else {
                        fp = (GraphicsObject*)findGOFigure(handle);
                    }
                    if (!fp) {
                        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
                    }
                    GOGenericProperty* hpType = fp->findProperty(GO_TYPE_PROPERTY_NAME_STR, false);
                    if (hpType) {
                        io->outputMessage(L"  " + hpType->get().asWideString() + L"\n");
                    }
                }
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
