//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include <limits>
#include <cmath>
#include <cstdio>
#include "GOUIComponent.h"
#include "GOChartDecorators.hpp"
#include "GOFigure.hpp"
#include "RenderInterface.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GORoot.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOPositionHelpers.hpp"
#include "GOHelpers.hpp"
#include "GOUIContainer.h"
#include "UITicksHelpers.h"
#include "GOUIComponentUnits.hpp"
#include "GOUIWebChildren.hpp"
#include "GOUIComponentWebGeometry.hpp"
#include "RenderWeb.hpp"
#include "UIItemsHelpers.h"
#include "GOUIHTML.h"
#include "GOUIRadioButton.h"
#include "MainThreadTaskQueue.hpp"
#include "GOUITable.h"
#include "GOUITab.h"
#include "GOUITabGroup.h"
#include "characters_encoding.hpp"
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static UIComponentBindingFactory uiComponentBindingFactory = nullptr;
//=============================================================================
void
setUIComponentBindingFactory(UIComponentBindingFactory factory)
{
    uiComponentBindingFactory = factory;
}
//=============================================================================
UIComponentBindingFactory
getUIComponentBindingFactory()
{
    return uiComponentBindingFactory;
}
//=============================================================================
GOUIComponent::GOUIComponent() = default;
//=============================================================================
GOUIComponent::~GOUIComponent()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    deleteWidget();
}
//=============================================================================
void
GOUIComponent::updateState()
{
    // Headless host (WEB_GUI): no widget backend is registered. The web path only
    // reads and writes properties.
    if (!binding && parentGoWindow && !parentGoWindow->isHeadlessHost()
        && uiComponentBindingFactory) {
        binding = uiComponentBindingFactory(this);
    }
    bool isNewWidget = false;
    if (binding && !binding->exists()) {
        isNewWidget = binding->create();
    }
    if (!binding || !binding->exists()) {
        onUnitsChanged(isNewWidget);
        syncSimplePositionTriplet(this);
        clearAllChanged();
        return;
    }
    {
        ProgrammaticGuard guard(this);
        onUnitsChanged(isNewWidget);
        binding->applyCommonProperties(isNewWidget);
        binding->applyState(isNewWidget);
        clearAllChanged();
    }
    if (binding->consumeGeometryChange()) {
        onGeometryChanged();
    }
}
//=============================================================================
void
GOUIComponent::emitCallbackEvent(const std::wstring& callbackPropertyName,
    const std::wstring& eventDataClassName, const std::wstring& eventName,
    const wstringVector& fieldNames, const ArrayOfVector& fieldValues, bool evenIfProgrammatic)
{
    if (programmaticChange && !evenIfProgrammatic) {
        return;
    }
    GOCallbackProperty* goCallback = (GOCallbackProperty*)findProperty(callbackPropertyName, false);
    if (!goCallback) {
        return;
    }
    ArrayOf callbackAsArrayOf = goCallback->get();
    if (callbackAsArrayOf.isEmpty()) {
        return;
    }
    goCallback->pushEvent(this, eventDataClassName, eventName, fieldNames, fieldValues);
}
//=============================================================================
void
GOUIComponent::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    if (gc.getRenderName() == L"Web") {
        // Emit a real HTML control: a Qt snapshot cannot be interacted with and leaks
        // alpha noise.
        auto* web = dynamic_cast<RenderWeb*>(&gc);
        if (web != nullptr) {
            std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
            if (sizevec.size() == 4) {
                double width = gc.width();
                double height = gc.height();
                uiComponentParentSizeInPixels(this, width, height);
                uiComponentPositionInPixels(this, sizevec, width, height);
                describeForWeb(*web, sizevec);
            }
        }
        return;
    }
    if (binding && binding->exists()) {
        std::wstring renderName = gc.getRenderName();
        if (renderName != L"GL" && renderName != L"Image" && renderName != L"SVG") {
            binding->renderSnapshot(gc);
        }
    }
}
//=============================================================================
static std::string
uiComponentCssColor(const std::vector<double>& rgb)
{
    if (rgb.size() < 3) {
        return std::string();
    }
    auto to255 = [](double v) {
        int n = (int)(v * 255.0 + 0.5);
        return n < 0 ? 0 : (n > 255 ? 255 : n);
    };
    return "rgb(" + std::to_string(to255(rgb[0])) + "," + std::to_string(to255(rgb[1])) + ","
        + std::to_string(to255(rgb[2])) + ")";
}
//=============================================================================
static std::string
uiComponentColorProp(GraphicsObject* object, const std::wstring& name)
{
    auto* colorProperty = dynamic_cast<GOColorProperty*>(object->findProperty(name, false));
    if (colorProperty == nullptr || colorProperty->isNone()) {
        return std::string();
    }
    return uiComponentCssColor(colorProperty->data());
}
//=============================================================================
static std::wstring
uiComponentJoinLines(const wstringVector& lines)
{
    std::wstring text;
    for (size_t k = 0; k < lines.size(); ++k) {
        if (k > 0) {
            text += L"\n";
        }
        text += lines[k];
    }
    return text;
}
//=============================================================================
static wstringVector
uiComponentSplitLines(const std::wstring& text)
{
    wstringVector lines;
    std::wstring current;
    for (wchar_t c : text) {
        if (c == L'\n') {
            lines.push_back(current);
            current.clear();
        } else if (c != L'\r') {
            current += c;
        }
    }
    lines.push_back(current);
    return lines;
}
//=============================================================================
static double
uiComponentScalarOf(GOGenericProperty* property, double fallback)
{
    if (property == nullptr) {
        return fallback;
    }
    ArrayOf value = property->get();
    if (value.isEmpty()) {
        return fallback;
    }
    try {
        value.promoteType(NLS_DOUBLE);
        return static_cast<const double*>(value.dataPointer())[0];
    } catch (...) {
        return fallback;
    }
}
//=============================================================================
// Attach the ticks a dial/scale control labels to its web payload. Auto ticks
// use a denser "nice" scale sized to the widget; MajorTicks stays coarser.
// pixelExtent and divisor set the density per kind.
static void
uiComponentAddTickData(
    GraphicsObject* object, nlohmann::json& extra, double pixelExtent, double divisor)
{
    std::vector<double> limits(object->findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
    const double minimum = (limits.size() == 2) ? limits[0] : 0.0;
    const double maximum = (limits.size() == 2) ? limits[1] : 1.0;
    const bool majorAuto
        = object->stringCheck(GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    std::vector<double> ticks;
    wstringVector labelsWide;
    uiDisplayMajorTicks(minimum, maximum, majorAuto,
        object->findVectorDoubleProperty(GO_MAJOR_TICKS_PROPERTY_NAME_STR),
        object->findStringVectorProperty(GO_MAJOR_TICK_LABELS_PROPERTY_NAME_STR), pixelExtent,
        divisor, ticks, labelsWide);
    extra["major"] = ticks;
    extra["minor"] = object->findVectorDoubleProperty(GO_MINOR_TICKS_PROPERTY_NAME_STR);
    std::vector<std::string> labels;
    for (const std::wstring& label : labelsWide) {
        labels.push_back(wstring_to_utf8(label));
    }
    extra["labels"] = labels;
}
//=============================================================================
static std::wstring
uiComponentValueAsText(const ArrayOf& value)
{
    if (value.isEmpty()) {
        return std::wstring();
    }
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return value.asWideString();
    }
    try {
        ArrayOf tmp(value);
        tmp.promoteType(NLS_DOUBLE);
        double d = static_cast<const double*>(tmp.dataPointer())[0];
        wchar_t buffer[64];
        swprintf(buffer, 64, L"%g", d);
        return std::wstring(buffer);
    } catch (...) {
        return std::wstring();
    }
}
//=============================================================================
// A uidatepicker SerialValue is a datenum; adding JULIAN_DAY_OFFSET gives a
// Julian Day Number. Convert JDN <-> ISO date (yyyy-mm-dd) without Qt.
static const long long UI_JULIAN_DAY_OFFSET = 1721059;
//=============================================================================
static void
uiComponentSerialToYMD(double serial, long long& year, long long& month, long long& day)
{
    long long jdn = static_cast<long long>(std::llround(serial)) + UI_JULIAN_DAY_OFFSET;
    long long l = jdn + 68569;
    long long n = (4 * l) / 146097;
    l = l - (146097 * n + 3) / 4;
    long long i = (4000 * (l + 1)) / 1461001;
    l = l - (1461 * i) / 4 + 31;
    long long j = (80 * l) / 2447;
    day = l - (2447 * j) / 80;
    l = j / 11;
    month = j + 2 - 12 * l;
    year = 100 * (n - 49) + i + l;
}
//=============================================================================
// Format a datepicker serial value using its DisplayFormat tokens (dd, d, MMMM,
// MMM, MM, M, yyyy/uuuu, yy). Other characters are copied verbatim. An empty
// format gives the ISO date (yyyy-MM-dd).
static std::wstring
uiComponentSerialToText(double serial, const std::wstring& format)
{
    if (!std::isfinite(serial)) {
        return std::wstring();
    }
    long long year = 0, month = 0, day = 0;
    uiComponentSerialToYMD(serial, year, month, day);
    std::wstring fmt = format.empty() ? std::wstring(L"yyyy-MM-dd") : format;
    static const wchar_t* shortMonths[] = { L"Jan", L"Feb", L"Mar", L"Apr", L"May", L"Jun", L"Jul",
        L"Aug", L"Sep", L"Oct", L"Nov", L"Dec" };
    static const wchar_t* longMonths[] = { L"January", L"February", L"March", L"April", L"May",
        L"June", L"July", L"August", L"September", L"October", L"November", L"December" };
    long long monthIndex = (month >= 1 && month <= 12) ? month - 1 : 0;
    std::wstring out;
    wchar_t buffer[16];
    size_t i = 0;
    while (i < fmt.size()) {
        wchar_t c = fmt[i];
        if (c == L'd' || c == L'M' || c == L'y' || c == L'u') {
            size_t j = i;
            while (j < fmt.size() && fmt[j] == c) {
                j++;
            }
            size_t count = j - i;
            if (c == L'd') {
                swprintf(buffer, 16, count >= 2 ? L"%02lld" : L"%lld", day);
                out += buffer;
            } else if (c == L'M') {
                if (count >= 4) {
                    out += longMonths[monthIndex];
                } else if (count == 3) {
                    out += shortMonths[monthIndex];
                } else {
                    swprintf(buffer, 16, count == 2 ? L"%02lld" : L"%lld", month);
                    out += buffer;
                }
            } else {
                if (count == 2) {
                    swprintf(buffer, 16, L"%02lld", ((year % 100) + 100) % 100);
                } else {
                    swprintf(buffer, 16, L"%04lld", year);
                }
                out += buffer;
            }
            i = j;
        } else {
            out += c;
            i++;
        }
    }
    return out;
}
//=============================================================================
static double
uiComponentIsoToSerial(const std::string& iso)
{
    int y = 0, m = 0, d = 0;
    if (std::sscanf(iso.c_str(), "%d-%d-%d", &y, &m, &d) != 3) {
        return std::nan("");
    }
    long long a = (14 - m) / 12;
    long long yy = (long long)y + 4800 - a;
    long long mm = (long long)m + 12 * a - 3;
    long long jdn = d + (153 * mm + 2) / 5 + 365 * yy + yy / 4 - yy / 100 + yy / 400 - 32045;
    return static_cast<double>(jdn - UI_JULIAN_DAY_OFFSET);
}
//=============================================================================
static ArrayOf
uiComponentMakeDatetime(double serialValue)
{
    wstringVector fieldnames = { L"SerialValue", L"FormatValue", L"TimeZoneValue" };
    ArrayOfVector fieldvalues;
    fieldvalues.push_back(ArrayOf::doubleScalar(serialValue));
    fieldvalues.push_back(ArrayOf::characterArray(std::wstring(L"dd-MMM-yyyy")));
    fieldvalues.push_back(ArrayOf::characterArray(std::wstring(L"")));
    return ArrayOf::classArray(L"datetime", fieldnames, fieldvalues);
}
//=============================================================================
static double
uiComponentDatetimeSerial(const ArrayOf& value)
{
    if (!value.isClassType()) {
        return std::nan("");
    }
    ArrayOf copied(value);
    ArrayOf serial = copied.field("SerialValue");
    if (serial.isEmpty() || !serial.isNumeric()) {
        return std::nan("");
    }
    return ArrayOf(serial).asDoubleScalar();
}
//=============================================================================
static std::wstring
uiTableCellText(const ArrayOf& data, indexType row, indexType column)
{
    indexType rows = data.rows();
    if (data.isCell()) {
        const ArrayOf* elements = static_cast<const ArrayOf*>(data.dataPointer());
        ArrayOf element = elements[row + column * rows];
        if (element.isRowVectorCharacterArray() || element.isScalarStringArray()) {
            return ArrayOf(element).asWideString();
        }
        if (element.isNumeric() && element.isScalar()) {
            wchar_t buffer[64];
            swprintf(buffer, 64, L"%g", ArrayOf(element).asDoubleScalar());
            return buffer;
        }
        if (element.isLogical() && element.isScalar()) {
            return ArrayOf(element).asDoubleScalar() != 0 ? L"true" : L"false";
        }
        return std::wstring();
    }
    if (data.isNumeric() || data.isLogical()) {
        ArrayOf asDouble(data);
        asDouble.promoteType(NLS_DOUBLE);
        const double* values = static_cast<const double*>(asDouble.dataPointer());
        wchar_t buffer[64];
        swprintf(buffer, 64, L"%g", values[row + column * rows]);
        return buffer;
    }
    return std::wstring();
}
//=============================================================================
static nlohmann::json
uiTableColumnWidthsJson(GraphicsObject* object, indexType columns)
{
    nlohmann::json widths = nlohmann::json::array();
    ArrayOf columnWidth = object->findArrayOfProperty(GO_COLUMN_WIDTH_PROPERTY_NAME_STR);
    if (columns == 0 || columnWidth.isEmpty()) {
        return widths;
    }
    if (columnWidth.isNumeric()) {
        ArrayOf numericWidths(columnWidth);
        numericWidths.promoteType(NLS_DOUBLE);
        const double* values = static_cast<const double*>(numericWidths.dataPointer());
        for (indexType c = 0; c < columns; ++c) {
            const indexType source = numericWidths.isScalar() ? 0 : c;
            if (source < numericWidths.elementCount() && std::isfinite(values[source])
                && values[source] > 0) {
                widths.push_back(values[source]);
            } else {
                widths.push_back(nullptr);
            }
        }
        return widths;
    }
    if (!columnWidth.isCell()) {
        return widths;
    }
    const ArrayOf* elements = static_cast<const ArrayOf*>(columnWidth.dataPointer());
    for (indexType c = 0; c < columns; ++c) {
        if (c < columnWidth.elementCount() && elements[c].isNumeric() && elements[c].isScalar()) {
            double value = ArrayOf(elements[c]).asDoubleScalar();
            widths.push_back(std::isfinite(value) && value > 0 ? nlohmann::json(value)
                                                               : nlohmann::json(nullptr));
        } else {
            widths.push_back(nullptr);
        }
    }
    return widths;
}
//=============================================================================
static std::string
uiTableJson(GraphicsObject* object)
{
    ArrayOf data = object->findArrayOfProperty(GO_DATA_PROPERTY_NAME_STR);
    const indexType rows = data.isEmpty() ? 0 : data.rows();
    const indexType columns = data.isEmpty() ? 0 : data.columns();
    nlohmann::json columnsJson = nlohmann::json::array();
    ArrayOf columnName = object->findArrayOfProperty(GO_COLUMN_NAME_PROPERTY_NAME_STR);
    const bool numbered = columnName.isRowVectorCharacterArray()
        && ArrayOf(columnName).asWideString() == L"numbered";
    if (!numbered && columnName.isCell()) {
        const ArrayOf* elements = static_cast<const ArrayOf*>(columnName.dataPointer());
        for (indexType k = 0; k < columnName.elementCount(); ++k) {
            ArrayOf element = elements[k];
            columnsJson.push_back(wstring_to_utf8(
                (element.isRowVectorCharacterArray() || element.isScalarStringArray())
                    ? ArrayOf(element).asWideString()
                    : std::wstring()));
        }
    } else {
        for (indexType c = 0; c < columns; ++c) {
            columnsJson.push_back(std::to_string(static_cast<long long>(c) + 1));
        }
    }
    nlohmann::json rowsJson = nlohmann::json::array();
    for (indexType r = 0; r < rows; ++r) {
        nlohmann::json rowJson = nlohmann::json::array();
        for (indexType c = 0; c < columns; ++c) {
            rowJson.push_back(wstring_to_utf8(uiTableCellText(data, r, c)));
        }
        rowsJson.push_back(rowJson);
    }
    // Per-column editability (ColumnEditable is logical: scalar = all columns).
    ArrayOf columnEditable = object->findArrayOfProperty(GO_COLUMN_EDITABLE_PROPERTY_NAME_STR);
    nlohmann::json editableJson = nlohmann::json::array();
    const bool haveEditable = !columnEditable.isEmpty() && columnEditable.isLogical();
    const logical* editableValues
        = haveEditable ? static_cast<const logical*>(columnEditable.dataPointer()) : nullptr;
    for (indexType c = 0; c < columns; ++c) {
        bool editable = false;
        if (editableValues != nullptr) {
            editable = columnEditable.isScalar()
                ? editableValues[0] != 0
                : (c < columnEditable.elementCount() && editableValues[c] != 0);
        }
        editableJson.push_back(editable);
    }
    return nlohmann::json {
        { "columns", columnsJson }, { "columnWidths", uiTableColumnWidthsJson(object, columns) },
        { "rows", rowsJson }, { "editable", editableJson }
    }.dump();
}
//=============================================================================
static nlohmann::json
uiTreeChildrenJson(GraphicsObject* object)
{
    nlohmann::json nodes = nlohmann::json::array();
    auto* childProperty = dynamic_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (childProperty == nullptr) {
        return nodes;
    }
    for (int64 handle : childProperty->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (child == nullptr || child->getType() != GO_PROPERTY_VALUE_UITREENODE_STR) {
            continue;
        }
        nlohmann::json node;
        node["text"] = wstring_to_utf8(child->findStringProperty(GO_TEXT_PROPERTY_NAME_STR));
        node["handle"] = static_cast<long long>(handle);
        nlohmann::json grandChildren = uiTreeChildrenJson(child);
        if (!grandChildren.empty()) {
            node["children"] = grandChildren;
        }
        nodes.push_back(node);
    }
    return nodes;
}
//=============================================================================
// A standard named icon maps to an emoji (Qt-free). File-path and RGB-matrix
// icons need an image encoder and are skipped.
static std::string
uiNamedIconEmoji(GraphicsObject* object)
{
    GOGenericProperty* iconProperty = object->findProperty(GO_ICON_PROPERTY_NAME_STR, false);
    if (iconProperty == nullptr) {
        return std::string();
    }
    ArrayOf icon = iconProperty->get();
    if (icon.isEmpty() || !(icon.isRowVectorCharacterArray() || icon.isScalarStringArray())) {
        return std::string();
    }
    const std::wstring name = ArrayOf(icon).asWideString();
    if (name == L"question") {
        return "\xE2\x9D\x93"; // question mark
    }
    if (name == L"info") {
        return "\xE2\x84\xB9\xEF\xB8\x8F"; // information
    }
    if (name == L"warning") {
        return "\xE2\x9A\xA0\xEF\xB8\x8F"; // warning
    }
    if (name == L"error") {
        return "\xE2\x9D\x8C"; // cross mark
    }
    if (name == L"success") {
        return "\xE2\x9C\x85"; // check mark
    }
    return std::string();
}
//=============================================================================
void
GOUIComponent::describeForWeb(RenderWeb& renderer, const std::vector<double>& position)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    const std::wstring type = getType();
    GOFigure* figure = parentGoWindow ? parentGoWindow->hostFigure() : nullptr;
    double layoutHeight = renderer.height();
    if (figure != nullptr) {
        std::vector<double> figurePosition
            = figure->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (figurePosition.size() >= 4) {
            layoutHeight = figurePosition[3];
        }
    }
    double scale = uiComponentWebPositionScale(this, figure, renderer.width(), renderer.height());
    std::vector<double> logicalPosition(position);
    if (scale != 1.0) {
        for (double& value : logicalPosition) {
            value *= scale;
        }
    }
    WebUIControl control;
    control.handle = static_cast<long long>(findGraphicsObjectHandle(this));
    control.x = logicalPosition[0];
    control.w = logicalPosition[2];
    control.h = logicalPosition[3];
    control.units = "pixels";
    control.y = layoutHeight - logicalPosition[1] - logicalPosition[3];
    control.enabled = !haveProperty(GO_ENABLE_PROPERTY_NAME_STR)
        || stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    if (haveProperty(GO_TOOLTIP_PROPERTY_NAME_STR)) {
        control.tooltip = wstring_to_utf8(findStringProperty(GO_TOOLTIP_PROPERTY_NAME_STR));
    }
    if (haveProperty(GO_FONT_SIZE_PROPERTY_NAME_STR)) {
        double fontSize = findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
        if (fontSize > 0) {
            control.fontSize = fontSize;
        }
    }
    if (haveProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR)) {
        control.background = uiComponentColorProp(this, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    }
    if (haveProperty(GO_FONT_COLOR_PROPERTY_NAME_STR)) {
        control.foreground = uiComponentColorProp(this, GO_FONT_COLOR_PROPERTY_NAME_STR);
    }
    control.minimum = 0.0;
    control.maximum = 1.0;

    if (type == GO_PROPERTY_VALUE_UIBUTTON_STR || type == GO_PROPERTY_VALUE_UISTATEBUTTON_STR
        || type == GO_PROPERTY_VALUE_UITOGGLEBUTTON_STR) {
        const bool stateButton = type != GO_PROPERTY_VALUE_UIBUTTON_STR;
        control.style = stateButton ? "togglebutton" : "pushbutton";
        control.text = wstring_to_utf8(
            uiComponentJoinLines(findStringVectorProperty(GO_TEXT_PROPERTY_NAME_STR)));
        std::string icon = uiNamedIconEmoji(this);
        if (!icon.empty()) {
            control.text = control.text.empty() ? icon : icon + " " + control.text;
        }
        control.horizontalAlignment
            = wstring_to_utf8(findStringProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR));
        control.verticalAlignment
            = wstring_to_utf8(findStringProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR));
        if (stateButton) {
            control.value
                = uiComponentScalarOf(findProperty(GO_VALUE_PROPERTY_NAME_STR, false), 0.0);
        }
    } else if (type == GO_PROPERTY_VALUE_UICHECKBOX_STR
        || type == GO_PROPERTY_VALUE_UIRADIOBUTTON_STR) {
        control.style = (type == GO_PROPERTY_VALUE_UICHECKBOX_STR) ? "checkbox" : "radiobutton";
        control.text = wstring_to_utf8(
            uiComponentJoinLines(findStringVectorProperty(GO_TEXT_PROPERTY_NAME_STR)));
        control.value = uiComponentScalarOf(findProperty(GO_VALUE_PROPERTY_NAME_STR, false), 0.0);
    } else if (type == GO_PROPERTY_VALUE_UILABEL_STR) {
        control.style = "text";
        control.text = wstring_to_utf8(
            uiComponentJoinLines(findStringVectorProperty(GO_TEXT_PROPERTY_NAME_STR)));
    } else if (type == GO_PROPERTY_VALUE_UIDROPDOWN_STR) {
        control.style = "popupmenu";
        for (const std::wstring& item : findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR)) {
            control.items.push_back(wstring_to_utf8(item));
        }
        control.value
            = uiComponentScalarOf(findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR, false), 1.0);
    } else if (type == GO_PROPERTY_VALUE_UILISTBOX_STR) {
        control.style = "listbox";
        for (const std::wstring& item : findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR)) {
            control.items.push_back(wstring_to_utf8(item));
        }
        // ValueIndex may hold several 1-based indices when Multiselect is on.
        std::vector<int> selected;
        ArrayOf valueIndex = findArrayOfProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR);
        if (!valueIndex.isEmpty() && valueIndex.isNumeric()) {
            ArrayOf asDouble(valueIndex);
            asDouble.promoteType(NLS_DOUBLE);
            const double* values = static_cast<const double*>(asDouble.dataPointer());
            for (indexType k = 0; k < valueIndex.elementCount(); ++k) {
                selected.push_back(static_cast<int>(values[k]));
            }
        }
        control.value = selected.empty() ? 1.0 : static_cast<double>(selected[0]);
        nlohmann::json data;
        data["multiselect"]
            = stringCheck(GO_MULTISELECT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
        data["selected"] = selected;
        control.extraJson = data.dump();
    } else if (type == GO_PROPERTY_VALUE_UIEDITFIELD_STR) {
        control.style = "edit";
        control.text = wstring_to_utf8(
            uiComponentValueAsText(findArrayOfProperty(GO_VALUE_PROPERTY_NAME_STR)));
    } else if (type == GO_PROPERTY_VALUE_UISPINNER_STR
        || type == GO_PROPERTY_VALUE_UINUMERICEDITFIELD_STR) {
        // A numeric field (spinner has steppers); Limits bound it (often
        // +/-Inf, which serialize to null and mean "no bound").
        control.style = "spinner";
        control.text = wstring_to_utf8(
            uiComponentValueAsText(findArrayOfProperty(GO_VALUE_PROPERTY_NAME_STR)));
        std::vector<double> limits(findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
        if (limits.size() == 2) {
            control.minimum = limits[0];
            control.maximum = limits[1];
        }
    } else if (type == GO_PROPERTY_VALUE_UITEXTAREA_STR) {
        control.style = "edit";
        control.text = wstring_to_utf8(
            uiComponentJoinLines(findStringVectorProperty(GO_VALUE_PROPERTY_NAME_STR)));
    } else if (type == GO_PROPERTY_VALUE_UIDISCRETEKNOB_STR) {
        // A discrete knob has no Limits: its dial spans the Items (1..n) and the
        // pointer sits on ValueIndex. Labels are shipped like a switch does.
        control.style = "knob";
        wstringVector items = findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
        for (const std::wstring& item : items) {
            control.items.push_back(wstring_to_utf8(item));
        }
        control.minimum = 1.0;
        control.maximum = items.empty() ? 1.0 : static_cast<double>(items.size());
        control.value
            = uiComponentScalarOf(findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR, false), 1.0);
        control.extraJson = nlohmann::json { { "discrete", true } }.dump();
    } else if (type == GO_PROPERTY_VALUE_UISLIDER_STR || type == GO_PROPERTY_VALUE_UIKNOB_STR
        || type == GO_PROPERTY_VALUE_UIRANGESLIDER_STR) {
        control.style = (type == GO_PROPERTY_VALUE_UIKNOB_STR) ? "knob" : "slider";
        std::vector<double> limits(findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
        if (limits.size() == 2) {
            control.minimum = limits[0];
            control.maximum = limits[1];
        }
        control.value
            = uiComponentScalarOf(findProperty(GO_VALUE_PROPERTY_NAME_STR, false), control.minimum);
        nlohmann::json extra;
        const bool sliderVertical = haveProperty(GO_ORIENTATION_PROPERTY_NAME_STR)
            && stringCheck(GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
        if (type == GO_PROPERTY_VALUE_UIKNOB_STR) {
            extra["discrete"] = false;
            uiComponentAddTickData(this, extra, std::min(control.w, control.h), 8.0);
        } else {
            uiComponentAddTickData(this, extra, sliderVertical ? control.h : control.w, 30.0);
        }
        if (haveProperty(GO_ORIENTATION_PROPERTY_NAME_STR)) {
            extra["vertical"] = sliderVertical;
        }
        if (type == GO_PROPERTY_VALUE_UIRANGESLIDER_STR) {
            // A range slider ships both values (low, high) so the renderer can draw two
            // thumbs. control.value keeps the low value for the flat descriptor.
            std::vector<double> values(findVectorDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
            if (values.size() >= 2) {
                control.value = values[0];
                extra["range"] = { values[0], values[1] };
            }
        }
        control.extraJson = extra.dump();
    } else if (type == GO_PROPERTY_VALUE_UISWITCH_STR
        || type == GO_PROPERTY_VALUE_UIROCKERSWITCH_STR
        || type == GO_PROPERTY_VALUE_UITOGGLESWITCH_STR) {
        control.style = "switch";
        wstringVector items = findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
        for (const std::wstring& item : items) {
            control.items.push_back(wstring_to_utf8(item));
        }
        int index = uiItemsIndexOfValue(findArrayOfProperty(GO_VALUE_PROPERTY_NAME_STR), items,
            findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR));
        control.value = (index >= 2) ? 1.0 : 0.0;
        if (haveProperty(GO_ORIENTATION_PROPERTY_NAME_STR)) {
            control.extraJson = nlohmann::json {
                { "vertical",
                    stringCheck(GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR) }
            }.dump();
        }
    } else if (type == GO_PROPERTY_VALUE_UIDATEPICKER_STR) {
        control.style = "date";
        control.text = wstring_to_utf8(uiComponentSerialToText(
            uiComponentDatetimeSerial(findArrayOfProperty(GO_VALUE_PROPERTY_NAME_STR)),
            findStringProperty(GO_DISPLAY_FORMAT_PROPERTY_NAME_STR)));
    } else if (type == GO_PROPERTY_VALUE_UIHTML_STR) {
        // The page is served separately. Only the stamps travel here, because
        // extraJson is hashed into the display signature and re-serialized on every
        // repaint.
        control.style = "html";
        control.enabled = true;
        // Export paths raster a captioned box: a live page cannot be reproduced on
        // paper, and a blank rectangle would look like a rendering bug.
        control.text = "uihtml";
        auto* html = static_cast<GOUIHTML*>(this);
        nlohmann::json extra;
        extra["source"] = wstring_to_utf8(findStringProperty(GO_HTML_SOURCE_PROPERTY_NAME_STR));
        extra["revision"] = html->dataRevision();
        extra["eventSeq"] = html->eventsQueuedCount();
        control.extraJson = extra.dump();
    } else if (type == GO_PROPERTY_VALUE_UITABLE_STR) {
        control.style = "table";
        control.extraJson = uiTableJson(this);
    } else if (type == GO_PROPERTY_VALUE_UITREE_STR
        || type == GO_PROPERTY_VALUE_UICHECKBOXTREE_STR) {
        control.style = "tree";
        control.extraJson = nlohmann::json { { "nodes", uiTreeChildrenJson(this) } }.dump();
    } else if (type == GO_PROPERTY_VALUE_UIGAUGE_STR || type == GO_PROPERTY_VALUE_UILINEARGAUGE_STR
        || type == GO_PROPERTY_VALUE_UININETYDEGREEGAUGE_STR
        || type == GO_PROPERTY_VALUE_UISEMICIRCULARGAUGE_STR) {
        control.style = "gauge";
        std::vector<double> limits(findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
        if (limits.size() == 2) {
            control.minimum = limits[0];
            control.maximum = limits[1];
        }
        control.value
            = uiComponentScalarOf(findProperty(GO_VALUE_PROPERTY_NAME_STR, false), control.minimum);
        control.enabled = false;
        const char* shape = "circular";
        if (type == GO_PROPERTY_VALUE_UILINEARGAUGE_STR) {
            shape = "linear";
        } else if (type == GO_PROPERTY_VALUE_UININETYDEGREEGAUGE_STR) {
            shape = "ninetydegree";
        } else if (type == GO_PROPERTY_VALUE_UISEMICIRCULARGAUGE_STR) {
            shape = "semicircular";
        }
        nlohmann::json extra;
        extra["shape"] = shape;
        const bool gaugeVertical = haveProperty(GO_ORIENTATION_PROPERTY_NAME_STR)
            && stringCheck(GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
        if (haveProperty(GO_ORIENTATION_PROPERTY_NAME_STR)) {
            extra["vertical"] = gaugeVertical;
        }
        if (type == GO_PROPERTY_VALUE_UILINEARGAUGE_STR) {
            uiComponentAddTickData(this, extra, gaugeVertical ? control.h : control.w, 30.0);
        } else {
            uiComponentAddTickData(this, extra, std::min(control.w, control.h), 18.0);
        }
        control.extraJson = extra.dump();
    } else if (type == GO_PROPERTY_VALUE_UILAMP_STR) {
        control.style = "lamp";
        control.text = std::string();
        control.background = uiComponentColorProp(this, GO_COLOR_PROPERTY_NAME_STR);
        control.enabled = false;
    } else if (type == GO_PROPERTY_VALUE_UIPANEL_STR
        || type == GO_PROPERTY_VALUE_UIBUTTONGROUP_STR) {
        control.style = "panel";
        control.text = wstring_to_utf8(findStringProperty(GO_TITLE_PROPERTY_NAME_STR));
        control.foreground = uiComponentColorProp(this, GO_BORDER_COLOR_PROPERTY_NAME_STR);
        control.enabled = false;
        // A panel hosting axes needs a transparent web overlay. The axes and the panel
        // background are drawn on the canvas below, an opaque div would hide them.
        if (uiContainerHasAxesChild(this)) {
            control.background.clear();
        }
    } else if (type == GO_PROPERTY_VALUE_UITABGROUP_STR) {
        control.style = "tabgroup";
        control.enabled = false;
        nlohmann::json extra;
        extra["tabLocation"]
            = wstring_to_utf8(findStringProperty(GO_TAB_LOCATION_PROPERTY_NAME_STR));
        auto* childProperty
            = dynamic_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        auto* selectedProperty = dynamic_cast<GOGObjectsProperty*>(
            findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
        int64 selectedHandle = (selectedProperty != nullptr && !selectedProperty->data().empty())
            ? selectedProperty->data()[0]
            : -1;
        int selectedIndex = 1;
        int index = 0;
        GraphicsObject* selectedTab = nullptr;
        if (childProperty != nullptr) {
            for (int64 tabHandle : childProperty->data()) {
                GraphicsObject* tab = findGraphicsObject(tabHandle, false);
                if (tab == nullptr || tab->getType() != GO_PROPERTY_VALUE_UITAB_STR) {
                    continue;
                }
                ++index;
                control.items.push_back(
                    wstring_to_utf8(tab->findStringProperty(GO_TITLE_PROPERTY_NAME_STR)));
                if (index == 1) {
                    selectedTab = tab;
                }
                if (tabHandle == selectedHandle) {
                    selectedIndex = index;
                    selectedTab = tab;
                }
            }
        }
        control.value = selectedIndex;
        control.extraJson = extra.dump();
        renderer.addControl(control);
        if (selectedTab != nullptr) {
            // A tab page has no widget in the headless renderer. Size it to the tab
            // group's page area, as native bindings do, before laying out descendants
            // (notably uigridlayout), so Web and Wasm use the same positions.
            auto* tabGroup = dynamic_cast<GOUITabGroup*>(this);
            auto* tabPage = dynamic_cast<GOUITab*>(selectedTab);
            if (tabGroup != nullptr && tabPage != nullptr && logicalPosition.size() == 4) {
                RenderWeb pageFrame(0, 0, logicalPosition[2], logicalPosition[3]);
                std::vector<double> page = tabGroup->contentRect(pageFrame);
                if (page.size() == 4) {
                    tabPage->onPageResized(page[2], page[3]);
                }
            }
            uiDescribeChildrenForWeb(selectedTab, renderer, position);
        }
        return;
    } else if (type == GO_PROPERTY_VALUE_UIGRIDLAYOUT_STR) {
        // Run the grid layout so each child's cell rectangle lands in its Position.
        // updateState creates the Qt widget if needed and calls performLayout, which
        // writes the child geometry back into GO_POSITION.
        updateState();
        uiDescribeChildrenForWeb(this, renderer, position);
        return;
    } else {
        // Every App Designer component has a web mapping. Anything else emits nothing
        // instead of rasterizing a Qt snapshot.
        return;
    }
    renderer.addControl(control);

    // Children are at container-relative coordinates that RenderWeb never sees (Qt
    // composites nested widgets itself). Recurse here, offsetting each child into
    // the parent's space, so the subtree flattens into the display list.
    if (type == GO_PROPERTY_VALUE_UIPANEL_STR || type == GO_PROPERTY_VALUE_UIBUTTONGROUP_STR) {
        uiDescribeChildrenForWeb(this, renderer, position);
    }
}
//=============================================================================
bool
GOUIComponent::applyWebAction(double value, const std::string& text)
{
    const std::wstring type = getType();
    int64 thisHandle = findGraphicsObjectHandle(this);
    if (thisHandle != -1) {
        setCurrentObject(thisHandle);
    }
    auto setProp = [&](const std::wstring& name, const ArrayOf& newValue) {
        GOGenericProperty* property = findProperty(name, false);
        if (property == nullptr) {
            return;
        }
        if (auto* arrayProperty = dynamic_cast<GOArrayOfProperty*>(property)) {
            arrayProperty->data(newValue);
        } else {
            property->set(newValue);
        }
        property->setModified(true);
    };
    auto getProp = [&](const std::wstring& name) -> ArrayOf {
        GOGenericProperty* property = findProperty(name, false);
        return property != nullptr ? property->get() : ArrayOf();
    };
    auto emitValueChanged = [&](const ArrayOf& now, const ArrayOf& previous) {
        ArrayOfVector fieldValues;
        fieldValues.push_back(now);
        fieldValues.push_back(previous);
        emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData",
            L"ValueChanged", { L"Value", L"PreviousValue" }, fieldValues);
    };

    if (type == GO_PROPERTY_VALUE_UIBUTTON_STR) {
        emitCallbackEvent(
            GO_BUTTON_PUSHED_FCN_PROPERTY_NAME_STR, L"ButtonPushedData", L"ButtonPushed");
        return true;
    }
    if (auto* radio = dynamic_cast<GOUIRadioButton*>(this)) {
        radio->onUserClick();
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UISTATEBUTTON_STR || type == GO_PROPERTY_VALUE_UICHECKBOX_STR) {
        ArrayOf previous = getProp(GO_VALUE_PROPERTY_NAME_STR);
        bool now = (value != 0.0);
        setProp(GO_VALUE_PROPERTY_NAME_STR, ArrayOf::logicalScalar(now));
        emitValueChanged(ArrayOf::logicalScalar(now), previous);
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UIHTML_STR) {
        // One envelope carries both directions of the page contract:
        //   {"kind":"data","value":<json>} and {"kind":"event","name":..,"data":..}
        //
        // The web layer calls this from its own server thread. Decoding builds ArrayOf
        // values and queues a callback holding them, and ArrayOf reference counts are
        // not atomic. A page drag delivers events continuously, so the work is handed
        // over instead of relying on the race being rare. The envelope travels as text
        // and is decoded once, on the thread that consumes it.
        MainThreadTaskQueue::getInstance()->postToInterpreterThread([thisHandle, envelope = text](
                                                                        Evaluator*) {
            auto* component = dynamic_cast<GOUIHTML*>(findGraphicsObject(thisHandle, false));
            if (component == nullptr) {
                return;
            }
            nlohmann::json message = nlohmann::json::parse(envelope, nullptr, false);
            if (message.is_discarded() || !message.is_object()) {
                return;
            }
            const std::string kind = message.value("kind", "");
            if (kind == "data") {
                component->onPageDataChanged(
                    message.contains("value") ? message.at("value").dump() : std::string("null"));
            } else if (kind == "event") {
                component->onPageEvent(message.value("name", ""),
                    message.contains("data") ? message.at("data").dump() : std::string("null"));
            }
        });
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UILISTBOX_STR) {
        // text is a JSON array of 1-based indices (one entry for single-select).
        // Rebuild Value/ValueIndex as the Qt path does.
        wstringVector items = findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
        ArrayOf itemsData = findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR);
        ArrayOf previousValue = getProp(GO_VALUE_PROPERTY_NAME_STR);
        ArrayOf previousValueIndex = getProp(GO_VALUE_INDEX_PROPERTY_NAME_STR);
        std::vector<int> indices;
        nlohmann::json selection = nlohmann::json::parse(text, nullptr, false);
        if (selection.is_array()) {
            for (const auto& entry : selection) {
                if (entry.is_number()) {
                    indices.push_back(entry.get<int>());
                }
            }
        } else if (value >= 1.0) {
            indices.push_back(static_cast<int>(value + 0.5));
        }
        ArrayOf newValue;
        ArrayOf newValueIndex;
        if (indices.empty()) {
            newValue = ArrayOf::emptyCell(Dimensions(1, 0));
            newValueIndex = ArrayOf::empty(0, 0);
        } else if (indices.size() == 1) {
            newValue = uiItemsValueAtIndex(indices[0], items, itemsData);
            newValueIndex = ArrayOf::doubleScalar(indices[0]);
        } else {
            ArrayOf* elements
                = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, indices.size()));
            for (size_t k = 0; k < indices.size(); ++k) {
                elements[k] = uiItemsValueAtIndex(indices[k], items, itemsData);
            }
            newValue = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, indices.size()), elements);
            std::vector<double> asDouble(indices.begin(), indices.end());
            newValueIndex = ArrayOf::doubleRowVector(asDouble);
        }
        setProp(GO_VALUE_PROPERTY_NAME_STR, newValue);
        setProp(GO_VALUE_INDEX_PROPERTY_NAME_STR, newValueIndex);
        ArrayOfVector fieldValues;
        fieldValues.push_back(newValue);
        fieldValues.push_back(previousValue);
        fieldValues.push_back(newValueIndex);
        fieldValues.push_back(previousValueIndex);
        emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData",
            L"ValueChanged", { L"Value", L"PreviousValue", L"ValueIndex", L"PreviousValueIndex" },
            fieldValues);
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UIDROPDOWN_STR || type == GO_PROPERTY_VALUE_UISWITCH_STR) {
        int index = (type == GO_PROPERTY_VALUE_UISWITCH_STR) ? ((value != 0.0) ? 2 : 1)
                                                             : (int)(value + 0.5);
        wstringVector items = findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
        ArrayOf itemsData = findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR);
        ArrayOf previous = getProp(GO_VALUE_PROPERTY_NAME_STR);
        ArrayOf now = uiItemsValueAtIndex(index, items, itemsData);
        setProp(GO_VALUE_PROPERTY_NAME_STR, now);
        setProp(GO_VALUE_INDEX_PROPERTY_NAME_STR, ArrayOf::doubleScalar((double)index));
        emitValueChanged(now, previous);
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UIEDITFIELD_STR || type == GO_PROPERTY_VALUE_UISPINNER_STR
        || type == GO_PROPERTY_VALUE_UINUMERICEDITFIELD_STR) {
        ArrayOf previous = getProp(GO_VALUE_PROPERTY_NAME_STR);
        const bool numeric = type != GO_PROPERTY_VALUE_UIEDITFIELD_STR || previous.isNumeric();
        ArrayOf now;
        if (numeric) {
            double parsed = 0.0;
            try {
                parsed = std::stod(text);
            } catch (...) {
                parsed = 0.0;
            }
            // Clamp to Limits (ignoring non-finite bounds), as the Qt field does.
            std::vector<double> limits(findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
            if (limits.size() == 2) {
                if (std::isfinite(limits[0]) && parsed < limits[0]) {
                    parsed = limits[0];
                }
                if (std::isfinite(limits[1]) && parsed > limits[1]) {
                    parsed = limits[1];
                }
            }
            now = ArrayOf::doubleScalar(parsed);
        } else {
            now = ArrayOf::characterArray(utf8_to_wstring(text));
        }
        setProp(GO_VALUE_PROPERTY_NAME_STR, now);
        emitValueChanged(now, previous);
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UITEXTAREA_STR) {
        ArrayOf previous = getProp(GO_VALUE_PROPERTY_NAME_STR);
        auto* stringProperty
            = static_cast<GOStringVectorProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
        if (stringProperty != nullptr) {
            stringProperty->data(uiComponentSplitLines(utf8_to_wstring(text)));
            stringProperty->setModified(true);
        }
        emitValueChanged(getProp(GO_VALUE_PROPERTY_NAME_STR), previous);
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UISLIDER_STR || type == GO_PROPERTY_VALUE_UIKNOB_STR) {
        ArrayOf previous = getProp(GO_VALUE_PROPERTY_NAME_STR);
        setProp(GO_VALUE_PROPERTY_NAME_STR, ArrayOf::doubleScalar(value));
        emitValueChanged(ArrayOf::doubleScalar(value), previous);
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UIDATEPICKER_STR) {
        ArrayOf previous = getProp(GO_VALUE_PROPERTY_NAME_STR);
        ArrayOf now = uiComponentMakeDatetime(uiComponentIsoToSerial(text));
        setProp(GO_VALUE_PROPERTY_NAME_STR, now);
        emitValueChanged(now, previous);
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UITREE_STR) {
        // text is the clicked uitreenode handle. The native handler reads the Qt
        // widget, which the web path lacks, so set SelectedNodes and fire
        // SelectionChangedFcn here.
        int64 nodeHandle = -1;
        try {
            nodeHandle = static_cast<int64>(std::stoll(text));
        } catch (...) {
            nodeHandle = -1;
        }
        if (nodeHandle != -1) {
            auto* selectedNodes = dynamic_cast<GOGObjectsProperty*>(
                findProperty(GO_SELECTED_NODES_PROPERTY_NAME_STR, false));
            if (selectedNodes != nullptr) {
                std::vector<int64> previousSelection = selectedNodes->data();
                std::vector<int64> newSelection { nodeHandle };
                selectedNodes->data(newSelection);
                selectedNodes->setModified(true);
                ArrayOfVector fieldValues;
                fieldValues.push_back(graphicsObjectVector(newSelection));
                fieldValues.push_back(graphicsObjectVector(previousSelection));
                emitCallbackEvent(GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR,
                    L"SelectedNodesChangedData", L"SelectionChanged",
                    { L"SelectedNodes", L"PreviousSelectedNodes" }, fieldValues);
            }
        }
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UITABLE_STR) {
        // text is a JSON {row, col, value} (0-based cell). Reuse the table's edit path
        // so Data and CellEditCallback behave as in Qt.
        auto* table = dynamic_cast<GOUITable*>(this);
        if (table != nullptr) {
            nlohmann::json edit = nlohmann::json::parse(text, nullptr, false);
            if (!edit.is_discarded() && edit.contains("row") && edit.contains("col")) {
                const int row = edit.value("row", -1);
                const int col = edit.value("col", -1);
                const std::string cellValue = edit.value("value", std::string());
                if (row >= 0 && col >= 0) {
                    table->onUserCellEdited(row, col, utf8_to_wstring(cellValue));
                }
            }
        }
        return true;
    }
    if (type == GO_PROPERTY_VALUE_UITABGROUP_STR) {
        const int index = static_cast<int>(value + 0.5);
        auto* childProperty
            = dynamic_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (childProperty != nullptr) {
            std::vector<int64> tabs;
            for (int64 tabHandle : childProperty->data()) {
                GraphicsObject* tab = findGraphicsObject(tabHandle, false);
                if (tab != nullptr && tab->getType() == GO_PROPERTY_VALUE_UITAB_STR) {
                    tabs.push_back(tabHandle);
                }
            }
            if (index >= 1 && index <= static_cast<int>(tabs.size())) {
                auto* selectedProperty = dynamic_cast<GOGObjectsProperty*>(
                    findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
                if (selectedProperty != nullptr) {
                    selectedProperty->data(std::vector<int64> { tabs[index - 1] });
                    selectedProperty->setModified(true);
                }
                emitCallbackEvent(GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR,
                    L"SelectionChangedData", L"SelectionChanged");
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
void
GOUIComponent::registerUIComponentCommonProperties(
    bool withFonts, bool withBackgroundColor, bool withVisibleChildren)
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(
        new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, true, withVisibleChildren);
    registerProperty(new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    registerProperty(new GOLayoutOptionsProperty, GO_LAYOUT_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_INNER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR, false);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_ENABLE_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TOOLTIP_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    if (withFonts) {
        registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
        registerProperty(new GOScalarProperty(1.0, (double)std::numeric_limits<int>::max()),
            GO_FONT_SIZE_PROPERTY_NAME_STR);
        registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
        registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
        registerProperty(new GOColorProperty, GO_FONT_COLOR_PROPERTY_NAME_STR);
    }
    if (withBackgroundColor) {
        registerProperty(new GOColorProperty, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIComponent::setupUIComponentCommonDefaults(
    double x, double y, double w, double h, bool withFonts, bool withBackgroundColor)
{
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, x, y, w, h);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, x, y, w, h);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, x, y, w, h);
    setStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIXELS_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TOOLTIP_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, L"");
    if (withFonts) {
        setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
        setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 12);
        setStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
        setStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
        setThreeVectorDefault(GO_FONT_COLOR_PROPERTY_NAME_STR, 0.129411764705882, 0.129411764705882,
            0.129411764705882);
    }
    if (withBackgroundColor) {
        setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 0.96078431372549,
            0.96078431372549, 0.96078431372549);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
