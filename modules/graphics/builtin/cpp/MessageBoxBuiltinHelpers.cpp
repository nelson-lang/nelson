//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MessageBoxBuiltinHelpers.hpp"
#include "WebDesktopBridge.hpp"
#include "__image__Builtin.hpp"
#include "__text__Builtin.hpp"
#include "axesBuiltin.hpp"
#include "closeBuiltin.hpp"
#include "dialogBuiltin.hpp"
#include "findobjBuiltin.hpp"
#include "graphics_object_setBuiltin.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "StringHelpers.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "uicontrolBuiltin.hpp"
#include <cmath>
#include <vector>
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static std::wstring
normalizedMode(const std::wstring& mode);
static ArrayOf
createMessageFigure(
    const std::wstring& message, const std::wstring& title, const std::wstring& mode, bool hasIcon);
static ArrayOf
addMessageIcon(const ArrayOf& figure, const std::wstring& icon);
static void
addMessageText(const ArrayOf& figure, const std::wstring& message, bool hasIcon);
static void
addMessageButton(const ArrayOf& figure, bool hasIcon);
static void
closeExistingMessage(const std::wstring& title);
static bool
hasMessageIcon(const std::wstring& icon);
static std::wstring
messageIconText(const std::wstring& icon);
static std::vector<double>
messageIconColor(const std::wstring& icon);
static ArrayOf
createIconAxes(const ArrayOf& figure, bool reverseY);
static void
setObjectProperty(const ArrayOf& handle, const std::wstring& property, const ArrayOf& value);
static void
setKeyPressCallback(const ArrayOf& figure);
static void
clearCurrentObject(const ArrayOf& figure);
static void
setReferenceMessageGeometry(const ArrayOf& figure, bool hasIcon);
static ArrayOf
createNamedFunctionHandle(const std::string& name);
static void
drawErrorIcon(const ArrayOf& axes);
static void
drawWarningIcon(const ArrayOf& axes);
static void
drawInfoIcon(const ArrayOf& axes, const std::wstring& text);
static void
drawIconImage(const ArrayOf& axes, const ArrayOf& cdata);
static ArrayOf
createIconCData(const std::wstring& icon);
static void
setIconPixel(double* data, indexType rows, indexType cols, indexType row, indexType col,
    const std::vector<double>& color);
static std::vector<double>
backgroundColor();
//=============================================================================
ArrayOf
MessageBox(const std::wstring& message, const std::wstring& title, const std::wstring& icon,
    const std::wstring& mode)
{
    // Web GUI: show a message modal in the browser instead of a docked figure.
    // Returns an empty handle.
    if (WebDesktopBridge::getInstance()->showWebMessageBox(title, message, icon)) {
        return ArrayOf::empty();
    }
    std::wstring modeValue = normalizedMode(mode);
    if (modeValue == L"replace" && !title.empty()) {
        closeExistingMessage(title);
    }
    bool withIcon = hasMessageIcon(icon);
    ArrayOf figure = createMessageFigure(message, title, modeValue, withIcon);
    addMessageButton(figure, withIcon);
    ArrayOf iconAxes;
    if (withIcon) {
        iconAxes = addMessageIcon(figure, icon);
    }
    addMessageText(figure, message, withIcon);
    if (!iconAxes.isEmpty()) {
        setObjectProperty(figure, L"CurrentAxes", iconAxes);
    }
    clearCurrentObject(figure);
    setReferenceMessageGeometry(figure, withIcon);
    return figure;
}
//=============================================================================
wstringVector
getSupportedMessageBoxIcons()
{
    return { L"none", L"help", L"warn", L"warning", L"error", L"question", L"custom" };
}
//=============================================================================
wstringVector
getSupportedMessageBoxModes()
{
    return { L"modal", L"non-modal", L"nonmodal", L"replace", L"on" };
}
//=============================================================================
static std::wstring
normalizedMode(const std::wstring& mode)
{
    if (mode == L"nonmodal") {
        return L"non-modal";
    }
    if (mode == L"on") {
        return L"replace";
    }
    return mode.empty() ? L"non-modal" : mode;
}
//=============================================================================
static ArrayOf
createMessageFigure(
    const std::wstring& message, const std::wstring& title, const std::wstring& mode, bool hasIcon)
{
    ArrayOfVector args;
    args << ArrayOf::characterArray(L"Name");
    args << ArrayOf::characterArray(title);
    args << ArrayOf::characterArray(L"Tag");
    args << ArrayOf::characterArray(L"Msgbox_" + title);
    args << ArrayOf::characterArray(L"Resize");
    args << ArrayOf::characterArray(L"off");
    args << ArrayOf::characterArray(L"ToolBar");
    args << ArrayOf::characterArray(L"none");
    args << ArrayOf::characterArray(L"Units");
    args << ArrayOf::characterArray(L"points");
    args << ArrayOf::characterArray(L"WindowStyle");
    args << ArrayOf::characterArray(mode == L"modal" ? L"modal" : L"normal");
    args << ArrayOf::characterArray(L"Position");
    args << ArrayOf::doubleRowVector(hasIcon ? std::vector<double> { 477.6, 391.8, 196.8, 61.2 }
                                             : std::vector<double> { 501, 396.6, 150, 52.8 });
    ArrayOfVector result = dialogBuiltin(1, args);
    if (result.empty()) {
        return ArrayOf::empty();
    }
    setKeyPressCallback(result[0]);
    return result[0];
}
//=============================================================================
static ArrayOf
addMessageIcon(const ArrayOf& figure, const std::wstring& icon)
{
    bool warning
        = StringHelpers::iequals(icon, L"warn") || StringHelpers::iequals(icon, L"warning");
    ArrayOf axes = createIconAxes(figure, warning);
    if (StringHelpers::iequals(icon, L"error")) {
        drawErrorIcon(axes);
    } else if (warning) {
        drawWarningIcon(axes);
    } else {
        drawInfoIcon(axes, messageIconText(icon));
    }
    return axes;
}
//=============================================================================
static void
addMessageText(const ArrayOf& figure, const std::wstring& message, bool hasIcon)
{
    ArrayOfVector args;
    args << figure;
    args << ArrayOf::characterArray(L"Visible");
    args << ArrayOf::characterArray(L"off");
    args << ArrayOf::characterArray(L"Units");
    args << ArrayOf::characterArray(L"normalized");
    args << ArrayOf::characterArray(L"Position");
    args << ArrayOf::doubleRowVector({ 0, 0, 1, 1 });
    args << ArrayOf::characterArray(L"XTick");
    args << ArrayOf::doubleRowVector({});
    args << ArrayOf::characterArray(L"YTick");
    args << ArrayOf::doubleRowVector({});
    ArrayOfVector axesResult = axesBuiltin(1, args);
    if (axesResult.empty()) {
        return;
    }

    ArrayOfVector textArgs;
    textArgs << ArrayOf::characterArray(GO_PARENT_PROPERTY_NAME_STR);
    textArgs << axesResult[0];
    textArgs << ArrayOf::characterArray(GO_TAG_PROPERTY_NAME_STR);
    textArgs << ArrayOf::characterArray(L"MessageBox");
    textArgs << ArrayOf::characterArray(GO_UNITS_PROPERTY_NAME_STR);
    textArgs << ArrayOf::characterArray(GO_PROPERTY_VALUE_POINTS_STR);
    textArgs << ArrayOf::characterArray(GO_POSITION_PROPERTY_NAME_STR);
    textArgs << ArrayOf::doubleRowVector(
        hasIcon ? std::vector<double> { 38, 34.5, 0 } : std::vector<double> { 7, 31, 0 });
    textArgs << ArrayOf::characterArray(GO_STRING_PROPERTY_NAME_STR);
    textArgs << ArrayOf::characterArray(message);
    textArgs << ArrayOf::characterArray(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    textArgs << ArrayOf::characterArray(L"left");
    textArgs << ArrayOf::characterArray(GO_FONT_SIZE_PROPERTY_NAME_STR);
    textArgs << ArrayOf::doubleScalar(8);
    textArgs << ArrayOf::characterArray(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    textArgs << ArrayOf::characterArray(L"normal");
    textArgs << ArrayOf::characterArray(GO_FONT_NAME_PROPERTY_NAME_STR);
    textArgs << ArrayOf::characterArray(L"MS Sans Serif");
    textArgs << ArrayOf::characterArray(GO_FONT_UNITS_PROPERTY_NAME_STR);
    textArgs << ArrayOf::characterArray(GO_PROPERTY_VALUE_POINTS_STR);
    textArgs << ArrayOf::characterArray(GO_COLOR_PROPERTY_NAME_STR);
    textArgs << ArrayOf::doubleRowVector(
        { 0.129411764705882, 0.129411764705882, 0.129411764705882 });
    __text__Builtin(1, textArgs);
}
//=============================================================================
static void
addMessageButton(const ArrayOf& figure, bool hasIcon)
{
    ArrayOfVector args;
    args << ArrayOf::characterArray(L"Parent");
    args << figure;
    args << ArrayOf::characterArray(L"Style");
    args << ArrayOf::characterArray(L"pushbutton");
    args << ArrayOf::characterArray(L"String");
    args << ArrayOf::characterArray(L"OK");
    args << ArrayOf::characterArray(L"Tag");
    args << ArrayOf::characterArray(L"OKButton");
    args << ArrayOf::characterArray(L"Units");
    args << ArrayOf::characterArray(L"points");
    args << ArrayOf::characterArray(L"HorizontalAlignment");
    args << ArrayOf::characterArray(L"center");
    args << ArrayOf::characterArray(L"Position");
    args << ArrayOf::doubleRowVector(
        hasIcon ? std::vector<double> { 78.5, 7, 40, 17 } : std::vector<double> { 55, 7, 40, 17 });
    args << ArrayOf::characterArray(L"Callback");
    args << ArrayOf::characterArray(L"close(gcbf)");
    uicontrolBuiltin(1, args);
}
//=============================================================================
static void
closeExistingMessage(const std::wstring& title)
{
    ArrayOfVector findArgs;
    findArgs << ArrayOf::characterArray(L"Type");
    findArgs << ArrayOf::characterArray(L"figure");
    findArgs << ArrayOf::characterArray(L"Name");
    findArgs << ArrayOf::characterArray(title);
    findArgs << ArrayOf::characterArray(L"Tag");
    findArgs << ArrayOf::characterArray(L"Msgbox_" + title);
    ArrayOfVector found = findobjBuiltin(1, findArgs);
    if (found.empty() || !found[0].isGraphicsObject() || found[0].isEmpty()) {
        return;
    }
    ArrayOfVector closeArgs;
    closeArgs << found[0];
    closeBuiltin(0, closeArgs);
}
//=============================================================================
static bool
hasMessageIcon(const std::wstring& icon)
{
    return !icon.empty() && !StringHelpers::iequals(icon, L"none")
        && !StringHelpers::iequals(icon, L"custom");
}
//=============================================================================
static std::wstring
messageIconText(const std::wstring& icon)
{
    if (StringHelpers::iequals(icon, L"warn") || StringHelpers::iequals(icon, L"warning")) {
        return L"!";
    }
    if (StringHelpers::iequals(icon, L"question")) {
        return L"?";
    }
    return StringHelpers::iequals(icon, L"error") ? L"X" : L"i";
}
//=============================================================================
static std::vector<double>
messageIconColor(const std::wstring& icon)
{
    if (StringHelpers::iequals(icon, L"error")) {
        return { 0.75, 0.10, 0.10 };
    }
    if (StringHelpers::iequals(icon, L"warn") || StringHelpers::iequals(icon, L"warning")) {
        return { 0.85, 0.45, 0.00 };
    }
    if (StringHelpers::iequals(icon, L"question")) {
        return { 0.10, 0.30, 0.75 };
    }
    return { 0.10, 0.45, 0.70 };
}
//=============================================================================
static ArrayOf
createIconAxes(const ArrayOf& figure, bool reverseY)
{
    ArrayOfVector args;
    args << figure;
    args << ArrayOf::characterArray(L"Tag");
    args << ArrayOf::characterArray(L"IconAxes");
    args << ArrayOf::characterArray(L"Box");
    args << ArrayOf::characterArray(L"off");
    args << ArrayOf::characterArray(L"Visible");
    args << ArrayOf::characterArray(L"off");
    args << ArrayOf::characterArray(L"XTick");
    args << ArrayOf::doubleRowVector({});
    args << ArrayOf::characterArray(L"YTick");
    args << ArrayOf::doubleRowVector({});
    args << ArrayOf::characterArray(L"XColor");
    args << ArrayOf::doubleRowVector({ 1, 1, 1 });
    args << ArrayOf::characterArray(L"YColor");
    args << ArrayOf::doubleRowVector({ 1, 1, 1 });
    args << ArrayOf::characterArray(L"XLim");
    args << ArrayOf::doubleRowVector({ 0, 1 });
    args << ArrayOf::characterArray(L"YLim");
    args << ArrayOf::doubleRowVector({ 0, 1 });
    if (reverseY) {
        args << ArrayOf::characterArray(L"YDir");
        args << ArrayOf::characterArray(L"reverse");
    }
    args << ArrayOf::characterArray(L"Units");
    args << ArrayOf::characterArray(L"normalized");
    args << ArrayOf::characterArray(L"Position");
    args << ArrayOf::doubleRowVector({ 7.0 / 196.8, 31.0 / 61.2, 24.0 / 196.8, 24.0 / 61.2 });
    ArrayOfVector result = axesBuiltin(1, args);
    if (result.empty()) {
        return ArrayOf::empty();
    }
    return result[0];
}
//=============================================================================
static void
setObjectProperty(const ArrayOf& handle, const std::wstring& property, const ArrayOf& value)
{
    ArrayOfVector args;
    args << handle;
    args << ArrayOf::characterArray(property);
    args << value;
    graphics_object_setBuiltin(0, args);
}
//=============================================================================
static void
setKeyPressCallback(const ArrayOf& figure)
{
    setObjectProperty(figure, L"KeyPressFcn", createNamedFunctionHandle("doKeyPress"));
}
//=============================================================================
static void
clearCurrentObject(const ArrayOf& figure)
{
    setObjectProperty(figure, L"CurrentObject", ArrayOf::empty(0, 0));
}
//=============================================================================
static void
setReferenceMessageGeometry(const ArrayOf& figure, bool hasIcon)
{
    if (!figure.isGraphicsObject()) {
        return;
    }
    GOFigure* fig = findGOFigure(figure.asGraphicsObjectScalar());
    if (!fig) {
        return;
    }
    if (hasIcon) {
        fig->setExposedPositionOverride({ 477.6, 390.6, 196.8, 61.8 },
            { 477.6, 390.6, 196.8, 61.8 }, { 477.6, 390.6, 196.8, 61.8 },
            { 7.02866598666667, 13.7599161766667, 6.94266666666667, 2.18016666666667 });
    } else {
        fig->setExposedPositionOverride({ 501, 396.6, 150, 52.8 }, { 501, 396.6, 150, 52.8 },
            { 501, 396.6, 150, 52.8 },
            { 7.85416598666667, 13.9186661766667, 5.29166666666667, 1.86266666666667 });
    }
    fig->setTwoVectorDefault(GO_CURRENT_POINT_PROPERTY_NAME_STR, -0.75, -0.75);
}
//=============================================================================
static ArrayOf
createNamedFunctionHandle(const std::string& name)
{
    function_handle callback;
    callback.anonymousHandle = nullptr;
    callback.anonymousHandle
        = reinterpret_cast<nelson_handle*>(new AnonymousMacroFunctionDef(name));
    return ArrayOf::functionHandle(callback);
}
//=============================================================================
static void
drawErrorIcon(const ArrayOf& axes)
{
    drawIconImage(axes, createIconCData(L"error"));
}
//=============================================================================
static void
drawWarningIcon(const ArrayOf& axes)
{
    drawIconImage(axes, createIconCData(L"warning"));
}
//=============================================================================
static void
drawInfoIcon(const ArrayOf& axes, const std::wstring& text)
{
    drawIconImage(axes, createIconCData(text == L"?" ? L"question" : L"help"));
}
//=============================================================================
static void
drawIconImage(const ArrayOf& axes, const ArrayOf& cdata)
{
    ArrayOfVector args;
    args << ArrayOf::characterArray(L"Parent");
    args << axes;
    args << ArrayOf::characterArray(GO_C_DATA_PROPERTY_NAME_STR);
    args << cdata;
    args << ArrayOf::characterArray(GO_X_DATA_PROPERTY_NAME_STR);
    args << ArrayOf::doubleRowVector({ 0, 1 });
    args << ArrayOf::characterArray(GO_Y_DATA_PROPERTY_NAME_STR);
    args << ArrayOf::doubleRowVector({ 0, 1 });
    __image__Builtin(1, args);
}
//=============================================================================
static ArrayOf
createIconCData(const std::wstring& icon)
{
    const indexType rows = 32;
    const indexType cols = 32;
    Dimensions dims({ rows, cols, 3 });
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount()));
    std::vector<double> bg = backgroundColor();
    std::fill(data, data + dims.elementCount(), bg[0]);
    for (indexType row = 0; row < rows; ++row) {
        for (indexType col = 0; col < cols; ++col) {
            setIconPixel(data, rows, cols, row, col, bg);
        }
    }
    std::vector<double> mainColor = messageIconColor(icon);
    bool warning = StringHelpers::iequals(icon, L"warning");
    for (indexType row = 0; row < rows; ++row) {
        double rawY = ((double)row + 0.5 - 16.0) / 16.0;
        double y = warning ? -rawY : rawY;
        for (indexType col = 0; col < cols; ++col) {
            double x = ((double)col + 0.5 - 16.0) / 16.0;
            bool fill = false;
            if (warning) {
                fill = y > -0.70 && y < 0.78 && std::abs(x) < (0.88 - y) * 0.58;
            } else {
                fill = std::abs(x) <= 0.72 && std::abs(y) <= 0.72
                    && std::abs(x) + std::abs(y) <= 1.05;
            }
            if (fill) {
                setIconPixel(data, rows, cols, row, col, mainColor);
            }
            bool whiteMark = false;
            if (warning) {
                whiteMark = (std::abs(x) < 0.12 && y > -0.20 && y < 0.42)
                    || (x * x + (y + 0.46) * (y + 0.46) < 0.015);
            } else if (StringHelpers::iequals(icon, L"error")) {
                whiteMark = (std::abs(x) < 0.12 && y > -0.42 && y < 0.20)
                    || (x * x + (y - 0.46) * (y - 0.46) < 0.015);
            } else {
                whiteMark = (std::abs(x) < 0.09 && y > -0.10 && y < 0.46)
                    || (x * x + (y + 0.42) * (y + 0.42) < 0.014);
            }
            if (whiteMark && fill) {
                setIconPixel(data, rows, cols, row, col, { 1, 1, 1 });
            }
        }
    }
    return ArrayOf(NLS_DOUBLE, dims, data);
}
//=============================================================================
static void
setIconPixel(double* data, indexType rows, indexType cols, indexType row, indexType col,
    const std::vector<double>& color)
{
    indexType plane = rows * cols;
    indexType offset = row + rows * col;
    data[offset] = color[0];
    data[offset + plane] = color[1];
    data[offset + 2 * plane] = color[2];
}
//=============================================================================
static std::vector<double>
backgroundColor()
{
    return { 0.9608, 0.9608, 0.9608 };
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
