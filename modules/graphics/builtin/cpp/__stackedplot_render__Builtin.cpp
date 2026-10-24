//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__stackedplot_render__Builtin.hpp"
#include <memory>
#include <vector>
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOAxis.hpp"
#include "GOConstructorHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOLineSeries.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOText.hpp"
#include "GraphicsObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "axesBuiltin.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static ArrayOf
cellElement(const ArrayOf& cellArray, indexType index);
static ArrayOf
graphicsObjectColumnVector(const std::vector<int64>& handles);
static ArrayOf
lineHandlesCell(const std::vector<std::vector<int64>>& panels);
static GOAxis*
axisFromHandle(go_handle handle);
static go_handle
createPanelAxes(const ArrayOf& parent, const std::vector<double>& position);
static std::vector<double>
matrixRow(const ArrayOf& matrix, indexType rowIndex);
static ArrayOf
lineColor(GOAxis* axis, indexType lineIndex);
static go_handle
createLine(go_handle axisHandle, const ArrayOf& xData, const ArrayOf& yData, const ArrayOf& color);
static go_handle
createPanelLabel(go_handle axisHandle, const std::wstring& label);
static go_handle
createXLabel(go_handle axisHandle, const std::wstring& label);
static go_handle
createTitle(go_handle axisHandle, const std::wstring& title);
static void
setPanelAxisProperties(GOAxis* axis, go_handle axisHandle, bool isBottom,
    const std::wstring& xLabel, bool isTop, const std::wstring& title);
static ArrayOf
emptyDoubleArray();
static ArrayOf
emptyCellArray();
static ArrayOf
charArray(const std::wstring& value);
static ArrayOf
doubleRowVector(const std::vector<double>& values);
static ArrayOf
scalar(double value);
//=============================================================================
ArrayOfVector
__stackedplot_render__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 7);
    nargoutcheck(nLhs, 0, 3);

    const ArrayOf& parent = argIn[0];
    const ArrayOf& axesPositions = argIn[1];
    const ArrayOf& xValues = argIn[2];
    const ArrayOf& values = argIn[3];
    const ArrayOf& names = argIn[4];
    std::wstring xLabel = argIn[5].asWideString();
    std::wstring title = argIn[6].asWideString();

    if (!parent.isEmpty() && !parent.isGraphicsObject()) {
        Error("Nelson:graphics:invalidFigureHandle", "Invalid figure handle.");
    }
    if (!axesPositions.isDoubleType(true) || axesPositions.dimensionLength(1) != 4) {
        Error("Nelson:stackedplot:invalidAxesPositions", "Invalid axes positions.");
    }
    if (!xValues.isCell() || !values.isCell() || !names.isCellArrayOfCharacterVectors()) {
        Error("Nelson:stackedplot:invalidData", "Invalid stackedplot data.");
    }

    indexType panelCount = values.elementCount();
    if (panelCount == 0 || xValues.elementCount() != panelCount
        || names.elementCount() != panelCount || axesPositions.dimensionLength(0) != panelCount) {
        Error("Nelson:stackedplot:invalidData", "Invalid stackedplot data.");
    }

    std::vector<int64> axesHandles;
    std::vector<int64> labelHandles;
    std::vector<std::vector<int64>> lineHandlesByPanel;
    axesHandles.reserve(panelCount);
    labelHandles.reserve(panelCount);
    lineHandlesByPanel.reserve(panelCount);

    for (indexType k = 0; k < panelCount; ++k) {
        go_handle axisHandle = createPanelAxes(parent, matrixRow(axesPositions, k));
        GOAxis* axis = axisFromHandle(axisHandle);
        GOFigure* fig = axis->getParentFigure();
        if (fig) {
            fig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, axisHandle);
        }

        ArrayOf panelX = cellElement(xValues, k);
        ArrayOf panelY = cellElement(values, k);
        if (!panelX.isCell() || !panelY.isCell()
            || panelX.elementCount() != panelY.elementCount()) {
            Error("Nelson:stackedplot:invalidData", "Invalid stackedplot data.");
        }

        std::vector<int64> panelLines;
        panelLines.reserve(panelY.elementCount());
        for (indexType j = 0; j < panelY.elementCount(); ++j) {
            panelLines.push_back(createLine(
                axisHandle, cellElement(panelX, j), cellElement(panelY, j), lineColor(axis, j)));
        }

        bool isBottom = (k + 1 == panelCount);
        bool isTop = (k == 0);
        setPanelAxisProperties(axis, axisHandle, isBottom, xLabel, isTop, title);

        std::wstring label = cellElement(names, k).asWideString();
        labelHandles.push_back(createPanelLabel(axisHandle, label));

        axesHandles.push_back(axisHandle);
        lineHandlesByPanel.push_back(panelLines);
    }

    ArrayOfVector retval;
    if (nLhs >= 1) {
        retval << graphicsObjectColumnVector(axesHandles);
    }
    if (nLhs >= 2) {
        retval << graphicsObjectColumnVector(labelHandles);
    }
    if (nLhs >= 3) {
        retval << lineHandlesCell(lineHandlesByPanel);
    }
    return retval;
}
//=============================================================================
ArrayOf
cellElement(const ArrayOf& cellArray, indexType index)
{
    if (!cellArray.isCell() || index >= cellArray.elementCount()) {
        Error("Nelson:stackedplot:invalidCellArray", "Invalid cell array.");
    }
    ArrayOf* elements = static_cast<ArrayOf*>(const_cast<void*>(cellArray.dataPointer()));
    return elements[index];
}
//=============================================================================
ArrayOf
graphicsObjectColumnVector(const std::vector<int64>& handles)
{
    nelson_handle* data = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_GO_HANDLE, handles.size(), stringVector(), false));
    for (size_t k = 0; k < handles.size(); ++k) {
        data[k] = static_cast<nelson_handle>(handles[k]);
    }
    return ArrayOf(NLS_GO_HANDLE, Dimensions(handles.size(), 1), data);
}
//=============================================================================
ArrayOf
lineHandlesCell(const std::vector<std::vector<int64>>& panels)
{
    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements = std::make_unique<ArrayOf[]>(panels.size());
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    for (size_t k = 0; k < panels.size(); ++k) {
        elements[k] = graphicsObjectColumnVector(panels[k]);
    }
    ArrayOf result(NLS_CELL_ARRAY, Dimensions(panels.size(), 1), elements.get());
    elements.release();
    return result;
}
//=============================================================================
GOAxis*
axisFromHandle(go_handle handle)
{
    GraphicsObject* object = findGraphicsObject(handle);
    if (!object || !object->isType(GO_PROPERTY_VALUE_AXES_STR)) {
        Error("Nelson:stackedplot:invalidAxesHandle", "Invalid axes handle.");
    }
    return static_cast<GOAxis*>(object);
}
//=============================================================================
go_handle
createPanelAxes(const ArrayOf& parent, const std::vector<double>& position)
{
    ArrayOfVector args;
    if (!parent.isEmpty()) {
        args << parent;
    }
    args << charArray(GO_POSITION_PROPERTY_NAME_STR) << doubleRowVector(position);
    ArrayOfVector created = axesBuiltin(1, args);
    return created[0].asGraphicsObjectScalar();
}
//=============================================================================
std::vector<double>
matrixRow(const ArrayOf& matrix, indexType rowIndex)
{
    const double* data = static_cast<const double*>(matrix.dataPointer());
    indexType rows = matrix.dimensionLength(0);
    std::vector<double> row(4);
    for (indexType col = 0; col < 4; ++col) {
        row[col] = data[rowIndex + col * rows];
    }
    return row;
}
//=============================================================================
ArrayOf
lineColor(GOAxis* axis, indexType lineIndex)
{
    std::vector<double> colorOrder
        = axis->findVectorDoubleProperty(GO_COLOR_ORDER_PROPERTY_NAME_STR);
    if (colorOrder.size() < 3) {
        return doubleRowVector({ 0., 0., 0. });
    }
    size_t colorCount = colorOrder.size() / 3;
    size_t index = static_cast<size_t>(lineIndex) % colorCount;
    return doubleRowVector(
        { colorOrder[index * 3], colorOrder[index * 3 + 1], colorOrder[index * 3 + 2] });
}
//=============================================================================
go_handle
createLine(go_handle axisHandle, const ArrayOf& xData, const ArrayOf& yData, const ArrayOf& color)
{
    ArrayOfVector args;
    args << charArray(GO_PARENT_PROPERTY_NAME_STR) << ArrayOf::graphicsObject(axisHandle)
         << charArray(GO_X_DATA_PROPERTY_NAME_STR) << xData
         << charArray(GO_Y_DATA_PROPERTY_NAME_STR) << yData << charArray(GO_COLOR_PROPERTY_NAME_STR)
         << color;
    return GOCommonConstructorHelper(new GOLineSeries, args);
}
//=============================================================================
go_handle
createPanelLabel(go_handle axisHandle, const std::wstring& label)
{
    GOAxis* axis = axisFromHandle(axisHandle);
    ArrayOf visibility = axis->findProperty(GO_VISIBLE_PROPERTY_NAME_STR)->get();
    ArrayOfVector args;
    args << charArray(GO_PARENT_PROPERTY_NAME_STR) << ArrayOf::graphicsObject(axisHandle)
         << charArray(GO_POSITION_PROPERTY_NAME_STR) << doubleRowVector({ -0.035, 0.5, 0. })
         << charArray(GO_STRING_PROPERTY_NAME_STR) << charArray(label)
         << charArray(GO_UNITS_PROPERTY_NAME_STR) << charArray(GO_PROPERTY_VALUE_NORMALIZED_STR)
         << charArray(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR)
         << charArray(GO_PROPERTY_VALUE_RIGHT_STR)
         << charArray(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)
         << charArray(GO_PROPERTY_VALUE_MIDDLE_STR) << charArray(GO_ROTATION_PROPERTY_NAME_STR)
         << scalar(0.) << charArray(GO_VISIBLE_PROPERTY_NAME_STR) << visibility;
    return GOCommonConstructorHelper(new GOText, args);
}
//=============================================================================
go_handle
createXLabel(go_handle axisHandle, const std::wstring& label)
{
    GOAxis* axis = axisFromHandle(axisHandle);
    ArrayOf visibility = axis->findProperty(GO_VISIBLE_PROPERTY_NAME_STR)->get();
    ArrayOfVector args;
    args << charArray(GO_STRING_PROPERTY_NAME_STR) << charArray(label)
         << charArray(GO_PARENT_PROPERTY_NAME_STR) << ArrayOf::graphicsObject(axisHandle)
         << charArray(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR)
         << charArray(GO_PROPERTY_VALUE_CENTER_STR)
         << charArray(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)
         << charArray(GO_PROPERTY_VALUE_MIDDLE_STR) << charArray(GO_AUTO_PARENT_PROPERTY_NAME_STR)
         << charArray(GO_PROPERTY_VALUE_OFF_STR) << charArray(GO_VISIBLE_PROPERTY_NAME_STR)
         << visibility;
    return GOCommonConstructorHelper(new GOText, args);
}
//=============================================================================
go_handle
createTitle(go_handle axisHandle, const std::wstring& title)
{
    GOAxis* axis = axisFromHandle(axisHandle);
    ArrayOf visibility = axis->findProperty(GO_VISIBLE_PROPERTY_NAME_STR)->get();
    ArrayOfVector args;
    args << charArray(GO_STRING_PROPERTY_NAME_STR) << charArray(title)
         << charArray(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR)
         << charArray(GO_PROPERTY_VALUE_CENTER_STR)
         << charArray(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)
         << charArray(GO_PROPERTY_VALUE_TOP_STR) << charArray(GO_POSITION_PROPERTY_NAME_STR)
         << doubleRowVector({ 0.5, 1., 0. }) << charArray(GO_PARENT_PROPERTY_NAME_STR)
         << ArrayOf::graphicsObject(axisHandle) << charArray(GO_AUTO_PARENT_PROPERTY_NAME_STR)
         << charArray(GO_PROPERTY_VALUE_OFF_STR) << charArray(GO_FONT_WEIGHT_PROPERTY_NAME_STR)
         << charArray(GO_PROPERTY_VALUE_BOLD_STR) << charArray(GO_MARGIN_PROPERTY_NAME_STR)
         << scalar(3.) << charArray(GO_VISIBLE_PROPERTY_NAME_STR) << visibility;
    return GOCommonConstructorHelper(new GOText, args);
}
//=============================================================================
void
setPanelAxisProperties(GOAxis* axis, go_handle axisHandle, bool isBottom,
    const std::wstring& xLabel, bool isTop, const std::wstring& title)
{
    axis->findProperty(GO_BOX_PROPERTY_NAME_STR)->set(charArray(GO_PROPERTY_VALUE_OFF_STR));
    if (!isBottom) {
        axis->findProperty(GO_X_TICK_PROPERTY_NAME_STR)->set(emptyDoubleArray());
        axis->findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR)->set(emptyCellArray());
    } else {
        axis->setGoProperty(GO_X_LABEL_PROPERTY_NAME_STR, createXLabel(axisHandle, xLabel));
    }
    if (isTop) {
        axis->setGoProperty(GO_TITLE_PROPERTY_NAME_STR, createTitle(axisHandle, title));
    }
    axis->updateState();
}
//=============================================================================
ArrayOf
emptyDoubleArray()
{
    return ArrayOf::empty(0, 0);
}
//=============================================================================
ArrayOf
emptyCellArray()
{
    return ArrayOf::emptyCell(Dimensions(0, 0));
}
//=============================================================================
ArrayOf
charArray(const std::wstring& value)
{
    return ArrayOf::characterArray(value);
}
//=============================================================================
ArrayOf
doubleRowVector(const std::vector<double>& values)
{
    return ArrayOf::doubleRowVector(values);
}
//=============================================================================
ArrayOf
scalar(double value)
{
    return ArrayOf::doubleScalar(value);
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
