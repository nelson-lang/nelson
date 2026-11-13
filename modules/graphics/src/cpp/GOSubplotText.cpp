//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "GOSubplotText.hpp"
#include "GOFigure.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
subplotTextAutoFontSize(GOFigure* figure)
{
    // Figure widths and heights (pixels) where the reference adds half a point, from 11 to 15.
    static const double widthSteps[] = { 177.5, 227.5, 282.5, 332.5, 402.5, 492.5, 582.5, 672.5 };
    static const double heightSteps[] = { 142.5, 177.5, 212.5, 252.5, 302.5, 367.5, 437.5, 502.5 };
    if (!figure) {
        return 15.0;
    }
    std::vector<double> position = figure->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    double width = position.size() >= 4 && position[2] > 0 ? position[2] : figure->getWidth();
    double height = position.size() >= 4 && position[3] > 0 ? position[3] : figure->getHeight();
    int widthCount = 0;
    int heightCount = 0;
    for (size_t k = 0; k < sizeof(widthSteps) / sizeof(widthSteps[0]); ++k) {
        widthCount += width >= widthSteps[k] ? 1 : 0;
        heightCount += height >= heightSteps[k] ? 1 : 0;
    }
    return 11.0 + 0.5 * std::min(widthCount, heightCount);
}
//=============================================================================
GOSubplotText::GOSubplotText() : GOText()
{
    registerProperty(new GOAutoManualProperty, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
    setRestrictedStringDefault(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 13);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
}
//=============================================================================
std::wstring
GOSubplotText::getType()
{
    return GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR;
}
//=============================================================================
wstringVector
GOSubplotText::getVisibleFieldnames()
{
    int64 parentHandle = findGoProperty(GO_PARENT_PROPERTY_NAME_STR);
    GraphicsObject* parent = findGraphicsObject(parentHandle, false);
    if (parent && parent->isType(GO_PROPERTY_VALUE_TILED_LAYOUT_STR)) {
        return {
            GO_STRING_PROPERTY_NAME_STR,
            GO_FONT_NAME_PROPERTY_NAME_STR,
            GO_FONT_SIZE_PROPERTY_NAME_STR,
            GO_FONT_ANGLE_PROPERTY_NAME_STR,
            GO_FONT_WEIGHT_PROPERTY_NAME_STR,
            GO_ROTATION_PROPERTY_NAME_STR,
            GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR,
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR,
            GO_INTERPRETER_PROPERTY_NAME_STR,
            GO_COLOR_PROPERTY_NAME_STR,
            GO_CHILDREN_PROPERTY_NAME_STR,
            GO_PARENT_PROPERTY_NAME_STR,
            GO_VISIBLE_PROPERTY_NAME_STR,
            GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR,
        };
    }
    return {
        GO_BACKGROUND_COLOR_PROPERTY_NAME_STR,
        GO_COLOR_PROPERTY_NAME_STR,
        GO_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_FONT_ANGLE_PROPERTY_NAME_STR,
        GO_FONT_NAME_PROPERTY_NAME_STR,
        GO_FONT_SIZE_PROPERTY_NAME_STR,
        GO_FONT_WEIGHT_PROPERTY_NAME_STR,
        GO_FONT_UNITS_PROPERTY_NAME_STR,
        GO_INTERPRETER_PROPERTY_NAME_STR,
        GO_LINE_STYLE_PROPERTY_NAME_STR,
        GO_LINE_WIDTH_PROPERTY_NAME_STR,
        GO_MARGIN_PROPERTY_NAME_STR,
        GO_STRING_PROPERTY_NAME_STR,
        GO_FONT_SIZE_MODE_PROPERTY_NAME_STR,
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR,
        GO_CHILDREN_PROPERTY_NAME_STR,
        GO_PARENT_PROPERTY_NAME_STR,
        GO_VISIBLE_PROPERTY_NAME_STR,
        GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR,
        GO_SELECTED_PROPERTY_NAME_STR,
        GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR,
        GO_HIT_TEST_PROPERTY_NAME_STR,
        GO_PICKABLE_PARTS_PROPERTY_NAME_STR,
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
    };
}
//=============================================================================
} // namespace Nelson
//=============================================================================
