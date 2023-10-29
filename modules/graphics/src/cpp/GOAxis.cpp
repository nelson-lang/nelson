//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#include <cmath>
#include <QtWidgets/QApplication>
#include <QtGui/QPainter>
#include "nlsBuildConfig.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAxis.hpp"
#include "GOAxisHelpers.hpp"
#include "GOPositionProperty.hpp"
#include "GOAlignVertProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOInOutProperty.hpp"
#include "GOProjectionModeProperty.hpp"
#include "GONextPlotModeProperty.hpp"
#include "GOLinearLogProperty.hpp"
#include "GOLineStyleOrderProperty.hpp"
#include "GONormalReverseProperty.hpp"
#include "GOLeftRightProperty.hpp"
#include "GOTopBottomProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOColorVectorProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOVectorSixDoubleProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorThreeDoubleProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "GOList.hpp"
#include "GOFigure.hpp"
#include "GOText.hpp"
#include "GOHelpers.hpp"
#include "RenderQt.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "TexToUnicode.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
const int MAX_MINI_TICK_COUNT = 10;
//=============================================================================
std::wstring
GOAxis::getType()
{
    return L"axes";
}
//=============================================================================
void
GOAxis::constructProperties()
{
    registerProperty(new GOTwoVectorProperty, GO_A_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_A_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_AMBIENT_LIGHT_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BOX_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_CAMERA_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_CAMERA_TARGET_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_C_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_C_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorVectorProperty, GO_COLOR_ORDER_PROPERTY_NAME_STR);
    registerProperty(
        new GOScalarPositiveIntegerValueProperty, GO_COLOR_ORDER_INDEX_PROPERTY_NAME_STR);
    registerProperty(new GOSixVectorProperty, GO_DATA_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_GRID_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOTopBottomProperty, GO_LAYER_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleOrderProperty, GO_LINE_STYLE_ORDER_PROPERTY_NAME_STR);
    registerProperty(
        new GOScalarPositiveIntegerValueProperty, GO_LINE_STYLE_ORDER_INDEX_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_MINOR_GRID_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GONextPlotModeProperty, GO_NEXT_PLOT_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_POSITION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOProjectionModeProperty, GO_PROJECTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_TEXT_HEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOInOutProperty, GO_TICK_DIR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_TICK_DIR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_TICK_LENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_TIGHT_INSET_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOTopBottomProperty, GO_X_AXIS_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOLeftRightProperty, GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_X_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_Y_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_Z_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GONormalReverseProperty, GO_X_DIR_PROPERTY_NAME_STR);
    registerProperty(new GONormalReverseProperty, GO_Y_DIR_PROPERTY_NAME_STR);
    registerProperty(new GONormalReverseProperty, GO_Z_DIR_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_X_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_Y_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_Z_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_X_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_Y_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_Z_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_X_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_Y_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_Z_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Z_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_X_MINOR_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_Y_MINOR_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_Z_MINOR_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOLinearLogProperty, GO_X_SCALE_PROPERTY_NAME_STR);
    registerProperty(new GOLinearLogProperty, GO_Y_SCALE_PROPERTY_NAME_STR);
    registerProperty(new GOLinearLogProperty, GO_Z_SCALE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_X_TICK_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Y_TICK_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Z_TICK_PROPERTY_NAME_STR);
    registerProperty(new GOStringVector, GO_X_TICK_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOStringVector, GO_Y_TICK_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOStringVector, GO_Z_TICK_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_TICK_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_TICK_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Z_TICK_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOColorVectorProperty, GO_COLOR_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_ALPHA_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOTextInterpreterProperty, GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
GOAxis::GOAxis()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
void
GOAxis::setupDefaults()
{
    setRestrictedStringDefault(GO_A_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_BOX_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setThreeVectorDefault(GO_CAMERA_POSITION_PROPERTY_NAME_STR, 0, 0, 1);
    setRestrictedStringDefault(
        GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_CAMERA_TARGET_PROPERTY_NAME_STR, 0, 0, 0);
    setRestrictedStringDefault(GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR, 0, 1, 0);
    setRestrictedStringDefault(
        GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_C_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    std::vector<double> colorsOrder;
    colorsOrder.push_back(0.0000);
    colorsOrder.push_back(0.4470);
    colorsOrder.push_back(0.7410);

    colorsOrder.push_back(0.8500);
    colorsOrder.push_back(0.3250);
    colorsOrder.push_back(0.0980);

    colorsOrder.push_back(0.9290);
    colorsOrder.push_back(0.6940);
    colorsOrder.push_back(0.1250);

    colorsOrder.push_back(0.4940);
    colorsOrder.push_back(0.1840);
    colorsOrder.push_back(0.5560);

    colorsOrder.push_back(0.4660);
    colorsOrder.push_back(0.6740);
    colorsOrder.push_back(0.1880);

    colorsOrder.push_back(0.3010);
    colorsOrder.push_back(0.7450);
    colorsOrder.push_back(0.9330);

    colorsOrder.push_back(0.6350);
    colorsOrder.push_back(0.0780);
    colorsOrder.push_back(0.1840);
    GOVectorProperty* hp
        = static_cast<GOVectorProperty*>(findProperty(GO_COLOR_ORDER_PROPERTY_NAME_STR));
    hp->data(colorsOrder);
    setScalarPositiveIntegerValueDefault(GO_COLOR_ORDER_INDEX_PROPERTY_NAME_STR, 1.0);
    setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, 1, 1, 1);
    setRestrictedStringDefault(
        GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"helvetica");
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10);
    setRestrictedStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
    setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_GRID_LINE_STYLE_PROPERTY_NAME_STR, L":");
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_LAYER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTTOM_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 1.0);
    wstringVector defaultLineStyleOrder;
    defaultLineStyleOrder.push_back(L"-");
    setRestrictedStringSetDefault(GO_LINE_STYLE_ORDER_PROPERTY_NAME_STR, defaultLineStyleOrder);
    setScalarPositiveIntegerValueDefault(GO_LINE_STYLE_ORDER_INDEX_PROPERTY_NAME_STR, 1.0);
    setRestrictedStringDefault(GO_MINOR_GRID_LINE_STYLE_PROPERTY_NAME_STR, L":");
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
    setRestrictedStringDefault(GO_NEXT_PLOT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REPLACE_STR);
    setThreeVectorDefault(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, 1, 1, 1);
    setRestrictedStringDefault(
        GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setRestrictedStringDefault(GO_POSITION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_PROJECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ORTHOGRAPHIC_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_TICK_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_IN_STR);
    setRestrictedStringDefault(GO_TICK_DIR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwoVectorDefault(GO_TICK_LENGTH_PROPERTY_NAME_STR, 0.01, 0.025);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_X_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTTOM_STR);
    setRestrictedStringDefault(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
    setThreeVectorDefault(GO_X_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setThreeVectorDefault(GO_Y_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setThreeVectorDefault(GO_Z_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setRestrictedStringDefault(GO_X_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_Z_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_X_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_Y_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_Z_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, 0, 1);
    setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, 0, 1);
    setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Z_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_X_MINOR_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_Y_MINOR_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_Z_MINOR_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_X_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setRestrictedStringDefault(GO_Y_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setRestrictedStringDefault(GO_Z_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setRestrictedStringDefault(GO_X_TICK_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_TICK_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Z_TICK_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwoVectorDefault(GO_C_LIM_PROPERTY_NAME_STR, 0, 1);
    setStringDefault(GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);

    loadParulaColorMap();
    updateAxisFont();
}
//=============================================================================
GOAxis::~GOAxis() { }
//=============================================================================
void
GOAxis::loadParulaColorMap()
{
    std::vector<double> cmap = { 0.2422, 0.1504, 0.6603, 0.2444, 0.1534, 0.6728, 0.2464, 0.1569,
        0.6847, 0.2484, 0.1607, 0.6961, 0.2503, 0.1648, 0.7071, 0.2522, 0.1689, 0.7179, 0.254,
        0.1732, 0.7286, 0.2558, 0.1773, 0.7393, 0.2576, 0.1814, 0.7501, 0.2594, 0.1854, 0.761,
        0.2611, 0.1893, 0.7719, 0.2628, 0.1932, 0.7828, 0.2645, 0.1972, 0.7937, 0.2661, 0.2011,
        0.8043, 0.2676, 0.2052, 0.8148, 0.2691, 0.2094, 0.8249, 0.2704, 0.2138, 0.8346, 0.2717,
        0.2184, 0.8439, 0.2729, 0.2231, 0.8528, 0.274, 0.228, 0.8612, 0.2749, 0.233, 0.8692, 0.2758,
        0.2382, 0.8767, 0.2766, 0.2435, 0.884, 0.2774, 0.2489, 0.8908, 0.2781, 0.2543, 0.8973,
        0.2788, 0.2598, 0.9035, 0.2794, 0.2653, 0.9094, 0.2798, 0.2708, 0.915, 0.2802, 0.2764,
        0.9204, 0.2806, 0.2819, 0.9255, 0.2809, 0.2875, 0.9305, 0.2811, 0.293, 0.9352, 0.2813,
        0.2985, 0.9397, 0.2814, 0.304, 0.9441, 0.2814, 0.3095, 0.9483, 0.2813, 0.315, 0.9524,
        0.2811, 0.3204, 0.9563, 0.2809, 0.3259, 0.96, 0.2807, 0.3313, 0.9636, 0.2803, 0.3367, 0.967,
        0.2798, 0.3421, 0.9702, 0.2791, 0.3475, 0.9733, 0.2784, 0.3529, 0.9763, 0.2776, 0.3583,
        0.9791, 0.2766, 0.3638, 0.9817, 0.2754, 0.3693, 0.984, 0.2741, 0.3748, 0.9862, 0.2726,
        0.3804, 0.9881, 0.271, 0.386, 0.9898, 0.2691, 0.3916, 0.9912, 0.267, 0.3973, 0.9924, 0.2647,
        0.403, 0.9935, 0.2621, 0.4088, 0.9946, 0.2591, 0.4145, 0.9955, 0.2556, 0.4203, 0.9965,
        0.2517, 0.4261, 0.9974, 0.2473, 0.4319, 0.9983, 0.2424, 0.4378, 0.9991, 0.2369, 0.4437,
        0.9996, 0.2311, 0.4497, 0.9995, 0.225, 0.4559, 0.9985, 0.2189, 0.462, 0.9968, 0.2128,
        0.4682, 0.9948, 0.2066, 0.4743, 0.9926, 0.2006, 0.4803, 0.9906, 0.195, 0.4861, 0.9887,
        0.1903, 0.4919, 0.9867, 0.1869, 0.4975, 0.9844, 0.1847, 0.503, 0.9819, 0.1831, 0.5084,
        0.9793, 0.1818, 0.5138, 0.9766, 0.1806, 0.5191, 0.9738, 0.1795, 0.5244, 0.9709, 0.1785,
        0.5296, 0.9677, 0.1778, 0.5349, 0.9641, 0.1773, 0.5401, 0.9602, 0.1768, 0.5452, 0.956,
        0.1764, 0.5504, 0.9516, 0.1755, 0.5554, 0.9473, 0.174, 0.5605, 0.9432, 0.1716, 0.5655,
        0.9393, 0.1686, 0.5705, 0.9357, 0.1649, 0.5755, 0.9323, 0.161, 0.5805, 0.9289, 0.1573,
        0.5854, 0.9254, 0.154, 0.5902, 0.9218, 0.1513, 0.595, 0.9182, 0.1492, 0.5997, 0.9147,
        0.1475, 0.6043, 0.9113, 0.1461, 0.6089, 0.908, 0.1446, 0.6135, 0.905, 0.1429, 0.618, 0.9022,
        0.1408, 0.6226, 0.8998, 0.1383, 0.6272, 0.8975, 0.1354, 0.6317, 0.8953, 0.1321, 0.6363,
        0.8932, 0.1288, 0.6408, 0.891, 0.1253, 0.6453, 0.8887, 0.1219, 0.6497, 0.8862, 0.1185,
        0.6541, 0.8834, 0.1152, 0.6584, 0.8804, 0.1119, 0.6627, 0.877, 0.1085, 0.6669, 0.8734,
        0.1048, 0.671, 0.8695, 0.1009, 0.675, 0.8653, 0.0964, 0.6789, 0.8609, 0.0914, 0.6828,
        0.8562, 0.0855, 0.6865, 0.8513, 0.0789, 0.6902, 0.8462, 0.0713, 0.6938, 0.8409, 0.0628,
        0.6972, 0.8355, 0.0535, 0.7006, 0.8299, 0.0433, 0.7039, 0.8242, 0.0328, 0.7071, 0.8183,
        0.0234, 0.7103, 0.8124, 0.0155, 0.7133, 0.8064, 0.0091, 0.7163, 0.8003, 0.0046, 0.7192,
        0.7941, 0.0019, 0.722, 0.7878, 0.0009, 0.7248, 0.7815, 0.0018, 0.7275, 0.7752, 0.0046,
        0.7301, 0.7688, 0.0094, 0.7327, 0.7623, 0.0162, 0.7352, 0.7558, 0.0253, 0.7376, 0.7492,
        0.0369, 0.74, 0.7426, 0.0504, 0.7423, 0.7359, 0.0638, 0.7446, 0.7292, 0.077, 0.7468, 0.7224,
        0.0899, 0.7489, 0.7156, 0.1023, 0.751, 0.7088, 0.1141, 0.7531, 0.7019, 0.1252, 0.7552,
        0.695, 0.1354, 0.7572, 0.6881, 0.1448, 0.7593, 0.6812, 0.1532, 0.7614, 0.6741, 0.1609,
        0.7635, 0.6671, 0.1678, 0.7656, 0.6599, 0.1741, 0.7678, 0.6527, 0.1799, 0.7699, 0.6454,
        0.1853, 0.7721, 0.6379, 0.1905, 0.7743, 0.6303, 0.1954, 0.7765, 0.6225, 0.2003, 0.7787,
        0.6146, 0.2061, 0.7808, 0.6065, 0.2118, 0.7828, 0.5983, 0.2178, 0.7849, 0.5899, 0.2244,
        0.7869, 0.5813, 0.2318, 0.7887, 0.5725, 0.2401, 0.7905, 0.5636, 0.2491, 0.7922, 0.5546,
        0.2589, 0.7937, 0.5454, 0.2695, 0.7951, 0.536, 0.2809, 0.7964, 0.5266, 0.2929, 0.7975,
        0.517, 0.3052, 0.7985, 0.5074, 0.3176, 0.7994, 0.4975, 0.3301, 0.8002, 0.4876, 0.3424,
        0.8009, 0.4774, 0.3548, 0.8016, 0.4669, 0.3671, 0.8021, 0.4563, 0.3795, 0.8026, 0.4454,
        0.3921, 0.8029, 0.4344, 0.405, 0.8031, 0.4233, 0.4184, 0.803, 0.4122, 0.4322, 0.8028,
        0.4013, 0.4463, 0.8024, 0.3904, 0.4608, 0.8018, 0.3797, 0.4753, 0.8011, 0.3691, 0.4899,
        0.8002, 0.3586, 0.5044, 0.7993, 0.348, 0.5187, 0.7982, 0.3374, 0.5329, 0.797, 0.3267, 0.547,
        0.7957, 0.3159, 0.5609, 0.7943, 0.305, 0.5748, 0.7929, 0.2941, 0.5886, 0.7913, 0.2833,
        0.6024, 0.7896, 0.2726, 0.6161, 0.7878, 0.2622, 0.6297, 0.7859, 0.2521, 0.6433, 0.7839,
        0.2423, 0.6567, 0.7818, 0.2329, 0.6701, 0.7796, 0.2239, 0.6833, 0.7773, 0.2155, 0.6963,
        0.775, 0.2075, 0.7091, 0.7727, 0.1998, 0.7218, 0.7703, 0.1924, 0.7344, 0.7679, 0.1852,
        0.7468, 0.7654, 0.1782, 0.759, 0.7629, 0.1717, 0.771, 0.7604, 0.1658, 0.7829, 0.7579,
        0.1608, 0.7945, 0.7554, 0.157, 0.806, 0.7529, 0.1546, 0.8172, 0.7505, 0.1535, 0.8281,
        0.7481, 0.1536, 0.8389, 0.7457, 0.1546, 0.8495, 0.7435, 0.1564, 0.86, 0.7413, 0.1587,
        0.8703, 0.7392, 0.1615, 0.8804, 0.7372, 0.165, 0.8903, 0.7353, 0.1695, 0.9, 0.7336, 0.1749,
        0.9093, 0.7321, 0.1815, 0.9184, 0.7308, 0.189, 0.9272, 0.7298, 0.1973, 0.9357, 0.729,
        0.2061, 0.944, 0.7285, 0.2151, 0.9523, 0.7284, 0.2237, 0.9606, 0.7285, 0.2312, 0.9689,
        0.7292, 0.2373, 0.977, 0.7304, 0.2418, 0.9842, 0.733, 0.2446, 0.99, 0.7365, 0.2429, 0.9946,
        0.7407, 0.2394, 0.9966, 0.7458, 0.2351, 0.9971, 0.7513, 0.2309, 0.9972, 0.7569, 0.2267,
        0.9971, 0.7626, 0.2224, 0.9969, 0.7683, 0.2181, 0.9966, 0.774, 0.2138, 0.9962, 0.7798,
        0.2095, 0.9957, 0.7856, 0.2053, 0.9949, 0.7915, 0.2012, 0.9938, 0.7974, 0.1974, 0.9923,
        0.8034, 0.1939, 0.9906, 0.8095, 0.1906, 0.9885, 0.8156, 0.1875, 0.9861, 0.8218, 0.1846,
        0.9835, 0.828, 0.1817, 0.9807, 0.8342, 0.1787, 0.9778, 0.8404, 0.1757, 0.9748, 0.8467,
        0.1726, 0.972, 0.8529, 0.1695, 0.9694, 0.8591, 0.1665, 0.9671, 0.8654, 0.1636, 0.9651,
        0.8716, 0.1608, 0.9634, 0.8778, 0.1582, 0.9619, 0.884, 0.1557, 0.9608, 0.8902, 0.1532,
        0.9601, 0.8963, 0.1507, 0.9596, 0.9023, 0.148, 0.9595, 0.9084, 0.145, 0.9597, 0.9143,
        0.1418, 0.9601, 0.9203, 0.1382, 0.9608, 0.9262, 0.1344, 0.9618, 0.932, 0.1304, 0.9629,
        0.9379, 0.1261, 0.9642, 0.9437, 0.1216, 0.9657, 0.9494, 0.1168, 0.9674, 0.9552, 0.1116,
        0.9692, 0.9609, 0.1061, 0.9711, 0.9667, 0.1001, 0.973, 0.9724, 0.0938, 0.9749, 0.9782,
        0.0872, 0.9769, 0.9839, 0.0805 };
    GOColorVectorProperty* hcv
        = static_cast<GOColorVectorProperty*>(findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR));
    hcv->data(cmap);
    cmap.clear();
    cmap.push_back(1.0);
    GOVectorProperty* hv
        = static_cast<GOVectorProperty*>(findProperty(GO_ALPHA_MAP_PROPERTY_NAME_STR));
    hv->data(cmap);
}
//=============================================================================
int
GOAxis::getTickCount(
    RenderInterface& gc, double x1, double y1, double z1, double x2, double y2, double z2)
{
    double u1, v1, u2, v2;
    gc.toPixels(x1, y1, z1, u1, v1);
    gc.toPixels(x2, y2, z2, u2, v2);
    double axlen;
    axlen = sqrt((u2 - u1) * (u2 - u1) + (v2 - v1) * (v2 - v1));
    int numtics = (int)(std::max(2.0, axlen / 25.0));
    return numtics;
}
//=============================================================================
void
GOAxis::recalculateTicks()
{
    QImage img(1, 1, QImage::Format_RGB32);
    QPainter pnt(&img);
    GOFigure* fig = getParentFigure();
    unsigned width = fig->getWidth();
    unsigned height = fig->getHeight();
    RenderQt gc(&pnt, 0, 0, width, height);
    setupProjection(gc);
    std::vector<double> limits(getAxisLimits());
    std::vector<double> xticks;
    std::vector<std::wstring> xlabels;
    std::vector<double> yticks;
    std::vector<std::wstring> ylabels;
    std::vector<double> zticks;
    std::vector<std::wstring> zlabels;
    int xcnt, ycnt, zcnt;
    xcnt = getTickCount(gc, limits[0], x1pos[1], x1pos[2], limits[1], x1pos[1], x1pos[2]);
    ycnt = getTickCount(gc, y1pos[0], limits[2], y1pos[2], y1pos[0], limits[3], y1pos[2]);
    zcnt = getTickCount(gc, z1pos[0], z1pos[1], limits[4], z1pos[0], z1pos[1], limits[5]);
    double xStart, xStop;
    double yStart, yStop;
    double zStart, zStop;
    GOTwoVectorProperty* tp = nullptr;
    GOLinearLogProperty* lp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    GOTextInterpreterProperty* textInterpreterProperty = static_cast<GOTextInterpreterProperty*>(
        findProperty(GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR));
    if (isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR)) {
        formatAxisAuto(textInterpreterProperty->getAsEnum(), limits[0], limits[1], xcnt,
            lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), xStart, xStop, xticks, xlabels);
        tp = static_cast<GOTwoVectorProperty*>(findProperty(GO_X_LIM_PROPERTY_NAME_STR));
        std::vector<double> lims;
        if (lp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
            lims.push_back(xStart);
            lims.push_back(xStop);
        } else {
            lims.push_back(pow(10.0, xStart));
            lims.push_back(pow(10.0, xStop));
        }
        tp->data(lims);
    } else {
        formatAxisManual(textInterpreterProperty->getAsEnum(), limits[0], limits[1], xcnt,
            lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), xStart, xStop, xticks, xlabels);
    }
    lp = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    if (isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR)) {
        formatAxisAuto(textInterpreterProperty->getAsEnum(), limits[2], limits[3], ycnt,
            lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), yStart, yStop, yticks, ylabels);
        tp = static_cast<GOTwoVectorProperty*>(findProperty(GO_Y_LIM_PROPERTY_NAME_STR));
        std::vector<double> lims;
        if (lp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
            lims.push_back(yStart);
            lims.push_back(yStop);
        } else {
            lims.push_back(pow(10.0, yStart));
            lims.push_back(pow(10.0, yStop));
        }
        tp->data(lims);
    } else {
        formatAxisManual(textInterpreterProperty->getAsEnum(), limits[2], limits[3], ycnt,
            lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), yStart, yStop, yticks, ylabels);
    }

    lp = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    if (isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)) {
        formatAxisAuto(textInterpreterProperty->getAsEnum(), limits[4], limits[5], zcnt,
            lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), zStart, zStop, zticks, zlabels);
        tp = static_cast<GOTwoVectorProperty*>(findProperty(GO_Z_LIM_PROPERTY_NAME_STR));
        std::vector<double> lims;
        if (lp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
            lims.push_back(zStart);
            lims.push_back(zStop);
        } else {
            lims.push_back(pow(10.0, zStart));
            lims.push_back(pow(10.0, zStop));
        }
        tp->data(lims);
    } else {
        formatAxisManual(textInterpreterProperty->getAsEnum(), limits[4], limits[5], zcnt,
            lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), zStart, zStop, zticks, zlabels);
    }
    GOVectorProperty* hp = nullptr;
    GOStringVector* qp = nullptr;
    if (isAuto(GO_X_TICK_MODE_PROPERTY_NAME_STR)) {
        hp = static_cast<GOVectorProperty*>(findProperty(GO_X_TICK_PROPERTY_NAME_STR));
        hp->data(xticks);
    }
    if (isAuto(GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        qp = static_cast<GOStringVector*>(findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR));
        qp->data(xlabels);
    }
    if (isAuto(GO_Y_TICK_MODE_PROPERTY_NAME_STR)) {
        hp = static_cast<GOVectorProperty*>(findProperty(GO_Y_TICK_PROPERTY_NAME_STR));
        hp->data(yticks);
    }
    if (isAuto(GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        qp = static_cast<GOStringVector*>(findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR));
        qp->data(ylabels);
    }
    if (isAuto(GO_Z_TICK_MODE_PROPERTY_NAME_STR)) {
        hp = static_cast<GOVectorProperty*>(findProperty(GO_Z_TICK_PROPERTY_NAME_STR));
        hp->data(zticks);
    }
    if (isAuto(GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        qp = static_cast<GOStringVector*>(findProperty(GO_Z_TICK_LABEL_PROPERTY_NAME_STR));
        qp->data(zlabels);
    }
}
//=============================================================================
void
GOAxis::drawZGridLine(RenderInterface& gc, double t, std::vector<double> limits)
{
    std::vector<double> m;
    gc.getModelviewMatrix(m);
    if (m[6] > 0) {
        gc.line(limits[0], limits[2], t, limits[1], limits[2], t);
    } else if (m[6] < 0) {
        gc.line(limits[0], limits[3], t, limits[1], limits[3], t);
    }
    if (m[2] > 0) {
        gc.line(limits[0], limits[2], t, limits[0], limits[3], t);
    } else if (m[2] < 0) {
        gc.line(limits[1], limits[2], t, limits[1], limits[3], t);
    }
}
//=============================================================================
void
GOAxis::drawYGridLine(RenderInterface& gc, double t, std::vector<double> limits)
{
    std::vector<double> m;
    gc.getModelviewMatrix(m);
    if (m[10] > 0) {
        gc.line(limits[0], t, limits[4], limits[1], t, limits[4]);
    } else if (m[10] < 0) {
        gc.line(limits[0], t, limits[5], limits[1], t, limits[5]);
    }
    if (m[2] > 0) {
        gc.line(limits[0], t, limits[4], limits[0], t, limits[5]);
    } else if (m[2] < 0) {
        gc.line(limits[1], t, limits[4], limits[1], t, limits[5]);
    }
}
//=============================================================================
void
GOAxis::drawXGridLine(RenderInterface& gc, double t, std::vector<double> limits)
{
    std::vector<double> m;
    gc.getModelviewMatrix(m);
    if (m[10] > 0) {
        gc.line(t, limits[2], limits[4], t, limits[3], limits[4]);
    } else if (m[10] < 0) {
        gc.line(t, limits[2], limits[5], t, limits[3], limits[5]);
    }
    if (m[6] > 0) {
        gc.line(t, limits[2], limits[4], t, limits[2], limits[5]);
    } else if (m[6] < 0) {
        gc.line(t, limits[3], limits[4], t, limits[3], limits[5]);
    }
}
//=============================================================================
void
GOAxis::drawMinorGridLines(RenderInterface& gc)
{
    std::vector<double> limits(getAxisLimits());
    gc.setLineStyle(
        (static_cast<GOLineStyleProperty*>(findProperty(GO_MINOR_GRID_LINE_STYLE_PROPERTY_NAME_STR))
                ->data()));
    gc.depth(false);
    GOVectorProperty* hp
        = static_cast<GOVectorProperty*>(findProperty(GO_X_TICK_PROPERTY_NAME_STR));
    std::vector<double> xticks(hp->data());
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Y_TICK_PROPERTY_NAME_STR));
    std::vector<double> yticks(hp->data());
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Z_TICK_PROPERTY_NAME_STR));
    std::vector<double> zticks(hp->data());
    GOColorProperty* xc = static_cast<GOColorProperty*>(findProperty(GO_X_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* yc = static_cast<GOColorProperty*>(findProperty(GO_Y_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* zc = static_cast<GOColorProperty*>(findProperty(GO_Z_COLOR_PROPERTY_NAME_STR));
    GOLinearLogProperty* sp = nullptr;
    if ((static_cast<GOOnOffProperty*>(findProperty(GO_X_MINOR_GRID_PROPERTY_NAME_STR)))
            ->asBool()) {
        gc.color(xc->data());
        sp = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
        if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
            for (int i = 0; i < xticks.size() - 1; i++) {
                double t = mapX((xticks[i] + xticks[i + 1]) / 2);
                drawXGridLine(gc, t, limits);
            }
        } else {
            for (int i = 0; i < xticks.size() - 1; i++) {
                double t1 = xticks[i];
                double t2 = xticks[i + 1];
                if (t2 > t1) {
                    int n = 2;
                    while ((t1 * n) < t2) {
                        double t = mapX(n * t1);
                        n++;
                        drawXGridLine(gc, t, limits);
                    }
                }
            }
        }
    }
    if ((static_cast<GOOnOffProperty*>(findProperty(GO_Y_MINOR_GRID_PROPERTY_NAME_STR)))
            ->asBool()) {
        gc.color(yc->data());
        sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
        if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
            for (int i = 0; i < yticks.size() - 1; i++) {
                double t = mapY((yticks[i] + yticks[i + 1]) / 2);
                drawYGridLine(gc, t, limits);
            }
        } else {
            for (int i = 0; i < yticks.size() - 1; i++) {
                double t1 = yticks[i];
                double t2 = yticks[i + 1];
                if (t2 > t1) {
                    int n = 2;
                    while ((t1 * n) < t2) {
                        double t = mapY(n * t1);
                        n++;
                        drawYGridLine(gc, t, limits);
                    }
                }
            }
        }
    }
    if ((static_cast<GOOnOffProperty*>(findProperty(GO_Z_MINOR_GRID_PROPERTY_NAME_STR)))
            ->asBool()) {
        gc.color(zc->data());
        sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
        if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
            for (int i = 0; i < zticks.size() - 1; i++) {
                double t = mapZ((zticks[i] + zticks[i + 1]) / 2);
                drawZGridLine(gc, t, limits);
            }
        } else {
            for (int i = 0; i < zticks.size() - 1; i++) {
                double t1 = zticks[i];
                double t2 = zticks[i + 1];
                if (t2 > t1) {
                    int n = 2;
                    while ((t1 * n) < t2) {
                        double t = mapZ(n * t1);
                        n++;
                        drawZGridLine(gc, t, limits);
                    }
                }
            }
        }
    }
    gc.depth(true);
}
//=============================================================================
void
GOAxis::drawGridLines(RenderInterface& gc)
{
    std::vector<double> limits(getAxisLimits());
    gc.depth(false);
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.setLineStyle(
        (static_cast<GOLineStyleProperty*>(findProperty(GO_GRID_LINE_STYLE_PROPERTY_NAME_STR)))
            ->data());
    GOVectorProperty* hp
        = static_cast<GOVectorProperty*>(findProperty(GO_X_TICK_PROPERTY_NAME_STR));
    std::vector<double> xticks(hp->data());
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Y_TICK_PROPERTY_NAME_STR));
    std::vector<double> yticks(hp->data());
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Z_TICK_PROPERTY_NAME_STR));
    std::vector<double> zticks(hp->data());
    GOColorProperty* xc = static_cast<GOColorProperty*>(findProperty(GO_X_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* yc = static_cast<GOColorProperty*>(findProperty(GO_Y_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* zc = static_cast<GOColorProperty*>(findProperty(GO_Z_COLOR_PROPERTY_NAME_STR));
    if (xvisible
        && (static_cast<GOOnOffProperty*>(findProperty(GO_X_GRID_PROPERTY_NAME_STR)))->asBool()) {
        gc.color(xc->data());
        for (int i = 0; i < xticks.size(); i++) {
            double t = mapX(xticks[i]);
            drawXGridLine(gc, t, limits);
        }
    }
    if (yvisible
        && (static_cast<GOOnOffProperty*>(findProperty(GO_Y_GRID_PROPERTY_NAME_STR)))->asBool()) {
        gc.color(yc->data());
        for (int i = 0; i < yticks.size(); i++) {
            double t = mapY(yticks[i]);
            drawYGridLine(gc, t, limits);
        }
    }
    if (zvisible
        && (static_cast<GOOnOffProperty*>(findProperty(GO_Z_GRID_PROPERTY_NAME_STR)))->asBool()) {
        gc.color(zc->data());
        for (int i = 0; i < zticks.size(); i++) {
            double t = mapZ(zticks[i]);
            drawZGridLine(gc, t, limits);
        }
    }
    gc.depth(true);
}
//=============================================================================
void
GOAxis::getMaxTickMetric(
    RenderInterface& gc, std::vector<std::wstring> labs, double& maxx, double& maxy)
{
    maxx = 0;
    maxy = 0;
    for (int i = 0; i < labs.size(); i++) {
        int width, height, xoffset, yoffset;
        gc.measureText(labs[i], m_font, RenderInterface::Min, RenderInterface::Min, width, height,
            xoffset, yoffset);
        maxx = std::max(maxx, (double)width);
        maxy = std::max(maxy, (double)height);
    }
}
//=============================================================================
void
GOAxis::setAxisLimits(std::vector<double> lims)
{
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, lims[0], lims[1]);
    } else {
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, pow(10.0, lims[0]), pow(10.0, lims[1]));
    }
    sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, lims[2], lims[3]);
    } else {
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, pow(10.0, lims[2]), pow(10.0, lims[3]));
    }
    sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, lims[4], lims[5]);
    } else {
        setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, pow(10.0, lims[4]), pow(10.0, lims[5]));
    }
}
//=============================================================================
void
GOAxis::computeAxisLimits(const std::wstring& axisLimitPropertyName, indexType indexMin,
    indexType indexMax, double& limMin, double& limMax)
{
    GOTwoVectorProperty* hp
        = static_cast<GOTwoVectorProperty*>(findProperty(axisLimitPropertyName));
    if (std::isfinite(hp->data()[0])) {
        limMin = hp->data()[0];
    } else {
        GOSixVectorProperty* dataLimits
            = static_cast<GOSixVectorProperty*>(findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR));
        limMin = dataLimits->data()[indexMin];
    }
    if (std::isfinite(hp->data()[1])) {
        limMax = hp->data()[1];
    } else {
        GOSixVectorProperty* dataLimits
            = static_cast<GOSixVectorProperty*>(findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR));
        limMax = dataLimits->data()[indexMax];
    }
}
//=============================================================================
GOFigure*
GOAxis::getParentFigure()
{
    GOGObjectsProperty* parent
        = static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR));
    if (parent->data().empty()) {
        return nullptr;
    }
    unsigned parent_handle = parent->data()[0];
    return findGOFigure(parent_handle);
}
//=============================================================================
std::vector<double>
GOAxis::getPropertyVectorAsPixels(const std::wstring& name)
{
    GOFourVectorProperty* hp = static_cast<GOFourVectorProperty*>(findProperty(name));
    return (unitsReinterpret(hp->data()));
}
//=============================================================================
void
GOAxis::drawBox(RenderInterface& gc)
{
    GOColorProperty* hp = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    if (hp->isNone()) {
        return;
    }
    std::vector<double> limits(getAxisLimits());
    gc.color(hp->data());
    gc.depth(false);
    gc.quad(limits[0], limits[2], limits[4], limits[1], limits[2], limits[4], limits[1], limits[3],
        limits[4], limits[0], limits[3], limits[4]);
    gc.quad(limits[0], limits[2], limits[5], limits[0], limits[3], limits[5], limits[1], limits[3],
        limits[5], limits[1], limits[2], limits[5]);
    gc.quad(limits[0], limits[2], limits[4], limits[0], limits[3], limits[4], limits[0], limits[3],
        limits[5], limits[0], limits[2], limits[5]);
    gc.quad(limits[1], limits[2], limits[4], limits[1], limits[2], limits[5], limits[1], limits[3],
        limits[5], limits[1], limits[3], limits[4]);
    gc.quad(limits[0], limits[2], limits[4], limits[0], limits[2], limits[5], limits[1], limits[2],
        limits[5], limits[1], limits[2], limits[4]);
    gc.quad(limits[0], limits[3], limits[4], limits[1], limits[3], limits[4], limits[1], limits[3],
        limits[5], limits[0], limits[3], limits[5]);

    gc.depth(true);
}
//=============================================================================
void
GOAxis::setupAxis(RenderInterface& gc)
{
    std::vector<double> model;
    gc.getModelviewMatrix(model);
    std::vector<double> limits(getAxisLimits());
    if ((static_cast<GOTopBottomProperty*>(findProperty(GO_X_AXIS_LOCATION_PROPERTY_NAME_STR)))
            ->isEqual(GO_PROPERTY_VALUE_BOTTOM_STR)) {
        x1pos[2] = limits[4];
    } else {
        x1pos[2] = limits[5];
    }
    if ((static_cast<GOLeftRightProperty*>(findProperty(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR)))
            ->isEqual(GO_PROPERTY_VALUE_LEFT_STR)) {
        y1pos[2] = limits[4];
    } else {
        y1pos[2] = limits[5];
    }
    if ((model[10] > 0) && (model[6] > 0)) {
        if (x1pos[2] == limits[4]) {
            x1pos[1] = limits[3];
        } else {
            x1pos[1] = limits[2];
        }
    } else if ((model[10] > 0) && (model[6] <= 0)) {
        if (x1pos[2] == limits[4]) {
            x1pos[1] = limits[2];
        } else {
            x1pos[1] = limits[3];
        }
    } else if ((model[10] <= 0) && (model[6] > 0)) {
        if (x1pos[2] == limits[4]) {
            x1pos[1] = limits[2];
        } else {
            x1pos[1] = limits[3];
        }
    } else if ((model[10] <= 0) && (model[6] <= 0)) {
        if (x1pos[2] == limits[4]) {
            x1pos[1] = limits[3];
        } else {
            x1pos[1] = limits[2];
        }
    }
    double px0, py0, px1, py1, px2, py2;
    gc.toPixels(limits[0], x1pos[1], x1pos[2], px0, py0);
    gc.toPixels(limits[0], flipY(x1pos[1]), x1pos[2], px1, py1);
    gc.toPixels(limits[0], x1pos[1], flipZ(x1pos[2]), px2, py2);
    double len1, len2;
    len1 = ((px1 - px0) * (px1 - px0) + (py1 - py0) * (py1 - py0));
    len2 = ((px2 - px0) * (px2 - px0) + (py2 - py0) * (py2 - py0));
    if ((len1 > len2) && (len1 > 0)) {
        x2pos[1] = flipY(x1pos[1]);
        x2pos[2] = x1pos[2];
    } else {
        x2pos[1] = x1pos[1];
        x2pos[2] = flipZ(x1pos[2]);
    }
    if ((model[10] > 0) && (model[2] > 0)) {
        if (y1pos[2] == limits[4]) {
            y1pos[0] = limits[1];
        } else {
            y1pos[0] = limits[0];
        }
    } else if ((model[10] <= 0) && (model[2] > 0)) {
        if (y1pos[2] == limits[4]) {
            y1pos[0] = limits[0];
        } else {
            y1pos[0] = limits[1];
        }
    } else if ((model[10] > 0) && (model[2] <= 0)) {
        if (y1pos[2] == limits[4]) {
            y1pos[0] = limits[0];
        } else {
            y1pos[0] = limits[1];
        }
    } else if ((model[10] <= 0) && (model[2] <= 0)) {
        if (y1pos[2] == limits[4]) {
            y1pos[0] = limits[1];
        } else {
            y1pos[0] = limits[0];
        }
    }
    gc.toPixels(y1pos[0], limits[2], y1pos[2], px0, py0);
    gc.toPixels(flipX(y1pos[0]), limits[2], y1pos[2], px1, py1);
    gc.toPixels(y1pos[0], limits[2], flipZ(y1pos[2]), px2, py2);
    len1 = ((px1 - px0) * (px1 - px0) + (py1 - py0) * (py1 - py0));
    len2 = ((px2 - px0) * (px2 - px0) + (py2 - py0) * (py2 - py0));
    if ((len1 > len2) && (len1 > 0)) {
        y2pos[0] = y1pos[0];
        y2pos[2] = flipZ(y1pos[2]);
    } else {
        y2pos[0] = flipX(y1pos[0]);
        y2pos[2] = y1pos[2];
    }
    if (model[6] > 0) {
        z1pos[0] = limits[1];
    } else {
        z1pos[0] = limits[0];
    }
    if (model[2] > 0) {
        z1pos[1] = limits[2];
    } else {
        z1pos[1] = limits[3];
    }
    if ((model[10] > 0) && (model[6] > 0) && (model[2] > 0)) {
        z2pos[0] = limits[1];
        z2pos[1] = limits[3];
    } else if ((model[10] > 0) && (model[6] > 0) && (model[2] < 0)) {
        z2pos[0] = limits[0];
        z2pos[1] = limits[3];
    } else if ((model[10] > 0) && (model[6] < 0) && (model[2] > 0)) {
        z2pos[0] = limits[1];
        z2pos[1] = limits[2];
    } else if ((model[10] > 0) && (model[6] < 0) && (model[2] < 0)) {
        z2pos[0] = limits[0];
        z2pos[1] = limits[2];
    } else if ((model[10] < 0) && (model[6] > 0) && (model[2] > 0)) {
        z2pos[0] = limits[0];
        z2pos[1] = limits[2];
    } else if ((model[10] < 0) && (model[6] > 0) && (model[2] < 0)) {
        z2pos[0] = limits[1];
        z2pos[1] = limits[2];
    } else if ((model[10] < 0) && (model[6] < 0) && (model[2] > 0)) {
        z2pos[0] = limits[0];
        z2pos[1] = limits[3];
    } else if ((model[10] < 0) && (model[6] < 0) && (model[2] < 0)) {
        z2pos[0] = limits[1];
        z2pos[1] = limits[3];
    }
    if ((model[2] == 0) && (model[6] == 0)) {
        x2pos[1] = flipY(x1pos[1]);
        x2pos[2] = x1pos[2];
        y2pos[0] = flipX(y1pos[0]);
    }
    if ((model[6] == 0) && (model[10] == 0)) {
        y2pos[0] = y1pos[0];
        y2pos[2] = flipZ(y1pos[2]);
        z2pos[0] = z1pos[0];
        z2pos[1] = flipY(z1pos[1]);
    }
    if ((model[2] == 0) && (model[10] == 0)) {
        x2pos[1] = x1pos[1];
        x2pos[2] = flipZ(x1pos[2]);
        z2pos[0] = flipX(z1pos[0]);
        z2pos[1] = z1pos[1];
    }
    double x1, y1, x2, y2;
    gc.toPixels(limits[0], x1pos[1], x1pos[2], x1, y1);
    gc.toPixels(limits[1], x1pos[1], x1pos[2], x2, y2);
    xvisible = (fabs(x1 - x2) > 2) || (fabs(y1 - y2) > 2);
    gc.toPixels(y1pos[0], limits[2], y1pos[2], x1, y1);
    gc.toPixels(y1pos[0], limits[3], y1pos[2], x2, y2);
    yvisible = (fabs(x1 - x2) > 2) || (fabs(y1 - y2) > 2);
    gc.toPixels(z1pos[0], z1pos[1], limits[4], x1, y1);
    gc.toPixels(z1pos[0], z1pos[1], limits[5], x2, y2);
    zvisible = (fabs(x1 - x2) > 2) || (fabs(y1 - y2) > 2);
}
//=============================================================================
double
GOAxis::flipZ(double t)
{
    std::vector<double> limits(getAxisLimits());
    if (t == limits[4]) {
        return limits[5];
    }
    return limits[4];
}
//=============================================================================
double
GOAxis::flipY(double t)
{
    std::vector<double> limits(getAxisLimits());
    if (t == limits[2]) {
        return limits[3];
    }
    return limits[2];
}
//=============================================================================
double
GOAxis::flipX(double t)
{
    std::vector<double> limits(getAxisLimits());
    if (t == limits[0]) {
        return limits[1];
    }
    return limits[0];
}
//=============================================================================
void
GOAxis::updateAxisFont()
{
    QFont::Style fstyle = QFont::StyleNormal;
    QFont::Weight fweight = QFont::Normal;
    GOStringProperty* fontname
        = static_cast<GOStringProperty*>(findProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
    GOFontAngleProperty* fontangle
        = static_cast<GOFontAngleProperty*>(findProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR));
    GOFontWeightProperty* fontweight
        = static_cast<GOFontWeightProperty*>(findProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR));
    GOScalarProperty* fontsize
        = static_cast<GOScalarProperty*>(findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
    if (fontangle->isEqual(GO_PROPERTY_VALUE_NORMAL_STR)) {
        fstyle = QFont::StyleNormal;
    }
    if (fontangle->isEqual(GO_PROPERTY_VALUE_ITALIC_STR)) {
        fstyle = QFont::StyleItalic;
    }
    if (fontangle->isEqual(GO_PROPERTY_VALUE_OBLIQUE_STR)) {
        fstyle = QFont::StyleOblique;
    }
    if (fontweight->isEqual(GO_PROPERTY_VALUE_NORMAL_STR)) {
        fweight = QFont::Normal;
    }
    if (fontweight->isEqual(GO_PROPERTY_VALUE_BOLD_STR)) {
        fweight = QFont::Bold;
    }
    if (fontweight->isEqual(GO_PROPERTY_VALUE_LIGHT_STR)) {
        fweight = QFont::Light;
    }
    if (fontweight->isEqual(GO_PROPERTY_VALUE_DEMI_STR)) {
        fweight = QFont::DemiBold;
    }
    QFont fnt(wstringToQString(fontname->data()), (int)(fontsize->data()));
    fnt.setStyle(fstyle);
    fnt.setWeight(fweight);
    m_font = fnt;
    QFontMetrics fm(m_font);
    QRect sze(fm.boundingRect("|"));
    setScalarDoubleDefault(GO_TEXT_HEIGHT_PROPERTY_NAME_STR, sze.height());
}
//=============================================================================
bool
GOAxis::is2DView()
{
    return (!(xvisible && yvisible && zvisible));
}
//=============================================================================
void
GOAxis::drawAxisLines(RenderInterface& gc)
{
    std::vector<double> limits(getAxisLimits());
    GOColorProperty* xc = static_cast<GOColorProperty*>(findProperty(GO_X_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* yc = static_cast<GOColorProperty*>(findProperty(GO_Y_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* zc = static_cast<GOColorProperty*>(findProperty(GO_Z_COLOR_PROPERTY_NAME_STR));
    gc.setLineStyle(L"-");
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    if (xvisible) {
        gc.color(xc->data());
        double px0, py0, px1, py1;
        gc.toPixels(limits[0], x1pos[1], x1pos[2], px0, py0);
        gc.toPixels(limits[1], x1pos[1], x1pos[2], px1, py1);
        gc.setupDirectDraw();
        gc.line(px0, py0, px1, py1);
        gc.releaseDirectDraw();
        if (is2DView()) {
            gc.toPixels(limits[0], x2pos[1], x2pos[2], px0, py0);
            gc.toPixels(limits[1], x2pos[1], x2pos[2], px1, py1);
            gc.setupDirectDraw();
            gc.line(px0, py0, px1, py1);
            gc.releaseDirectDraw();
        }
    }
    if (yvisible) {
        gc.color(yc->data());
        double px0, py0, px1, py1;
        gc.toPixels(y1pos[0], limits[2], y1pos[2], px0, py0);
        gc.toPixels(y1pos[0], limits[3], y1pos[2], px1, py1);
        gc.setupDirectDraw();
        gc.line(px0, py0, px1, py1);
        gc.releaseDirectDraw();
        if (is2DView()) {
            gc.toPixels(y2pos[0], limits[2], y2pos[2], px0, py0);
            gc.toPixels(y2pos[0], limits[3], y2pos[2], px1, py1);
            gc.setupDirectDraw();
            gc.line(px0, py0, px1, py1);
            gc.releaseDirectDraw();
        }
    }
    if (zvisible) {
        gc.color(zc->data());
        double px0, py0, px1, py1;
        gc.toPixels(z1pos[0], z1pos[1], limits[4], px0, py0);
        gc.toPixels(z1pos[0], z1pos[1], limits[5], px1, py1);
        gc.setupDirectDraw();
        gc.line(px0, py0, px1, py1);
        gc.releaseDirectDraw();
        if (is2DView()) {
            gc.toPixels(z2pos[0], z2pos[1], limits[4], px0, py0);
            gc.toPixels(z2pos[0], z2pos[1], limits[5], px1, py1);
            gc.setupDirectDraw();
            gc.line(px0, py0, px1, py1);
            gc.releaseDirectDraw();
        }
    }
}
//=============================================================================
void
GOAxis::rePackFigure()
{
    if (!isAuto(GO_POSITION_MODE_PROPERTY_NAME_STR)) {
        return;
    }
    int titleHeight = 0;
    int xlabelHeight = 0;
    int ylabelHeight = 0;
    int zlabelHeight = 0;
    int maxLabelHeight = 0;
    int tickHeight = 0;
    int maxTickWidth = 0;
    int maxTickHeight = 0;
    QFontMetrics fm(m_font);
    GOGObjectsProperty* lbl
        = static_cast<GOGObjectsProperty*>(findProperty(GO_X_LABEL_PROPERTY_NAME_STR));
    if (!lbl->data().empty()) {
        GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
        xlabelHeight = fp->getTextHeightInPixels();
    }
    GOStringVector* gp
        = static_cast<GOStringVector*>(findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> xlabels(gp->data());
    for (int i = 0; i < xlabels.size(); i++) {
        QRect sze(fm.boundingRect(Nelson::wstringToQString(xlabels[i])));
        maxTickWidth = std::max(maxTickWidth, sze.width());
        maxTickHeight = std::max(maxTickHeight, sze.height());
    }
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Y_LABEL_PROPERTY_NAME_STR));
    if (!lbl->data().empty()) {
        GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
        ylabelHeight = fp->getTextHeightInPixels();
    }
    gp = static_cast<GOStringVector*>(findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> ylabels(gp->data());
    for (int i = 0; i < ylabels.size(); i++) {
        QRect sze(fm.boundingRect(wstringToQString(ylabels[i])));
        maxTickWidth = std::max(maxTickWidth, sze.width());
        maxTickHeight = std::max(maxTickHeight, sze.height());
    }
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Z_LABEL_PROPERTY_NAME_STR));
    if (!lbl->data().empty()) {
        GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
        zlabelHeight = fp->getTextHeightInPixels();
    }
    gp = static_cast<GOStringVector*>(findProperty(GO_Z_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> zlabels(gp->data());
    for (int i = 0; i < zlabels.size(); i++) {
        QRect sze(fm.boundingRect(wstringToQString(zlabels[i])));
        maxTickWidth = std::max(maxTickWidth, sze.width());
        maxTickHeight = std::max(maxTickHeight, sze.height());
    }
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_TITLE_PROPERTY_NAME_STR));
    if (!lbl->data().empty()) {
        GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
        titleHeight = fp->getTextHeightInPixels();
    }
    QRect sze(fm.boundingRect("|"));
    tickHeight = sze.height();
    maxLabelHeight = std::max(titleHeight, xlabelHeight);
    maxLabelHeight = std::max(maxLabelHeight, ylabelHeight);
    maxLabelHeight = std::max(maxLabelHeight, zlabelHeight);
    std::vector<double> outerpos(getPropertyVectorAsPixels(GO_OUTER_POSITION_PROPERTY_NAME_STR));
    GOFigure* fig = getParentFigure();
    int width = fig->getWidth();
    int height = fig->getHeight();
    if ((maxTickWidth == 0) && (maxTickHeight == 0) && (maxLabelHeight == 0)) {
        GOFourVectorProperty* hp
            = static_cast<GOFourVectorProperty*>(findProperty(GO_POSITION_PROPERTY_NAME_STR));
        hp->value(
            outerpos[0] / width, outerpos[1] / height, outerpos[2] / width, outerpos[3] / height);
        return;
    }
    double posx0 = outerpos[2] * 0.1 + outerpos[0];
    double posy0 = outerpos[3] * 0.1 + outerpos[1];
    double poswidth = outerpos[2] * 0.8;
    double posheight = outerpos[3] * .8;
    maxLabelHeight = (int)(maxLabelHeight * 2.0 + tickHeight);
    if (posx0 < maxLabelHeight) {
        posx0 = maxLabelHeight;
    }
    if (posy0 < maxLabelHeight) {
        posy0 = maxLabelHeight;
    }
    if ((outerpos[2] - poswidth) < 2 * maxLabelHeight) {
        poswidth = outerpos[2] - 2 * maxLabelHeight;
    }
    if ((outerpos[3] - posheight) < 2 * maxLabelHeight) {
        posheight = outerpos[3] - 2 * maxLabelHeight;
    }
    GOFourVectorProperty* hp
        = static_cast<GOFourVectorProperty*>(findProperty(GO_POSITION_PROPERTY_NAME_STR));
    hp->value((double)(posx0 / width), double(posy0 / height), double(poswidth / width),
        double(posheight / height));
    hp->clearModified();
}
//=============================================================================
void
GOAxis::updateLimits(bool x, bool y, bool z, bool a, bool c)
{
    if (!x && !y && !z && !a && !c) {
        return;
    }
    std::vector<double> limits;
    bool first = true;
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());

    std::vector<double> clim(findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR));
    std::vector<double> alim(findVectorDoubleProperty(GO_A_LIM_PROPERTY_NAME_STR));

    GOSixVectorProperty* hp
        = static_cast<GOSixVectorProperty*>(findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR));

    for (int i = 0; i < handles.size(); i++) {
        GraphicsObject* fp = findGraphicsObject(handles[i]);
        std::wstring goType = fp->getType();
        if (goType == GO_PROPERTY_VALUE_HGGROUP_STR) {
            auto childrenHgGroup
                = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
            std::vector<int64> handlesHgGroup(childrenHgGroup->data());
            for (size_t k = 0; k < handlesHgGroup.size(); ++k) {
                GraphicsObject* hp = findGraphicsObject(handlesHgGroup[k]);
                std::vector<double> child_limits(hp->getLimits());
                if (!child_limits.empty()) {
                    if (first) {
                        limits = child_limits;
                        if (hp->getType() == GO_PROPERTY_VALUE_PATCH_STR) {
                            limits[6] = std::min(limits[6], clim[0]);
                            limits[7] = std::max(limits[7], clim[1]);
                            limits[8] = std::min(limits[8], alim[0]);
                            limits[9] = std::max(limits[9], alim[1]);
                        }
                        first = false;
                    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
                        for (ompIndexType i = 0;
                             i < (ompIndexType)std::min(limits.size(), child_limits.size());
                             i += 2) {
                            limits[i] = std::min(limits[i], child_limits[i]);
                            limits[i + 1] = std::max(limits[i + 1], child_limits[i + 1]);
                        }
                    }
                }
            }
        } else {
            std::vector<double> child_limits(fp->getLimits());
            if (!child_limits.empty()) {
                if (first) {
                    limits = child_limits;
                    if (goType == GO_PROPERTY_VALUE_PATCH_STR) {
                        limits[6] = std::min(limits[6], clim[0]);
                        limits[7] = std::max(limits[7], clim[1]);
                        limits[8] = std::min(limits[8], alim[0]);
                        limits[9] = std::max(limits[9], alim[1]);
                    }
                    first = false;
                } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
                    for (ompIndexType i = 0;
                         i < (ompIndexType)std::min(limits.size(), child_limits.size()); i += 2) {
                        limits[i] = std::min(limits[i], child_limits[i]);
                        limits[i + 1] = std::max(limits[i + 1], child_limits[i + 1]);
                    }
                }
            }
        }
    }
    if (first) {
        return;
    }
    if (limits[1] == limits[0]) {
        limits[0] = limits[0] - 0.5;
        limits[1] = limits[0] + 1;
    }
    if (limits[3] == limits[2]) {
        limits[2] = limits[2] - 0.5;
        limits[3] = limits[2] + 1;
    }
    if (limits[5] == limits[4]) {
        limits[4] = limits[4] - 0.5;
        limits[5] = limits[4] + 1;
    }
    hp->value(limits[0], limits[1], limits[2], limits[3], limits[4], limits[5]);
    if (x) {
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, limits[0], limits[1]);
    }
    if (y) {
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, limits[2], limits[3]);
    }
    if (z) {
        setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, limits[4], limits[5]);
    }
    if (c) {
        setTwoVectorDefault(GO_C_LIM_PROPERTY_NAME_STR, limits[6], limits[7]);
    }
    if (a) {
        setTwoVectorDefault(GO_A_LIM_PROPERTY_NAME_STR, limits[8], limits[9]);
    }
}
//=============================================================================
void
GOAxis::handlePlotBoxFlags()
{
    bool xflag = isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR);
    bool yflag = isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    bool zflag = isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR);

    bool axesauto = xflag && yflag && zflag;
    bool darauto = isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    bool pbaauto = isAuto(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    bool onemanual
        = (!xflag && yflag && zflag) || (xflag && !yflag && zflag) || (xflag && yflag && !zflag);

    std::vector<double> limits(getAxisLimits());
    double xrange = limits[1] - limits[0];
    double yrange = limits[3] - limits[2];
    double zrange = limits[5] - limits[4];
    double minrange = std::min(xrange, std::min(yrange, zrange));
    double maxrange = std::max(xrange, std::max(yrange, zrange));
    std::vector<double> pba(findVectorDoubleProperty(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR));
    double xratio = pba[0];
    double yratio = pba[1];
    double zratio = pba[2];
    double minratio = std::min(xratio, std::min(yratio, zratio));
    xratio /= minratio;
    yratio /= minratio;
    zratio /= minratio;
    std::vector<double> dar(findVectorDoubleProperty(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR));
    double xscale = dar[0];
    double yscale = dar[1];
    double zscale = dar[2];
    double minscale = std::min(xscale, std::min(yscale, zscale));
    xscale /= minscale;
    yscale /= minscale;
    zscale /= minscale;

    if (darauto && pbaauto) {
        setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, 1, 1, 1);
        setThreeVectorDefault(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, xrange / minrange,
            yrange / minrange, zrange / minrange);
    } else if (darauto && !pbaauto) {
        setThreeVectorDefault(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, xratio, yratio, zratio);
        setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, xrange / xratio,
            yrange / yratio, zrange / zratio);
    } else if (!darauto && pbaauto) {
        setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, xscale, yscale, zscale);
    } else {
        if (axesauto) {
            rerange(limits[0], limits[1], xratio / xscale * maxrange);
            rerange(limits[2], limits[3], yratio / yscale * maxrange);
            rerange(limits[4], limits[5], zratio / zscale * maxrange);
            setAxisLimits(limits);
        } else if (onemanual) {
            if (!xflag) {
                rerange(limits[2], limits[3], yratio / yscale * xrange);
                rerange(limits[4], limits[5], zratio / zscale * xrange);
                setAxisLimits(limits);
            } else if (!yflag) {
                rerange(limits[0], limits[1], xratio / xscale * yrange);
                rerange(limits[4], limits[5], zratio / zscale * yrange);
                setAxisLimits(limits);
            } else if (!zflag) {
                rerange(limits[0], limits[1], xratio / xscale * zrange);
                rerange(limits[2], limits[3], yratio / yscale * zrange);
                setAxisLimits(limits);
            }
        } else {
            // IGNORE
        }
    }
}
//=============================================================================
void
GOAxis::drawLabel(RenderInterface& gc, double dx, double dy, double x2, double y2,
    const std::vector<double>& color, const std::wstring& txt)
{
    double angle = atan2(dy, dx) * 180.0 / M_PI;
    RenderInterface::AlignmentFlag xalign = RenderInterface::Min;
    RenderInterface::AlignmentFlag yalign = RenderInterface::Min;
    if (fabs(angle) < 10) {
        xalign = RenderInterface::Min;
        yalign = RenderInterface::Mean;
    } else if (fabs(angle) > 170) {
        xalign = RenderInterface::Max;
        yalign = RenderInterface::Mean;
    } else if ((angle >= 10) && (angle < 80)) {
        xalign = RenderInterface::Min;
        yalign = RenderInterface::Min;
    } else if ((angle >= 80) && (angle < 100)) {
        xalign = RenderInterface::Mean;
        yalign = RenderInterface::Min;
    } else if ((angle >= 100) && (angle < 170)) {
        xalign = RenderInterface::Max;
        yalign = RenderInterface::Min;
    } else if ((angle <= -10) && (angle > -80)) {
        xalign = RenderInterface::Min;
        yalign = RenderInterface::Max;
    } else if ((angle <= -80) && (angle > -100)) {
        xalign = RenderInterface::Mean;
        yalign = RenderInterface::Max;
    } else if ((angle <= -100) && (angle > -170)) {
        xalign = RenderInterface::Max;
        yalign = RenderInterface::Max;
    }
    gc.setupDirectDraw();
    gc.putText(x2, y2, txt, color, xalign, yalign, m_font, 0);
    gc.releaseDirectDraw();
}
//=============================================================================
void
GOAxis::drawTickLabels(RenderInterface& gc, const std::vector<double>& color, double px1,
    double py1, double pz1, double px2, double py2, double pz2, double limmin, double limmax,
    double unitx, double unity, double unitz, std::vector<double> maptics,
    std::vector<double> minortics, std::vector<std::wstring> labels, const std::wstring& labelname,
    int ticlen, double ticdir)
{
    gc.color(color);
    double dx1, dy1, dx2, dy2;
    gc.debug();
    gc.toPixels(limmin * unitx + px1, limmin * unity + py1, limmin * unitz + pz1, dx1, dy1);
    gc.toPixels(limmin * unitx + px2, limmin * unity + py2, limmin * unitz + pz2, dx2, dy2);
    double delx = dx2 - dx1;
    double dely = dy2 - dy1;
    double norm = sqrt(delx * delx + dely * dely);
    if (norm > 0) {
        delx /= norm;
        dely /= norm;
        for (int i = 0; i < minortics.size(); i++) {
            double t = minortics[i];
            double x1, y1, x2, y2;
            gc.toPixels(t * unitx + px1, t * unity + py1, t * unitz + pz1, x1, y1);
            x2 = delx * ticlen * ticdir * 0.6 + x1;
            y2 = dely * ticlen * ticdir * 0.6 + y1;
            gc.setupDirectDraw();
            gc.line(x1, y1, x2, y2);
            gc.releaseDirectDraw();
        }
        for (int i = 0; i < maptics.size(); i++) {
            double t = maptics[i];
            double x1, y1, x2, y2;
            gc.toPixels(t * unitx + px1, t * unity + py1, t * unitz + pz1, x1, y1);
            x2 = delx * ticlen * ticdir + x1;
            y2 = dely * ticlen * ticdir + y1;
            gc.setupDirectDraw();
            gc.line(x1, y1, x2, y2);
            gc.releaseDirectDraw();
            double x3, y3;
            if (ticdir > 0) {
                x3 = -delx * 0.015 * norm + x1;
                y3 = -dely * 0.015 * norm + y1;
            } else {
                x3 = -delx * 0.015 * norm + x2;
                y3 = -dely * 0.015 * norm + y2;
            }
            if (!labels.empty()) {
                drawLabel(gc, -delx, -dely, x3, y3, color, labels[i % labels.size()]);
            }
            if (is2DView()) {
                gc.toPixels(t * unitx + px2, t * unity + py2, t * unitz + pz2, x1, y1);
                x2 = -delx * ticlen * ticdir + x1;
                y2 = -dely * ticlen * ticdir + y1;
                gc.setupDirectDraw();
                gc.line(x1, y1, x2, y2);
                gc.releaseDirectDraw();
            }
        }
        double maxx, maxy;
        getMaxTickMetric(gc, labels, maxx, maxy);
        double x1, x2, x3, y1, y2, y3;
        double meanval = (limmin + limmax) / 2.0;
        gc.toPixels(meanval * unitx + px1, meanval * unity + py1, meanval * unitz + pz1, x1, y1);
        x2 = delx * ticlen * ticdir + x1;
        y2 = dely * ticlen * ticdir + y1;
        if (ticdir > 0) {
            x3 = -delx * 0.015 * norm + x1;
            y3 = -dely * 0.015 * norm + y1;
        } else {
            x3 = -delx * 0.015 * norm + x2;
            y3 = -dely * 0.015 * norm + y2;
        }
        double lx, ly;
        if (delx != 0) {
            lx = fabs(maxx / delx);
        } else {
            lx = 1e10;
        }
        if (dely != 0) {
            ly = fabs(maxy / dely);
        } else {
            ly = 1e10;
        }
        double lmax = std::min(lx, ly);
        GOGObjectsProperty* lbl = static_cast<GOGObjectsProperty*>(findProperty(labelname));
        if (!lbl->data().empty()) {
            GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
            double axx1, axy1, axx2, axy2;
            gc.toPixels(0, 0, 0, axx1, axy1);
            gc.toPixels(unitx, unity, unitz, axx2, axy2);
            double angle = atan2(axy2 - axy1, axx2 - axx1) * 180.0 / M_PI;
            if (angle < -90) {
                angle += 180;
            }
            if (angle > 90) {
                angle -= 180;
            }
            GOScalarProperty* sp
                = static_cast<GOScalarProperty*>(fp->findProperty(GO_ROTATION_PROPERTY_NAME_STR));
            double origx, origy;
            gc.toPixels(meanval * unitx + (px1 + px2) / 2.0, meanval * unity + (py1 + py2) / 2.0,
                meanval * unitz + (pz1 + pz2) / 2.0, origx, origy);
            double meanx, meany;
            gc.toPixels(
                meanval * unitx + px1, meanval * unity + py1, meanval * unitz + pz1, meanx, meany);
            double xl1 = x3 - lmax * delx;
            double yl1 = y3 - lmax * dely;
            double angle2 = atan2(y3 - origy, x3 - origx) * 180.0 / M_PI;
            if ((angle == 90) && (angle2 > -90)) {
                angle = -90;
            }
            if (angle2 == 180) {
                angle2 = -180;
            }
            if (angle2 < 0) {
                if (fabs(angle) != 90) {
                    (static_cast<GOAlignVertProperty*>(
                         fp->findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)))
                        ->data(GO_PROPERTY_VALUE_TOP_STR);
                } else {

                    (static_cast<GOAlignVertProperty*>(
                         fp->findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)))
                        ->data(GO_PROPERTY_VALUE_BOTTOM_STR);
                }
            } else {
                (static_cast<GOAlignVertProperty*>(
                     fp->findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)))
                    ->data(GO_PROPERTY_VALUE_BOTTOM_STR);
            }
            if ((angle == -90) && (angle2 == -180)) {
                angle = 90;
            }
            sp->data(angle);
            xl1 += (x3 - origx) * 0.04;
            yl1 += (y3 - origy) * 0.04;
            GOThreeVectorProperty* gp = static_cast<GOThreeVectorProperty*>(
                fp->findProperty(GO_POSITION_PROPERTY_NAME_STR));
            std::vector<double> outerpos(
                getPropertyVectorAsPixels(GO_OUTER_POSITION_PROPERTY_NAME_STR));
            double xnorm, ynorm;
            xnorm = (xl1 - outerpos[0]) / outerpos[2];
            ynorm = (yl1 - outerpos[1]) / outerpos[3];
            gp->value(xnorm, ynorm, 0.0);
        }
    }
}
//=============================================================================
void
GOAxis::drawAxisLabels(RenderInterface& gc)
{
    gc.lookAt(0, 0, 1, 0.0, 0.0, 0, 0, 1, 0);
    gc.project(0, 1, 0, 1, -1, 1);
    std::vector<double> outerpos(getPropertyVectorAsPixels(GO_OUTER_POSITION_PROPERTY_NAME_STR));
    gc.viewport(outerpos[0], outerpos[1], outerpos[2], outerpos[3]);
    GOGObjectsProperty* lbl;
    std::wstring xdir(findStringProperty(GO_X_DIR_PROPERTY_NAME_STR));
    std::wstring ydir(findStringProperty(GO_Y_DIR_PROPERTY_NAME_STR));
    setStringDefault(GO_X_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    if (xvisible) {
        lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_X_LABEL_PROPERTY_NAME_STR));
        if (!lbl->data().empty()) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            fp->paintMe(gc);
        }
    }
    if (yvisible) {
        lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Y_LABEL_PROPERTY_NAME_STR));
        if (!lbl->data().empty()) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            fp->paintMe(gc);
        }
    }
    if (zvisible) {
        lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Z_LABEL_PROPERTY_NAME_STR));
        if (!lbl->data().empty()) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0], false);
            if (fp) {
                fp->paintMe(gc);
            }
        }
    }
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_TITLE_PROPERTY_NAME_STR));
    if (!lbl->data().empty()) {
        GraphicsObject* fp = findGraphicsObject(lbl->data()[0], false);
        if (fp) {
            fp->paintMe(gc);
        }
    }
    setupProjection(gc);
    setStringDefault(GO_X_DIR_PROPERTY_NAME_STR, xdir);
    setStringDefault(GO_Y_DIR_PROPERTY_NAME_STR, ydir);
}
//============================================================================
void
GOAxis::drawTickMarks(RenderInterface& gc)
{
    GOVectorProperty* hp = nullptr;
    hp = static_cast<GOVectorProperty*>(findProperty(GO_X_TICK_PROPERTY_NAME_STR));
    std::vector<double> xticks(hp->data());
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Y_TICK_PROPERTY_NAME_STR));
    std::vector<double> yticks(hp->data());
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Z_TICK_PROPERTY_NAME_STR));
    std::vector<double> zticks(hp->data());
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    GOColorProperty* xc = static_cast<GOColorProperty*>(findProperty(GO_X_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* yc = static_cast<GOColorProperty*>(findProperty(GO_Y_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* zc = static_cast<GOColorProperty*>(findProperty(GO_Z_COLOR_PROPERTY_NAME_STR));

    std::vector<double> position(getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR));
    int maxlen = (int)((position[2] > position[3]) ? position[2] : position[3]);
    GOTwoVectorProperty* kp
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_TICK_LENGTH_PROPERTY_NAME_STR));
    std::vector<double> ticklen(kp->data());
    int ticlen;
    if (is2DView()) {
        ticlen = (int)(maxlen * ticklen[0]);
    } else {
        ticlen = (int)(maxlen * ticklen[1]);
    }
    float ticdir;
    if (isAuto(GO_TICK_DIR_MODE_PROPERTY_NAME_STR)) {
        if (is2DView()) {
            ticdir = 1;
        } else {
            ticdir = -1;
        }
    } else {
        if ((static_cast<GOInOutProperty*>(findProperty(GO_TICK_DIR_PROPERTY_NAME_STR)))
                ->isEqual(GO_PROPERTY_VALUE_IN_STR)) {
            ticdir = 1;
        } else {
            ticdir = -1;
        }
    }

    GOTextInterpreterProperty* textInterpreterProperty = static_cast<GOTextInterpreterProperty*>(
        findProperty(GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR));
    TEXT_INTERPRETER_FORMAT textFormat = textInterpreterProperty->getAsEnum();

    GOStringVector* qp
        = static_cast<GOStringVector*>(findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> xlabeltxt;
    if (textFormat == TEX_MARKUP) {
        xlabeltxt = texToUnicode(qp->data());
    } else {
        xlabeltxt = qp->data();
    }

    qp = static_cast<GOStringVector*>(findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> ylabeltxt;
    if (textFormat == TEX_MARKUP) {
        ylabeltxt = texToUnicode(qp->data());
    } else {
        ylabeltxt = qp->data();
    }

    qp = static_cast<GOStringVector*>(findProperty(GO_Z_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> zlabeltxt;
    if (textFormat == TEX_MARKUP) {
        zlabeltxt = texToUnicode(qp->data());
    } else {
        zlabeltxt = qp->data();
    }

    std::vector<double> limits(getAxisLimits());
    gc.setLineStyle(L"-");

    if (xvisible) {
        std::vector<double> mapticks;
        mapticks.reserve(xticks.size());
        for (size_t i = 0; i < xticks.size(); i++) {
            mapticks.push_back(mapX(xticks[i]));
        }
        std::vector<double> minorticks;
        GOLinearLogProperty* sp
            = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
        if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR)) {
            minorticks.reserve(xticks.size());
            for (size_t i = 0; i < xticks.size() - 1; i++) {
                double t1 = xticks[i];
                double t2 = xticks[i + 1];
                int n = 2;
                while (((t1 * n) < t2) && (n < MAX_MINI_TICK_COUNT)) {
                    minorticks.push_back(mapX(n * t1));
                    n++;
                }
            }
        }
        drawTickLabels(gc, xc->data(), 0, x1pos[1], x1pos[2], 0, x2pos[1], x2pos[2], limits[0],
            limits[1], 1, 0, 0, mapticks, minorticks, xlabeltxt, GO_X_LABEL_PROPERTY_NAME_STR,
            ticlen, ticdir);
    }
    if (yvisible) {
        std::vector<double> mapticks;
        mapticks.reserve(yticks.size());
        for (size_t i = 0; i < yticks.size(); i++) {
            mapticks.push_back(mapY(yticks[i]));
        }
        std::vector<double> minorticks;
        GOLinearLogProperty* sp
            = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
        if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR)) {
            minorticks.reserve(yticks.size());
            for (size_t i = 0; i < yticks.size() - 1; i++) {
                double t1 = yticks[i];
                double t2 = yticks[i + 1];
                int n = 2;
                while (((t1 * n) < t2) && (n < MAX_MINI_TICK_COUNT)) {
                    minorticks.push_back(mapY(n * t1));
                    n++;
                }
            }
        }
        drawTickLabels(gc, yc->data(), y1pos[0], 0, y1pos[2], y2pos[0], 0, y2pos[2], limits[2],
            limits[3], 0, 1, 0, mapticks, minorticks, ylabeltxt, GO_Y_LABEL_PROPERTY_NAME_STR,
            ticlen, ticdir);
    }
    if (zvisible) {
        std::vector<double> mapticks;
        mapticks.reserve(zticks.size());
        for (size_t i = 0; i < zticks.size(); i++) {
            mapticks.push_back(mapZ(zticks[i]));
        }
        std::vector<double> minorticks;
        GOLinearLogProperty* sp
            = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
        if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR)) {
            minorticks.reserve(zticks.size());
            for (size_t i = 0; i < zticks.size() - 1; i++) {
                double t1 = zticks[i];
                double t2 = zticks[i + 1];
                int n = 2;
                while (((t1 * n) < t2) && (n < MAX_MINI_TICK_COUNT)) {
                    minorticks.push_back(mapZ(n * t1));
                    n++;
                }
            }
        }
        drawTickLabels(gc, zc->data(), z1pos[0], z1pos[1], 0, z2pos[0], z2pos[1], 0, limits[4],
            limits[5], 0, 0, 1, mapticks, minorticks, zlabeltxt, GO_Z_LABEL_PROPERTY_NAME_STR,
            ticlen, ticdir);
    }
}
//=============================================================================
void
GOAxis::drawChildren(RenderInterface& gc)
{
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());
    for (auto h : handles) {
        GraphicsObject* fp = findGraphicsObject(h, false);
        if (fp) {
            fp->paintMe(gc);
        }
    }
}
//=============================================================================
void
GOAxis::updateState()
{
    std::vector<std::wstring> tset;
    if (hasChanged(GO_X_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_X_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_Z_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_A_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_A_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_C_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_C_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_TICK_PROPERTY_NAME_STR)) {
        toManual(GO_X_TICK_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_TICK_PROPERTY_NAME_STR)) {
        toManual(GO_Y_TICK_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_TICK_PROPERTY_NAME_STR)) {
        toManual(GO_Z_TICK_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_TICK_LABEL_PROPERTY_NAME_STR)) {
        toManual(GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_TICK_LABEL_PROPERTY_NAME_STR)) {
        toManual(GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_TICK_LABEL_PROPERTY_NAME_STR)) {
        toManual(GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        toManual(GO_POSITION_MODE_PROPERTY_NAME_STR);
    }
    tset.push_back(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    tset.push_back(GO_FONT_NAME_PROPERTY_NAME_STR);
    tset.push_back(GO_FONT_SIZE_PROPERTY_NAME_STR);
    tset.push_back(GO_FONT_UNITS_PROPERTY_NAME_STR);
    tset.push_back(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    tset.push_back(GO_X_TICK_LABEL_PROPERTY_NAME_STR);
    tset.push_back(GO_Y_TICK_LABEL_PROPERTY_NAME_STR);
    tset.push_back(GO_Z_TICK_LABEL_PROPERTY_NAME_STR);
    tset.push_back(GO_X_COLOR_PROPERTY_NAME_STR);
    tset.push_back(GO_Y_COLOR_PROPERTY_NAME_STR);
    tset.push_back(GO_Z_COLOR_PROPERTY_NAME_STR);
    if (hasChanged(tset)) {
        updateAxisFont();
        clearChanged(tset);
    }

    bool xflag = isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR);
    bool yflag = isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    bool zflag = isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR);
    bool aflag = isAuto(GO_A_LIM_MODE_PROPERTY_NAME_STR);
    bool cflag = isAuto(GO_C_LIM_MODE_PROPERTY_NAME_STR);
    updateLimits(xflag, yflag, zflag, aflag, cflag);

    if (hasChanged(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR))
        toManual(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    if (hasChanged(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR))
        toManual(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);

    handlePlotBoxFlags();

    if (hasChanged(GO_CAMERA_TARGET_PROPERTY_NAME_STR))
        toManual(GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR);
    if (isAuto(GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR)) {
        GOThreeVectorProperty* tv
            = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR));
        std::vector<double> limits(getAxisLimits());
        tv->value((limits[0] + limits[1]) / 2.0, (limits[2] + limits[3]) / 2.0,
            (limits[4] + limits[5]) / 2.0);
    }
    if (hasChanged(GO_CAMERA_POSITION_PROPERTY_NAME_STR)) {
        toManual(GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR)) {
        GOThreeVectorProperty* tv = static_cast<GOThreeVectorProperty*>(
            findProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR));
        std::vector<double> limits(getAxisLimits());
        tv->value((limits[0] + limits[1]) / 2.0, (limits[2] + limits[3]) / 2.0, limits[5] + 1);
    }
    if (hasChanged(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR)) {
        toManual(GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR)) {
        GOThreeVectorProperty* tv = static_cast<GOThreeVectorProperty*>(
            findProperty(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR));
        tv->value(0, 1, 0);
    }

    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());
    for (int i = 0; i < handles.size(); i++) {
        GraphicsObject* fp = findGraphicsObject(handles[i]);
        fp->updateState();
    }

    rePackFigure();
    recalculateTicks();
    rePackFigure();
    recalculateTicks();
    rePackFigure();
    clearAllChanged();
    GOFigure* fig = getParentFigure();
    if (fig) {
        fig->repaint();
    }
}
//=============================================================================
void
GOAxis::paintMe(RenderInterface& gc)
{
    if (getParentFigure() == NULL) {
        return;
    }
    setupProjection(gc);
    setupAxis(gc);
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        drawBox(gc);
        drawGridLines(gc);
        drawMinorGridLines(gc);
    } else {
        GOColorProperty* hp
            = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
        if (!hp->isNone()) {
            gc.color(hp->data());
        }
    }

    drawChildren(gc);

    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        drawAxisLines(gc);
        drawTickMarks(gc);
        drawAxisLabels(gc);
    }
}
//=============================================================================
bool
GOAxis::is2D()
{
    return (!(xvisible && yvisible && zvisible));
}
//=============================================================================
std::vector<double>
GOAxis::unitsReinterpret(std::vector<double> a)
{
    GOFigure* fig = getParentFigure();
    unsigned width = fig->getWidth();
    unsigned height = fig->getHeight();
    GOUnitsProperty* hp = static_cast<GOUnitsProperty*>(findProperty(GO_UNITS_PROPERTY_NAME_STR));
    if (hp->isEqual(GO_PROPERTY_VALUE_NORMALIZED_STR)) {
        for (size_t i = 0; i < a.size(); i += 2) {
            a[i] *= width;
            a[i + 1] *= height;
        }
    } else if (hp->isEqual(GO_PROPERTY_VALUE_PIXELS_STR)) {
    } else {
        Error(_W("Units not managed:") + hp->data());
    }
    return a;
}
//=============================================================================
std::vector<double>
GOAxis::getAxisLimits()
{
    std::vector<double> lims;
    double xLimMin;
    double xLimMax;
    computeAxisLimits(GO_X_LIM_PROPERTY_NAME_STR, 0, 1, xLimMin, xLimMax);
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        lims.push_back(xLimMin);
        lims.push_back(xLimMax);
    } else {
        lims.push_back(tickLog(xLimMin));
        lims.push_back(tickLog(xLimMax));
    }
    double yLimMin;
    double yLimMax;
    computeAxisLimits(GO_Y_LIM_PROPERTY_NAME_STR, 2, 3, yLimMin, yLimMax);
    sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        lims.push_back(yLimMin);
        lims.push_back(yLimMax);
    } else {
        lims.push_back(tickLog(yLimMin));
        lims.push_back(tickLog(yLimMax));
    }

    double zLimMin;
    double zLimMax;
    computeAxisLimits(GO_Z_LIM_PROPERTY_NAME_STR, 4, 5, zLimMin, zLimMax);
    sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        lims.push_back(zLimMin);
        lims.push_back(zLimMax);
    } else {
        lims.push_back(tickLog(zLimMin));
        lims.push_back(tickLog(zLimMax));
    }
    return lims;
}
//=============================================================================
void
GOAxis::setupProjection(RenderInterface& gc)
{
    GOThreeVectorProperty* tv1
        = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR));
    GOThreeVectorProperty* tv2
        = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR));
    GOThreeVectorProperty* tv3
        = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR));
    gc.lookAt(tv1->data()[0], tv1->data()[1], tv1->data()[2], tv2->data()[0], tv2->data()[1],
        tv2->data()[2], tv3->data()[0], tv3->data()[1], tv3->data()[2]);
    std::vector<double> dar(findVectorDoubleProperty(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR));
    gc.scale(1.0 / dar[0], 1.0 / dar[1], 1.0 / dar[2]);
    std::vector<double> limits(getAxisLimits());
    double xvals[8];
    double yvals[8];
    double zvals[8];
    gc.mapPoint(limits[0], limits[2], limits[4], xvals[0], yvals[0], zvals[0]);
    gc.mapPoint(limits[0], limits[2], limits[5], xvals[1], yvals[1], zvals[1]);
    gc.mapPoint(limits[0], limits[3], limits[4], xvals[2], yvals[2], zvals[2]);
    gc.mapPoint(limits[0], limits[3], limits[5], xvals[3], yvals[3], zvals[3]);
    gc.mapPoint(limits[1], limits[2], limits[4], xvals[4], yvals[4], zvals[4]);
    gc.mapPoint(limits[1], limits[2], limits[5], xvals[5], yvals[5], zvals[5]);
    gc.mapPoint(limits[1], limits[3], limits[4], xvals[6], yvals[6], zvals[6]);
    gc.mapPoint(limits[1], limits[3], limits[5], xvals[7], yvals[7], zvals[7]);
    double xmin, xmax, ymin, ymax, zmin, zmax;
    minMaxVector(xvals, 8, xmin, xmax);
    minMaxVector(yvals, 8, ymin, ymax);
    minMaxVector(zvals, 8, zmin, zmax);
    if (zmin == zmax) {
        zmin = zmax - 1;
        zmax = zmax + 1;
    }
    std::vector<double> position(getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR));
    if (stringCheck(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)
        || stringCheck(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        double xratio = (xmax - xmin) / position[2];
        double yratio = (ymax - ymin) / position[3];
        double maxratio = std::max(xratio, yratio);
        rerange(xmin, xmax, maxratio * position[2]);
        rerange(ymin, ymax, maxratio * position[3]);
    }
    gc.project(xmin, xmax, ymin, ymax, -zmax, -zmin);
    gc.viewport(position[0], position[1], position[2], position[3]);
}
//=============================================================================
std::vector<double>
GOAxis::reMap(std::vector<double> t, bool forceLinearScale)
{
    std::vector<double> s;
    s.reserve(t.size() * 3);
    for (size_t i = 0; i < t.size(); i += 3) {
        s.push_back(mapX(t[i], forceLinearScale));
        s.push_back(mapY(t[i + 1], forceLinearScale));
        if (t.size() > 2) {
            s.push_back(mapZ(t[i + 2], forceLinearScale));
        } else {
            s.push_back(mapZ(0, forceLinearScale));
        }
    }
    return s;
}
//=============================================================================
void
GOAxis::reMap(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs,
    std::vector<double>& ax, std::vector<double>& ay, std::vector<double>& az)
{
    ax.resize(xs.size());
    ay.resize(xs.size());
    az.resize(xs.size());
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)xs.size(); i++) {
        ax[i] = mapX(xs[i]);
        ay[i] = mapY(ys[i]);
        az[i] = mapZ(zs[i]);
    }
}
//=============================================================================
double
GOAxis::mapX(double x, bool forceLinearScale)
{
    GONormalReverseProperty* hp
        = static_cast<GONormalReverseProperty*>(findProperty(GO_X_DIR_PROPERTY_NAME_STR));
    GOTwoVectorProperty* xlim
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_X_LIM_PROPERTY_NAME_STR));
    std::vector<double> lims(xlim->data());
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR) && !forceLinearScale) {
        x = tickLog(x);
    }
    if (hp->isEqual(GO_PROPERTY_VALUE_REVERSE_STR)) {
        double xmin(lims[0]);
        double xmax(lims[1]);
        return (xmin + xmax - x);
    }
    return (x);
}
//=============================================================================
double
GOAxis::mapY(double y, bool forceLinearScale)
{
    GONormalReverseProperty* hp
        = static_cast<GONormalReverseProperty*>(findProperty(GO_Y_DIR_PROPERTY_NAME_STR));
    GOTwoVectorProperty* ylim
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_Y_LIM_PROPERTY_NAME_STR));
    std::vector<double> lims(ylim->data());
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR) && !forceLinearScale) {
        y = tickLog(y);
    }
    if (hp->isEqual(GO_PROPERTY_VALUE_REVERSE_STR)) {
        double ymin(lims[0]);
        double ymax(lims[1]);
        return (ymin + ymax - y);
    }
    return (y);
}
//=============================================================================
double
GOAxis::mapZ(double z, bool forceLinearScale)
{
    GONormalReverseProperty* hp
        = static_cast<GONormalReverseProperty*>(findProperty(GO_Z_DIR_PROPERTY_NAME_STR));
    GOTwoVectorProperty* zlim
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_Z_LIM_PROPERTY_NAME_STR));
    std::vector<double> lims(zlim->data());
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR) && !forceLinearScale) {
        z = tickLog(z);
    }
    if (hp->isEqual(GO_PROPERTY_VALUE_REVERSE_STR)) {
        double zmin(lims[0]);
        double zmax(lims[1]);
        return (zmin + zmax - z);
    }
    return (z);
}
//=============================================================================
}
//=============================================================================
