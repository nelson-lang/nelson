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
#include "GOList.hpp"
#include "GOFigure.hpp"
#include "GOText.hpp"
#include "GOHelpers.hpp"
#include "RenderQt.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
const int MAX_TICK_COUNT = 1000;
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
    registerProperty(new GOLineStyleOrderProperty, GO_LINE_STYLEORDER_PROPERTY_NAME_STR);
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
    GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_COLOR_ORDER_PROPERTY_NAME_STR);
    hp->data(colorsOrder);
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
    setRestrictedStringSetDefault(GO_LINE_STYLEORDER_PROPERTY_NAME_STR, L"-|--|:|-.");
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
    updateAxisFont();
}
//=============================================================================
GOAxis::~GOAxis() { }
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
    GOLinearLogProperty* lp = (GOLinearLogProperty*)findProperty(GO_X_SCALE_PROPERTY_NAME_STR);
    if (isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR)) {
        formatAxisAuto(limits[0], limits[1], xcnt, lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), xStart,
            xStop, xticks, xlabels);
        tp = (GOTwoVectorProperty*)findProperty(GO_X_LIM_PROPERTY_NAME_STR);
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
        formatAxisManual(limits[0], limits[1], xcnt, lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), xStart,
            xStop, xticks, xlabels);
    }
    lp = (GOLinearLogProperty*)findProperty(GO_Y_SCALE_PROPERTY_NAME_STR);
    if (isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR)) {
        formatAxisAuto(limits[2], limits[3], ycnt, lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), yStart,
            yStop, yticks, ylabels);
        tp = (GOTwoVectorProperty*)findProperty(GO_Y_LIM_PROPERTY_NAME_STR);
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
        formatAxisManual(limits[2], limits[3], ycnt, lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), yStart,
            yStop, yticks, ylabels);
    }

    lp = (GOLinearLogProperty*)findProperty(GO_Z_SCALE_PROPERTY_NAME_STR);
    if (isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)) {
        formatAxisAuto(limits[4], limits[5], zcnt, lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), zStart,
            zStop, zticks, zlabels);
        tp = (GOTwoVectorProperty*)findProperty(GO_Z_LIM_PROPERTY_NAME_STR);
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
        formatAxisManual(limits[4], limits[5], zcnt, lp->isEqual(GO_PROPERTY_VALUE_LOG_STR), zStart,
            zStop, zticks, zlabels);
    }
    GOVectorProperty* hp = nullptr;
    GOStringVector* qp = nullptr;
    if (isAuto(GO_X_TICK_MODE_PROPERTY_NAME_STR)) {
        hp = (GOVectorProperty*)findProperty(GO_X_TICK_PROPERTY_NAME_STR);
        hp->data(xticks);
    }
    if (isAuto(GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        qp = (GOStringVector*)findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR);
        qp->data(xlabels);
    }
    if (isAuto(GO_Y_TICK_MODE_PROPERTY_NAME_STR)) {
        hp = (GOVectorProperty*)findProperty(GO_Y_TICK_PROPERTY_NAME_STR);
        hp->data(yticks);
    }
    if (isAuto(GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        qp = (GOStringVector*)findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR);
        qp->data(ylabels);
    }
    if (isAuto(GO_Z_TICK_MODE_PROPERTY_NAME_STR)) {
        hp = (GOVectorProperty*)findProperty(GO_Z_TICK_PROPERTY_NAME_STR);
        hp->data(zticks);
    }
    if (isAuto(GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        qp = (GOStringVector*)findProperty(GO_Z_TICK_LABEL_PROPERTY_NAME_STR);
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
        ((GOLineStyleProperty*)findProperty(GO_MINOR_GRID_LINE_STYLE_PROPERTY_NAME_STR))->data());
    gc.depth(false);
    GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_X_TICK_PROPERTY_NAME_STR);
    std::vector<double> xticks(hp->data());
    hp = (GOVectorProperty*)findProperty(GO_Y_TICK_PROPERTY_NAME_STR);
    std::vector<double> yticks(hp->data());
    hp = (GOVectorProperty*)findProperty(GO_Z_TICK_PROPERTY_NAME_STR);
    std::vector<double> zticks(hp->data());
    GOColorProperty* xc = (GOColorProperty*)findProperty(GO_X_COLOR_PROPERTY_NAME_STR);
    GOColorProperty* yc = (GOColorProperty*)findProperty(GO_Y_COLOR_PROPERTY_NAME_STR);
    GOColorProperty* zc = (GOColorProperty*)findProperty(GO_Z_COLOR_PROPERTY_NAME_STR);
    GOLinearLogProperty* sp = nullptr;
    if (((GOOnOffProperty*)findProperty(GO_X_MINOR_GRID_PROPERTY_NAME_STR))->asBool()) {
        gc.color(xc->data());
        sp = (GOLinearLogProperty*)findProperty(GO_X_SCALE_PROPERTY_NAME_STR);
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
    if (((GOOnOffProperty*)findProperty(GO_Y_MINOR_GRID_PROPERTY_NAME_STR))->asBool()) {
        gc.color(yc->data());
        sp = (GOLinearLogProperty*)findProperty(GO_Y_SCALE_PROPERTY_NAME_STR);
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
    if (((GOOnOffProperty*)findProperty(GO_Z_MINOR_GRID_PROPERTY_NAME_STR))->asBool()) {
        gc.color(zc->data());
        sp = (GOLinearLogProperty*)findProperty(GO_Z_SCALE_PROPERTY_NAME_STR);
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
        ((GOLineStyleProperty*)findProperty(GO_GRID_LINE_STYLE_PROPERTY_NAME_STR))->data());
    GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_X_TICK_PROPERTY_NAME_STR);
    std::vector<double> xticks(hp->data());
    hp = (GOVectorProperty*)findProperty(GO_Y_TICK_PROPERTY_NAME_STR);
    std::vector<double> yticks(hp->data());
    hp = (GOVectorProperty*)findProperty(GO_Z_TICK_PROPERTY_NAME_STR);
    std::vector<double> zticks(hp->data());
    GOColorProperty* xc = (GOColorProperty*)findProperty(GO_X_COLOR_PROPERTY_NAME_STR);
    GOColorProperty* yc = (GOColorProperty*)findProperty(GO_Y_COLOR_PROPERTY_NAME_STR);
    GOColorProperty* zc = (GOColorProperty*)findProperty(GO_Z_COLOR_PROPERTY_NAME_STR);
    if (xvisible && ((GOOnOffProperty*)findProperty(GO_X_GRID_PROPERTY_NAME_STR))->asBool()) {
        gc.color(xc->data());
        for (int i = 0; i < xticks.size(); i++) {
            double t = mapX(xticks[i]);
            drawXGridLine(gc, t, limits);
        }
    }
    if (yvisible && ((GOOnOffProperty*)findProperty(GO_Y_GRID_PROPERTY_NAME_STR))->asBool()) {
        gc.color(yc->data());
        for (int i = 0; i < yticks.size(); i++) {
            double t = mapY(yticks[i]);
            drawYGridLine(gc, t, limits);
        }
    }
    if (zvisible && ((GOOnOffProperty*)findProperty(GO_Z_GRID_PROPERTY_NAME_STR))->asBool()) {
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
    GOLinearLogProperty* sp = (GOLinearLogProperty*)findProperty(GO_X_SCALE_PROPERTY_NAME_STR);
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, lims[0], lims[1]);
    } else {
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, pow(10.0, lims[0]), pow(10.0, lims[1]));
    }
    sp = (GOLinearLogProperty*)findProperty(GO_Y_SCALE_PROPERTY_NAME_STR);
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, lims[2], lims[3]);
    } else {
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, pow(10.0, lims[2]), pow(10.0, lims[3]));
    }
    sp = (GOLinearLogProperty*)findProperty(GO_Z_SCALE_PROPERTY_NAME_STR);
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
    GOTwoVectorProperty* hp = (GOTwoVectorProperty*)findProperty(axisLimitPropertyName);
    if (std::isfinite(hp->data()[0])) {
        limMin = hp->data()[0];
    } else {
        GOSixVectorProperty* dataLimits
            = (GOSixVectorProperty*)findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR);
        limMin = dataLimits->data()[indexMin];
    }
    if (std::isfinite(hp->data()[1])) {
        limMax = hp->data()[1];
    } else {
        GOSixVectorProperty* dataLimits
            = (GOSixVectorProperty*)findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR);
        limMax = dataLimits->data()[indexMax];
    }
}
//=============================================================================
GOFigure*
GOAxis::getParentFigure()
{
    GOGObjectsProperty* parent = (GOGObjectsProperty*)findProperty(GO_PARENT_PROPERTY_NAME_STR);
    if (parent->data().empty())
        return NULL;
    unsigned parent_handle = parent->data()[0];
    GOFigure* fig = findGOFigure(parent_handle);
    return fig;
}
//=============================================================================
std::vector<double>
GOAxis::getPropertyVectorAsPixels(const std::wstring& name)
{
    GOFourVectorProperty* hp = (GOFourVectorProperty*)findProperty(name);
    return (unitsReinterpret(hp->data()));
}
//=============================================================================
void
GOAxis::drawBox(RenderInterface& gc)
{
    GOColorProperty* hp = (GOColorProperty*)findProperty(GO_COLOR_PROPERTY_NAME_STR);
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
    if (((GOTopBottomProperty*)findProperty(GO_X_AXIS_LOCATION_PROPERTY_NAME_STR))
            ->isEqual(GO_PROPERTY_VALUE_BOTTOM_STR)) {
        x1pos[2] = limits[4];
    } else {
        x1pos[2] = limits[5];
    }
    if (((GOLeftRightProperty*)findProperty(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR))
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
        // y2pos[2] = y2pos[2];
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
    GOStringProperty* fontname = (GOStringProperty*)findProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    GOFontAngleProperty* fontangle
        = (GOFontAngleProperty*)findProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    GOFontWeightProperty* fontweight
        = (GOFontWeightProperty*)findProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    GOScalarProperty* fontsize = (GOScalarProperty*)findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
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
    GOColorProperty* xc = (GOColorProperty*)findProperty(GO_X_COLOR_PROPERTY_NAME_STR);
    GOColorProperty* yc = (GOColorProperty*)findProperty(GO_Y_COLOR_PROPERTY_NAME_STR);
    GOColorProperty* zc = (GOColorProperty*)findProperty(GO_Z_COLOR_PROPERTY_NAME_STR);
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
    GOGObjectsProperty* lbl = (GOGObjectsProperty*)findProperty(GO_X_LABEL_PROPERTY_NAME_STR);
    if (!lbl->data().empty()) {
        GOText* fp = (GOText*)findGraphicsObject(lbl->data()[0]);
        xlabelHeight = fp->getTextHeightInPixels();
    }
    GOStringVector* gp = nullptr;
    gp = (GOStringVector*)findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR);
    std::vector<std::wstring> xlabels(gp->data());
    for (int i = 0; i < xlabels.size(); i++) {
        QRect sze(fm.boundingRect(Nelson::wstringToQString(xlabels[i])));
        maxTickWidth = std::max(maxTickWidth, sze.width());
        maxTickHeight = std::max(maxTickHeight, sze.height());
    }
    lbl = (GOGObjectsProperty*)findProperty(GO_Y_LABEL_PROPERTY_NAME_STR);
    if (!lbl->data().empty()) {
        GOText* fp = (GOText*)findGraphicsObject(lbl->data()[0]);
        ylabelHeight = fp->getTextHeightInPixels();
    }
    gp = (GOStringVector*)findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR);
    std::vector<std::wstring> ylabels(gp->data());
    for (int i = 0; i < ylabels.size(); i++) {
        QRect sze(fm.boundingRect(wstringToQString(ylabels[i])));
        maxTickWidth = std::max(maxTickWidth, sze.width());
        maxTickHeight = std::max(maxTickHeight, sze.height());
    }
    lbl = (GOGObjectsProperty*)findProperty(GO_Z_LABEL_PROPERTY_NAME_STR);
    if (!lbl->data().empty()) {
        GOText* fp = (GOText*)findGraphicsObject(lbl->data()[0]);
        zlabelHeight = fp->getTextHeightInPixels();
    }
    gp = (GOStringVector*)findProperty(GO_Z_TICK_LABEL_PROPERTY_NAME_STR);
    std::vector<std::wstring> zlabels(gp->data());
    for (int i = 0; i < zlabels.size(); i++) {
        QRect sze(fm.boundingRect(wstringToQString(zlabels[i])));
        maxTickWidth = std::max(maxTickWidth, sze.width());
        maxTickHeight = std::max(maxTickHeight, sze.height());
    }
    lbl = (GOGObjectsProperty*)findProperty(GO_TITLE_PROPERTY_NAME_STR);
    if (!lbl->data().empty()) {
        GOText* fp = (GOText*)findGraphicsObject(lbl->data()[0]);
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
            = (GOFourVectorProperty*)findProperty(GO_POSITION_PROPERTY_NAME_STR);
        hp->value(
            outerpos[0] / width, outerpos[1] / height, outerpos[2] / width, outerpos[3] / height);
        return;
    }
    double posx0 = outerpos[2] * 0.1 + outerpos[0];
    double posy0 = outerpos[3] * 0.1 + outerpos[1];
    double poswidth = outerpos[2] * 0.8;
    double posheight = outerpos[3] * .8;
    maxLabelHeight = (int)(maxLabelHeight * 1.2 + tickHeight);
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
    GOFourVectorProperty* hp = (GOFourVectorProperty*)findProperty(GO_POSITION_PROPERTY_NAME_STR);
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
    GOGObjectsProperty* children = (GOGObjectsProperty*)findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> handles(children->data());
    for (int i = 0; i < handles.size(); i++) {
        GraphicsObject* fp = findGraphicsObject(handles[i]);
        std::vector<double> child_limits(fp->getLimits());
        if (!child_limits.empty()) {
            if (first) {
                limits = child_limits;
                first = false;
            } else {
                for (int i = 0; i < std::min(limits.size(), child_limits.size()); i += 2) {
                    limits[i] = std::min(limits[i], child_limits[i]);
                    limits[i + 1] = std::max(limits[i + 1], child_limits[i + 1]);
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
    GOSixVectorProperty* hp = (GOSixVectorProperty*)findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR);
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
        GOGObjectsProperty* lbl = (GOGObjectsProperty*)findProperty(labelname);
        if (!lbl->data().empty()) {
            GOText* fp = (GOText*)findGraphicsObject(lbl->data()[0]);
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
                = (GOScalarProperty*)fp->findProperty(GO_ROTATION_PROPERTY_NAME_STR);
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
                    ((GOAlignVertProperty*)fp->findProperty(
                         GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR))
                        ->data(GO_PROPERTY_VALUE_TOP_STR);
                } else {

                    ((GOAlignVertProperty*)fp->findProperty(
                         GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR))
                        ->data(GO_PROPERTY_VALUE_BOTTOM_STR);
                }
            } else {
                ((GOAlignVertProperty*)fp->findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR))
                    ->data(GO_PROPERTY_VALUE_BOTTOM_STR);
            }
            if ((angle == -90) && (angle2 == -180)) {
                angle = 90;
            }
            sp->data(angle);
            xl1 += (x3 - origx) * 0.04;
            yl1 += (y3 - origy) * 0.04;
            GOThreeVectorProperty* gp
                = (GOThreeVectorProperty*)fp->findProperty(GO_POSITION_PROPERTY_NAME_STR);
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
        lbl = (GOGObjectsProperty*)findProperty(GO_X_LABEL_PROPERTY_NAME_STR);
        if (!lbl->data().empty()) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            fp->paintMe(gc);
        }
    }
    if (yvisible) {
        lbl = (GOGObjectsProperty*)findProperty(GO_Y_LABEL_PROPERTY_NAME_STR);
        if (!lbl->data().empty()) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            fp->paintMe(gc);
        }
    }
    if (zvisible) {
        lbl = (GOGObjectsProperty*)findProperty(GO_Z_LABEL_PROPERTY_NAME_STR);
        if (!lbl->data().empty()) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            fp->paintMe(gc);
        }
    }
    lbl = (GOGObjectsProperty*)findProperty(GO_TITLE_PROPERTY_NAME_STR);
    if (!lbl->data().empty()) {
        GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
        fp->paintMe(gc);
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
    hp = (GOVectorProperty*)findProperty(GO_X_TICK_PROPERTY_NAME_STR);
    std::vector<double> xticks(hp->data());
    hp = (GOVectorProperty*)findProperty(GO_Y_TICK_PROPERTY_NAME_STR);
    std::vector<double> yticks(hp->data());
    hp = (GOVectorProperty*)findProperty(GO_Z_TICK_PROPERTY_NAME_STR);
    std::vector<double> zticks(hp->data());
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    GOColorProperty* xc = (GOColorProperty*)findProperty(GO_X_COLOR_PROPERTY_NAME_STR);
    GOColorProperty* yc = (GOColorProperty*)findProperty(GO_Y_COLOR_PROPERTY_NAME_STR);
    GOColorProperty* zc = (GOColorProperty*)findProperty(GO_Z_COLOR_PROPERTY_NAME_STR);

    std::vector<double> position(getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR));
    int maxlen = (int)((position[2] > position[3]) ? position[2] : position[3]);
    GOTwoVectorProperty* kp = (GOTwoVectorProperty*)findProperty(GO_TICK_LENGTH_PROPERTY_NAME_STR);
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
        if (((GOInOutProperty*)findProperty(GO_TICK_DIR_PROPERTY_NAME_STR))
                ->isEqual(GO_PROPERTY_VALUE_IN_STR)) {
            ticdir = 1;
        } else {
            ticdir = -1;
        }
    }
    GOStringVector* qp = (GOStringVector*)findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR);
    std::vector<std::wstring> xlabeltxt(qp->data());
    qp = (GOStringVector*)findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR);
    std::vector<std::wstring> ylabeltxt(qp->data());
    qp = (GOStringVector*)findProperty(GO_Z_TICK_LABEL_PROPERTY_NAME_STR);
    std::vector<std::wstring> zlabeltxt(qp->data());
    std::vector<double> limits(getAxisLimits());
    gc.setLineStyle(L"-");

    if (xvisible) {
        std::vector<double> mapticks;
        for (int i = 0; i < xticks.size(); i++) {
            mapticks.push_back(mapX(xticks[i]));
        }
        std::vector<double> minorticks;
        GOLinearLogProperty* sp = (GOLinearLogProperty*)findProperty(GO_X_SCALE_PROPERTY_NAME_STR);
        if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR)) {
            for (int i = 0; i < xticks.size() - 1; i++) {
                double t1 = xticks[i];
                double t2 = xticks[i + 1];
                int n = 2;
                while (((t1 * n) < t2) && (n < MAX_TICK_COUNT)) {
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
        for (int i = 0; i < yticks.size(); i++) {
            mapticks.push_back(mapY(yticks[i]));
        }
        std::vector<double> minorticks;
        GOLinearLogProperty* sp = (GOLinearLogProperty*)findProperty(GO_Y_SCALE_PROPERTY_NAME_STR);
        if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR)) {
            for (int i = 0; i < yticks.size() - 1; i++) {
                double t1 = yticks[i];
                double t2 = yticks[i + 1];
                int n = 2;
                while (((t1 * n) < t2) && (n < MAX_TICK_COUNT)) {
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
        for (int i = 0; i < zticks.size(); i++) {
            mapticks.push_back(mapZ(zticks[i]));
        }
        std::vector<double> minorticks;
        GOLinearLogProperty* sp = (GOLinearLogProperty*)findProperty(GO_Z_SCALE_PROPERTY_NAME_STR);
        if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR)) {
            for (int i = 0; i < zticks.size() - 1; i++) {
                double t1 = zticks[i];
                double t2 = zticks[i + 1];
                int n = 2;
                while (((t1 * n) < t2) && (n < MAX_TICK_COUNT)) {
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
    GOGObjectsProperty* children = (GOGObjectsProperty*)findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> handles(children->data());
    for (int i = 0; i < handles.size(); i++) {
        GraphicsObject* fp = findGraphicsObject(handles[i]);
        fp->paintMe(gc);
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
            = (GOThreeVectorProperty*)findProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR);
        std::vector<double> limits(getAxisLimits());
        tv->value((limits[0] + limits[1]) / 2.0, (limits[2] + limits[3]) / 2.0,
            (limits[4] + limits[5]) / 2.0);
    }
    if (hasChanged(GO_CAMERA_POSITION_PROPERTY_NAME_STR)) {
        toManual(GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR)) {
        GOThreeVectorProperty* tv
            = (GOThreeVectorProperty*)findProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR);
        std::vector<double> limits(getAxisLimits());
        tv->value((limits[0] + limits[1]) / 2.0, (limits[2] + limits[3]) / 2.0, limits[5] + 1);
    }
    if (hasChanged(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR)) {
        toManual(GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR)) {
        GOThreeVectorProperty* tv
            = (GOThreeVectorProperty*)findProperty(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR);
        tv->value(0, 1, 0);
    }

    GOGObjectsProperty* children = (GOGObjectsProperty*)findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
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
    GOUnitsProperty* hp = (GOUnitsProperty*)findProperty(GO_UNITS_PROPERTY_NAME_STR);
    if (hp->isEqual(GO_PROPERTY_VALUE_NORMALIZED_STR)) {
        for (int i = 0; i < a.size(); i += 2) {
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
    GOLinearLogProperty* sp = (GOLinearLogProperty*)findProperty(GO_X_SCALE_PROPERTY_NAME_STR);
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
    sp = (GOLinearLogProperty*)findProperty(GO_Y_SCALE_PROPERTY_NAME_STR);
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
    sp = (GOLinearLogProperty*)findProperty(GO_Z_SCALE_PROPERTY_NAME_STR);
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
        = (GOThreeVectorProperty*)findProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR);
    GOThreeVectorProperty* tv2
        = (GOThreeVectorProperty*)findProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR);
    GOThreeVectorProperty* tv3
        = (GOThreeVectorProperty*)findProperty(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR);
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
GOAxis::reMap(std::vector<double> t)
{
    std::vector<double> s;
    s.reserve(t.size() * 3);
    for (size_t i = 0; i < t.size(); i += 3) {
        s.push_back(mapX(t[i]));
        s.push_back(mapY(t[i + 1]));
        if (t.size() > 2) {
            s.push_back(mapZ(t[i + 2]));
        } else {
            s.push_back(mapZ(0));
        }
    }
    return s;
}
//=============================================================================
void
GOAxis::reMap(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs,
    std::vector<double>& ax, std::vector<double>& ay, std::vector<double>& az)
{
    ax.reserve(xs.size());
    ay.reserve(xs.size());
    az.reserve(xs.size());
    for (size_t i = 0; i < xs.size(); i++) {
        ax.push_back(mapX(xs[i]));
        ay.push_back(mapY(ys[i]));
        az.push_back(mapZ(zs[i]));
    }
}
//=============================================================================
double
GOAxis::mapX(double x)
{
    GONormalReverseProperty* hp
        = (GONormalReverseProperty*)findProperty(GO_X_DIR_PROPERTY_NAME_STR);
    GOTwoVectorProperty* xlim = (GOTwoVectorProperty*)findProperty(GO_X_LIM_PROPERTY_NAME_STR);
    std::vector<double> lims(xlim->data());
    GOLinearLogProperty* sp = (GOLinearLogProperty*)findProperty(GO_X_SCALE_PROPERTY_NAME_STR);
    if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR)) {
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
GOAxis::mapY(double y)
{
    GONormalReverseProperty* hp
        = (GONormalReverseProperty*)findProperty(GO_Y_DIR_PROPERTY_NAME_STR);
    GOTwoVectorProperty* ylim = (GOTwoVectorProperty*)findProperty(GO_Y_LIM_PROPERTY_NAME_STR);
    std::vector<double> lims(ylim->data());
    GOLinearLogProperty* sp = (GOLinearLogProperty*)findProperty(GO_Y_SCALE_PROPERTY_NAME_STR);
    if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR)) {
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
GOAxis::mapZ(double z)
{
    GONormalReverseProperty* hp
        = (GONormalReverseProperty*)findProperty(GO_Z_DIR_PROPERTY_NAME_STR);
    GOTwoVectorProperty* zlim = (GOTwoVectorProperty*)findProperty(GO_Z_LIM_PROPERTY_NAME_STR);
    std::vector<double> lims(zlim->data());
    GOLinearLogProperty* sp = (GOLinearLogProperty*)findProperty(GO_Z_SCALE_PROPERTY_NAME_STR);
    if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR)) {
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
