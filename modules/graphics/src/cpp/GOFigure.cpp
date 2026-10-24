//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <math.h>
#include <algorithm>
#include <cmath>
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOFigure.hpp"
#include "RenderInterface.hpp"
#include "GOList.hpp"
#include "GOFigureHost.hpp"
#include "GONextPlotModeProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOColorVectorProperty.hpp"
#include "GOColorProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "GOFiguresManager.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOToolBarProperty.hpp"
#include "GOMenuBarProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GraphicsEnvironment.hpp"
#include "NelsonConfiguration.hpp"
#include "GOCallbackProperty.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "CallbackQueue.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOWindowStateProperty.hpp"
#include "GOCommonProperties.hpp"
#include "GOAxis.hpp"
#include "GOAnnotation.hpp"
#include "GOColorBar.hpp"
#include "GOBubbleLegend.hpp"
#include "GOLegend.hpp"
#include "GOUIComponent.h"
#include "StringHelpers.hpp"
#include "UnitsConversionHelpers.hpp"
#include "GOPositionHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr double FIGURE_LIGHT_COLOR = GOFigure::LIGHT_BACKGROUND_GRAY;
static constexpr double FIGURE_DARK_COLOR = GOFigure::DARK_BACKGROUND_GRAY;
static constexpr double AXES_LIGHT_COLOR = 1.0;
static constexpr double AXES_DARK_COLOR = 18.0 / 255.0;
static constexpr double AXES_LIGHT_FOREGROUND = 33.0 / 255.0;
static constexpr double AXES_DARK_FOREGROUND = 217.0 / 255.0;
static constexpr double THEME_COLOR_TOLERANCE = 1e-12;
static const wchar_t* FIGURE_HANDLE_VISIBILITY_DICT[4]
    = { GO_PROPERTY_VALUE_ON_STR, L"callback", GO_PROPERTY_VALUE_OFF_STR, 0 };
//=============================================================================
std::vector<double>
defaultFigurePosition()
{
    // Scale the default figure with the screen. It is horizontally centered with
    // its bottom edge one sixth of the screen height above the bottom. Width and
    // height keep the screen fractions of the calibration screen (859x517 on
    // 1536x864), which gives [338 144 859 517] there.
    int screenWidth = 0;
    int screenHeight = 0;
    if (GraphicsEnvironment::currentScreenResolution(screenWidth, screenHeight) && screenWidth > 0
        && screenHeight > 0) {
        double width = std::round(static_cast<double>(screenWidth) * GO_DEFAULT_FIGURE_WIDTH
            / GO_DEFAULT_FIGURE_REFERENCE_SCREEN_WIDTH);
        double height = std::round(static_cast<double>(screenHeight) * GO_DEFAULT_FIGURE_HEIGHT
            / GO_DEFAULT_FIGURE_REFERENCE_SCREEN_HEIGHT);
        double x = std::floor((static_cast<double>(screenWidth) - width) / 2.0);
        double y = std::round(static_cast<double>(screenHeight) / 6.0);
        if (x < 0.0) {
            x = 0.0;
        }
        if (y < 0.0) {
            y = 0.0;
        }
        return { x, y, width, height };
    }
    // Screen size unknown (headless, no screen hook): use fixed values so layout
    // math and getframe stay deterministic.
    return { GO_DEFAULT_FIGURE_X, GO_DEFAULT_FIGURE_Y, GO_DEFAULT_FIGURE_WIDTH,
        GO_DEFAULT_FIGURE_HEIGHT };
}
//=============================================================================
static bool
isColorClose(const std::vector<double>& color, double red, double green, double blue)
{
    if (color.size() < 3) {
        return false;
    }
    return std::fabs(color[0] - red) <= THEME_COLOR_TOLERANCE
        && std::fabs(color[1] - green) <= THEME_COLOR_TOLERANCE
        && std::fabs(color[2] - blue) <= THEME_COLOR_TOLERANCE;
}
//=============================================================================
static void
setColorWithoutManualMode(
    GraphicsObject* object, const std::wstring& name, double red, double green, double blue)
{
    object->setThreeVectorDefault(name, red, green, blue);
    object->clearChanged(name);
}
//=============================================================================
static bool
isThemeManagedFigureColor(const std::vector<double>& color)
{
    return isColorClose(color, FIGURE_LIGHT_COLOR, FIGURE_LIGHT_COLOR, FIGURE_LIGHT_COLOR)
        || isColorClose(color, FIGURE_DARK_COLOR, FIGURE_DARK_COLOR, FIGURE_DARK_COLOR);
}
//=============================================================================
static bool
isThemeManagedAxesColor(const std::vector<double>& color)
{
    return isColorClose(color, AXES_LIGHT_COLOR, AXES_LIGHT_COLOR, AXES_LIGHT_COLOR)
        || isColorClose(color, AXES_DARK_COLOR, AXES_DARK_COLOR, AXES_DARK_COLOR);
}
//=============================================================================
static bool
isFourVector(const std::vector<double>& value)
{
    return value.size() == 4;
}
//=============================================================================
static bool
usesNormalizedUnits(const std::wstring& previousUnits, const std::wstring& currentUnits)
{
    return previousUnits == GO_PROPERTY_VALUE_NORMALIZED_STR
        || currentUnits == GO_PROPERTY_VALUE_NORMALIZED_STR;
}
//=============================================================================
static void
syncFigureUnitsIfChanged(GOFigure* fig)
{
    if (!fig || !fig->hasChanged(GO_UNITS_PROPERTY_NAME_STR)) {
        return;
    }
    auto* unitsProperty
        = static_cast<GOUnitsProperty*>(fig->findProperty(GO_UNITS_PROPERTY_NAME_STR));
    std::wstring previousUnits = unitsProperty->getPreviousUnits();
    std::wstring currentUnits = unitsProperty->data();
    if (previousUnits == currentUnits) {
        return;
    }
    int screenWidth = 0;
    int screenHeight = 0;
    GraphicsEnvironment::currentScreenResolution(screenWidth, screenHeight);
    if (usesNormalizedUnits(previousUnits, currentUnits)
        && (screenWidth <= 0 || screenHeight <= 0)) {
        return;
    }
    syncPositionUnitsTriplet(fig, previousUnits, currentUnits, screenWidth, screenHeight);
}
//=============================================================================
static void
syncRenderingSizeFromPosition(GOFigure* fig)
{
    if (!fig) {
        return;
    }
    std::vector<double> requested = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (requested.size() < 4) {
        return;
    }
    int screenWidth = 0, screenHeight = 0;
    GraphicsEnvironment::currentScreenResolution(screenWidth, screenHeight);
    std::vector<double> pixels = convertUnitsVector(
        fig->findStringProperty(GO_UNITS_PROPERTY_NAME_STR), GO_PROPERTY_VALUE_PIXELS_STR,
        requested[0], requested[1], requested[2], requested[3], screenWidth, screenHeight);
    if (pixels.size() < 4 || pixels[2] <= 0 || pixels[3] <= 0) {
        return;
    }
    fig->setRenderingSize(
        static_cast<int>(std::lround(pixels[2])), static_cast<int>(std::lround(pixels[3])));
}
//=============================================================================
static std::wstring
normalizeThemeName(const std::wstring& theme)
{
    std::wstring normalized(theme);
    StringHelpers::to_lower(normalized);
    if (normalized == GO_PROPERTY_VALUE_LIGHT_STR || normalized == GO_PROPERTY_VALUE_DARK_STR) {
        return normalized;
    }
    Error(L"Nelson:figure:invalidTheme", L"Specify \"light\" or \"dark\".");
    return GO_PROPERTY_VALUE_LIGHT_STR;
}
//=============================================================================
static void
applyThemeToAxes(GOAxis* axis, bool darkTheme)
{
    if (!axis) {
        return;
    }
    const double axesColor = darkTheme ? AXES_DARK_COLOR : AXES_LIGHT_COLOR;
    const double foreground = darkTheme ? AXES_DARK_FOREGROUND : AXES_LIGHT_FOREGROUND;
    const double gridColor = darkTheme ? AXES_LIGHT_COLOR : AXES_LIGHT_FOREGROUND;
    if (!axis->hasChanged(GO_COLOR_PROPERTY_NAME_STR)
        && isThemeManagedAxesColor(axis->findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR))) {
        setColorWithoutManualMode(
            axis, GO_COLOR_PROPERTY_NAME_STR, axesColor, axesColor, axesColor);
    }
    if (!axis->hasChanged(GO_X_COLOR_PROPERTY_NAME_STR)
        && axis->isAuto(GO_X_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorWithoutManualMode(
            axis, GO_X_COLOR_PROPERTY_NAME_STR, foreground, foreground, foreground);
    }
    if (!axis->hasChanged(GO_Y_COLOR_PROPERTY_NAME_STR)
        && axis->isAuto(GO_Y_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorWithoutManualMode(
            axis, GO_Y_COLOR_PROPERTY_NAME_STR, foreground, foreground, foreground);
    }
    if (!axis->hasChanged(GO_Z_COLOR_PROPERTY_NAME_STR)
        && axis->isAuto(GO_Z_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorWithoutManualMode(
            axis, GO_Z_COLOR_PROPERTY_NAME_STR, foreground, foreground, foreground);
    }
    if (!axis->hasChanged(GO_GRID_COLOR_PROPERTY_NAME_STR)
        && axis->isAuto(GO_GRID_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorWithoutManualMode(
            axis, GO_GRID_COLOR_PROPERTY_NAME_STR, gridColor, gridColor, gridColor);
    }
    if (!axis->hasChanged(GO_MINOR_GRID_COLOR_PROPERTY_NAME_STR)
        && axis->isAuto(GO_MINOR_GRID_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorWithoutManualMode(
            axis, GO_MINOR_GRID_COLOR_PROPERTY_NAME_STR, gridColor, gridColor, gridColor);
    }
}
//=============================================================================
static void
applyThemeToColorBar(GOColorBar* colorBar, bool darkTheme)
{
    if (!colorBar || colorBar->hasChanged(GO_COLOR_PROPERTY_NAME_STR)) {
        return;
    }
    const double foreground = darkTheme ? AXES_DARK_FOREGROUND : AXES_LIGHT_FOREGROUND;
    setColorWithoutManualMode(
        colorBar, GO_COLOR_PROPERTY_NAME_STR, foreground, foreground, foreground);
}
//=============================================================================
std::wstring
GOFigure::getType()
{
    return GO_PROPERTY_VALUE_FIGURE_STR;
}
//=============================================================================
GOFigure::GOFigure(GOFigureHost* win, int number)
{
    std::vector<double> position = defaultFigurePosition();
    m_width = static_cast<int>(position[2]);
    m_height = static_cast<int>(position[3]);
    m_win = win;
    registerProperties();
    initializeProperties();
    setScalarDoubleDefault(GO_NUMBER_PROPERTY_NAME_STR, (double)number);
}
//=============================================================================
GOFigureHost*
GOFigure::getHost()
{
    return m_win;
}
//=============================================================================
void
GOFigure::registerProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_NUMBER_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_ALPHA_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorVectorProperty, GO_COLOR_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CURRENT_AXES_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CURRENT_OBJECT_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR, false);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_NAME_PROPERTY_NAME_STR);
    registerProperty(new GONextPlotModeProperty, GO_NEXT_PLOT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_DRAW_LATER_PROPERTY_NAME_STR, true, false);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_NUMBER_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_GRAPHICS_SMOOTHING_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(FIGURE_HANDLE_VISIBILITY_DICT),
        GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOToolBarProperty, GO_TOOL_BAR_PROPERTY_NAME_STR);
    registerProperty(new GOMenuBarProperty, GO_MENU_BAR_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CLOSE_REQUEST_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_RESIZE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOWindowStateProperty, GO_WINDOW_STATE_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOFigure::initializeProperties()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setThreeVectorDefault(
        GO_COLOR_PROPERTY_NAME_STR, FIGURE_LIGHT_COLOR, FIGURE_LIGHT_COLOR, FIGURE_LIGHT_COLOR);
    setStringDefault(GO_NEXT_PLOT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ADD_STR);
    setGoProperty(GO_PARENT_PROPERTY_NAME_STR, HANDLE_ROOT_OBJECT);
    setStringDefault(GO_NAME_PROPERTY_NAME_STR, {});
    std::vector<double> position = defaultFigurePosition();
    setFourVectorDefault(
        GO_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
    setRestrictedStringDefault(GO_DRAW_LATER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_GRAPHICS_SMOOTHING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_TOOL_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_MENU_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FIGURE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_WINDOW_STATE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_RESIZE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    // Default the device pixel ratio to the display scale, so a figure captured
    // without a realized window reports the resolution the Qt backend uses. A
    // realized window later overwrites it with the exact widget ratio. It stays 1.0
    // when no windowing backend is registered.
    setScalarDoubleDefault(
        GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, GraphicsEnvironment::frameCaptureDeviceRatio());
    GOGenericProperty* hp = findProperty(GO_CLOSE_REQUEST_FCN_PROPERTY_NAME_STR);
    if (hp) {
        hp->set(ArrayOf::characterArray(GO_PROPERTY_VALUE_CLOSEREQ_STR));
    }
    hp = findProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    if (hp) {
        hp->set(ArrayOf::characterArray(L""));
    }
    loadDefaultAlphaMap();
    loadDefaultColorMap();
    initializeCommonProperties(this);
    _resized = false;
}
//=============================================================================
void
GOFigure::setFocus()
{
    if (m_win && NelsonConfiguration::getInstance()->isCurrentFigureOnClick()) {
        notifyCurrentFigureChanged(m_win->hostId());
    }
}
//=============================================================================
static void
updateFigureLegends(GOFigure* fig)
{
    if (!fig) {
        return;
    }
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());
    for (int64 handle : handles) {
        GraphicsObject* fp = findGraphicsObject(handle, false);
        GOLegend* legend = dynamic_cast<GOLegend*>(fp);
        if (legend) {
            legend->updateState();
        }
    }
}
//=============================================================================
void
GOFigure::repaint()
{
    repaintInternal(false);
}
//=============================================================================
void
GOFigure::repaintNow()
{
    repaintInternal(true);
}
//=============================================================================
void
GOFigure::repaintInternal(bool immediate)
{
    if (!m_win) {
        return;
    }
    updateFigureLegends(this);
    if (isDrawLater()) {
        setRenderingStateInvalid(true);
        return;
    }
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        setRenderingStateInvalid(true);
        return;
    }
    if (immediate) {
        m_win->requestHostRepaintNow();
    } else {
        m_win->requestHostRepaint();
    }
}
//=============================================================================
bool
GOFigure::isDrawLater()
{
    GOOnOffProperty* drawLaterProperty
        = static_cast<GOOnOffProperty*>(findProperty(GO_DRAW_LATER_PROPERTY_NAME_STR, false));
    return drawLaterProperty && drawLaterProperty->asBool();
}
//=============================================================================
void
GOFigure::setTheme(const std::wstring& theme, bool manualMode)
{
    std::wstring normalized = normalizeThemeName(theme);
    std::wstring previousTheme = findStringProperty(GO_THEME_PROPERTY_NAME_STR);
    setStringDefault(GO_THEME_PROPERTY_NAME_STR, normalized);
    if (manualMode) {
        setRestrictedStringDefault(GO_THEME_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    applyTheme();
    if (previousTheme != normalized) {
        GOCallbackProperty* callback = static_cast<GOCallbackProperty*>(
            findProperty(GO_THEME_CHANGED_FCN_PROPERTY_NAME_STR));
        if (callback) {
            callback->executeNow(this, L"ThemeChangedData", L"ThemeChanged");
        }
    }
}
//=============================================================================
void
GOFigure::setThemeMode(const std::wstring& mode)
{
    GOGenericProperty* modeProperty = findProperty(GO_THEME_MODE_PROPERTY_NAME_STR);
    modeProperty->set(ArrayOf::characterArray(mode));
    if (stringCheck(GO_THEME_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR)) {
        setTheme(GO_PROPERTY_VALUE_LIGHT_STR, false);
    } else {
        applyTheme();
    }
}
//=============================================================================
void
GOFigure::applyThemeToObject(GraphicsObject* object)
{
    if (!object) {
        return;
    }
    const bool darkTheme = stringCheck(GO_THEME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DARK_STR);
    GOAxis* axis = dynamic_cast<GOAxis*>(object);
    if (axis) {
        applyThemeToAxes(axis, darkTheme);
        return;
    }
    GOColorBar* colorBar = dynamic_cast<GOColorBar*>(object);
    if (colorBar) {
        applyThemeToColorBar(colorBar, darkTheme);
    }
}
//=============================================================================
void
GOFigure::applyTheme()
{
    const bool darkTheme = stringCheck(GO_THEME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DARK_STR);
    const double figureColor = darkTheme ? FIGURE_DARK_COLOR : FIGURE_LIGHT_COLOR;
    if (isThemeManagedFigureColor(findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR))) {
        setColorWithoutManualMode(
            this, GO_COLOR_PROPERTY_NAME_STR, figureColor, figureColor, figureColor);
    }
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    for (int64 handle : children->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        applyThemeToObject(child);
    }
    setRenderingStateInvalid(true);
    if (!isDrawLater()) {
        repaint();
    }
}
//=============================================================================
void
HSVRAMP(double h, double& r, double& g, double& b)
{
    int i;
    double f, p, q, t;
    h *= 6;
    i = (int)floor(h);
    f = h - i;
    p = 0;
    q = 1 - f;
    t = f;
    switch (i) {
    case 0:
        r = 1;
        g = t;
        b = p;
        break;
    case 1:
        r = q;
        g = 1;
        b = p;
        break;
    case 2:
        r = p;
        g = 1;
        b = t;
        break;
    case 3:
        r = p;
        g = q;
        b = 1;
        break;
    case 4:
        r = t;
        g = p;
        b = 1;
        break;
    default:
        r = 1;
        g = p;
        b = q;
        break;
    }
}
//=============================================================================
static void
updateResizeChildren(GOFigure* fig);
//=============================================================================
void
GOFigure::updateState(bool forceUpdate)
{
    // Capture before the host consumes it: the web desktop reacts to a runtime
    // WindowStyle change by moving the figure between a native window and a docked
    // panel. Cleared here so a later repaint does not re-fire it.
    const bool windowStyleChanged = hasChanged(GO_WINDOW_STYLE_PROPERTY_NAME_STR);
    const bool visibleChanged = hasChanged(GO_VISIBLE_PROPERTY_NAME_STR);
    const bool titleChanged
        = hasChanged(GO_NAME_PROPERTY_NAME_STR) || hasChanged(GO_NUMBER_TITLE_PROPERTY_NAME_STR);
    const bool positionChanged = hasChanged(GO_POSITION_PROPERTY_NAME_STR)
        || hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR)
        || hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    m_win->updateHostState(forceUpdate);
    if (windowStyleChanged) {
        notifyWebFigureWindowStyleChanged(this);
        clearChanged(GO_WINDOW_STYLE_PROPERTY_NAME_STR);
    }
    if (visibleChanged) {
        // Web desktop: show or hide the native window. The notify clears the changed
        // flag in web mode only.
        notifyWebFigureVisibleChanged(this);
    }
    if (titleChanged) {
        notifyWebFigureTitleChanged(this);
    }
    updateFigureLegends(this);
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());
    if (hasChanged(GO_COLOR_MAP_PROPERTY_NAME_STR) || forceUpdate) {
        ArrayOf colormapAsArrayOf = findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR)->get();
        for (size_t i = 0; i < handles.size(); i++) {
            GraphicsObject* fp = findGraphicsObject(handles[i]);
            if (fp->haveProperty(GO_COLOR_MAP_PROPERTY_NAME_STR)) {
                fp->findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR)->set(colormapAsArrayOf);
                fp->updateState();
            }
        }
        clearChanged(GO_COLOR_MAP_PROPERTY_NAME_STR);
    }
    refreshPositionProperties(forceUpdate);
    if (positionChanged || m_childrenLayoutStale) {
        m_childrenLayoutStale = false;
        updateResizeChildren(this);
    }
    refreshDrawLaterProperty(forceUpdate);
}
//=============================================================================
void
GOFigure::updateState()
{
    m_win->updateHostState(false);
    this->updateState(false);
}
//=============================================================================
void
GOFigure::loadDefaultAlphaMap()
{
    GORoot* groot = getGraphicsRootObject();
    if (groot) {
        GOVectorProperty* hvDefault
            = (GOVectorProperty*)groot->findProperty(GO_DEFAULT_FIGURE_ALPHAMAP_PROPERTY_NAME_STR);
        GOVectorProperty* hv = (GOVectorProperty*)findProperty(GO_ALPHA_MAP_PROPERTY_NAME_STR);
        hv->data(hvDefault->data());
    }
}
//=============================================================================
void
GOFigure::loadDefaultColorMap()
{
    GORoot* groot = getGraphicsRootObject();
    if (groot) {
        GOVectorProperty* hvDefault
            = (GOVectorProperty*)groot->findProperty(GO_DEFAULT_FIGURE_COLORMAP_PROPERTY_NAME_STR);
        GOColorVectorProperty* hv
            = (GOColorVectorProperty*)findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
        hv->data(hvDefault->data());
    }
}
//=============================================================================
void
GOFigure::paintMe(RenderInterface& gc)
{
    GOOnOffProperty* drawLaterProperty
        = static_cast<GOOnOffProperty*>(findProperty(GO_DRAW_LATER_PROPERTY_NAME_STR));
    if (!drawLaterProperty->asBool()) {
        GOOnOffProperty* graphicsSmoothingProperty
            = static_cast<GOOnOffProperty*>(findProperty(GO_GRAPHICS_SMOOTHING_PROPERTY_NAME_STR));
        gc.setGraphicsSmoothing(graphicsSmoothingProperty->asBool());
        GOColorProperty* color
            = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
        gc.clear(color->data());
        GOGObjectsProperty* children
            = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
        GOGObjectsProperty* currentAxes
            = static_cast<GOGObjectsProperty*>(findProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR));
        std::vector<int64> handlesCurrentAxes(currentAxes->data());
        std::vector<int64> handlesChildren(children->data());
        std::vector<GraphicsObject*> colorBarObjects;
        std::vector<GraphicsObject*> legendObjects;
        std::vector<GraphicsObject*> annotationObjects;
        std::vector<GraphicsObject*> uiControlObjects;

        // Fold in children attached since the last recompute before the
        // colorbars/legends read the axes limits: their updateState() runs ahead of
        // the axes' own paintMe, so a dirty axes would hand them stale values.
        for (int64 childHandle : handlesChildren) {
            GOAxis* axis = dynamic_cast<GOAxis*>(findGraphicsObject(childHandle, false));
            if (axis) {
                axis->updateStateIfDirty();
            }
        }

        for (int64 childHandle : handlesChildren) {
            GraphicsObject* fp = findGraphicsObject(childHandle, false);
            GOColorBar* colorBar = dynamic_cast<GOColorBar*>(fp);
            if (colorBar) {
                colorBar->updateState();
            }
            GOLegend* legend = dynamic_cast<GOLegend*>(fp);
            if (legend) {
                legend->updateState();
            }
            GOBubbleLegend* bubbleLegend = dynamic_cast<GOBubbleLegend*>(fp);
            if (bubbleLegend) {
                bubbleLegend->updateState();
            }
        }

        for (ompIndexType i = 0; i < (ompIndexType)handlesChildren.size(); i++) {
            if (handlesCurrentAxes.empty()) {
                GraphicsObject* fp = findGraphicsObject(handlesChildren[i], false);
                if (fp) {
                    if (fp->getType() == GO_PROPERTY_VALUE_UICONTROL_STR
                        || dynamic_cast<GOUIComponent*>(fp) != nullptr) {
                        uiControlObjects.push_back(fp);
                    } else if (dynamic_cast<GOAnnotation*>(fp)) {
                        annotationObjects.push_back(fp);
                    } else if (fp->getType() == GO_PROPERTY_VALUE_COLORBAR_STR) {
                        colorBarObjects.push_back(fp);
                    } else if (fp->getType() == GO_PROPERTY_VALUE_LEGEND_STR
                        || fp->getType() == GO_PROPERTY_VALUE_BUBBLE_LEGEND_STR) {
                        legendObjects.push_back(fp);
                    } else {
                        fp->paintMe(gc);
                    }
                }
            } else if (handlesChildren[i] != handlesCurrentAxes[0]) {
                GraphicsObject* fp = findGraphicsObject(handlesChildren[i], false);
                if (fp) {
                    if (fp->getType() == GO_PROPERTY_VALUE_UICONTROL_STR
                        || dynamic_cast<GOUIComponent*>(fp) != nullptr) {
                        uiControlObjects.push_back(fp);
                    } else if (dynamic_cast<GOAnnotation*>(fp)) {
                        annotationObjects.push_back(fp);
                    } else if (fp->getType() == GO_PROPERTY_VALUE_COLORBAR_STR) {
                        colorBarObjects.push_back(fp);
                    } else if (fp->getType() == GO_PROPERTY_VALUE_LEGEND_STR
                        || fp->getType() == GO_PROPERTY_VALUE_BUBBLE_LEGEND_STR) {
                        legendObjects.push_back(fp);
                    } else {
                        fp->paintMe(gc);
                        GOStringProperty* ft = static_cast<GOStringProperty*>(
                            fp->findProperty(GO_TAG_PROPERTY_NAME_STR, false));
                        GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                            fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
                        if (ft && fv) {
                            if (ft->data() == L"legend" && fv->data() == GO_PROPERTY_VALUE_ON_STR) {
                                legendObjects.push_back(fp);
                            }
                        }
                    }
                }
            }
        }
        // The current axes is painted last so it lands on top, but only when the
        // figure holds it. An axes inside a uipanel or a uitab is painted by that
        // container in its own coordinates; painting it here too laid a second copy
        // over the whole figure.
        if (!handlesCurrentAxes.empty()
            && std::find(handlesChildren.begin(), handlesChildren.end(), handlesCurrentAxes[0])
                != handlesChildren.end()) {
            GraphicsObject* fp = findGraphicsObject(handlesCurrentAxes[0], false);
            if (fp) {
                fp->paintMe(gc);
            }
        }
        if (!colorBarObjects.empty()) {
            for (auto colorBar : colorBarObjects) {
                if (colorBar) {
                    colorBar->paintMe(gc);
                }
            }
        }
        if (!legendObjects.empty()) {
            for (auto l : legendObjects) {
                if (l) {
                    l->paintMe(gc);
                }
            }
        }
        if (!annotationObjects.empty()) {
            for (auto annotation : annotationObjects) {
                if (annotation && gc.getRenderName() != L"GL") {
                    annotation->paintMe(gc);
                }
            }
        }
        if (!uiControlObjects.empty()) {
            for (auto uio : uiControlObjects) {
                if (uio) {
                    uio->paintMe(gc);
                }
            }
        }

        _resized = false;
        setRenderingStateInvalid(false);
    }
}
//=============================================================================
void
GOFigure::paintAnnotations(RenderInterface& gc)
{
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handlesChildren(children->data());
    for (int64 childHandle : handlesChildren) {
        GraphicsObject* fp = findGraphicsObject(childHandle, false);
        if (dynamic_cast<GOAnnotation*>(fp)) {
            fp->paintMe(gc);
        }
    }
}
//=============================================================================
// Wayland clients cannot place their own windows nor read back their position:
// Qt reports (0, 0). Keep the position the user asked for.
static void
keepRequestedPosition(GOFigure* fig, std::vector<double>& position)
{
    if (GraphicsEnvironment::platformReportsWindowPosition() || position.size() < 4) {
        return;
    }
    std::vector<double> current(fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    if (current.size() >= 4 && std::isfinite(current[0]) && std::isfinite(current[1])) {
        position[0] = current[0];
        position[1] = current[1];
    }
}
//=============================================================================
static void
updateResizeChildren(GOFigure* fig)
{
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());
    // Every child is refreshed, whatever AutoResizeChildren: a pixel Position is
    // anchored to the bottom-left corner, so the widget must move when the figure
    // height changes even though its Position stays the same.
    for (ompIndexType i = 0; i < (ompIndexType)handles.size(); i++) {
        GraphicsObject* fp = findGraphicsObject(handles[i]);
        if (!fp) {
            continue;
        }
        bool userPositionEdit = false;
        if (fp->haveProperty(GO_POSITION_PROPERTY_NAME_STR)) {
            GOGenericProperty* gop = fp->findProperty(GO_POSITION_PROPERTY_NAME_STR);
            if (gop) {
                userPositionEdit = gop->isModified();
                gop->setModified(true);
            }
        }
        bool restoreAutoPosition = !userPositionEdit
            && fp->haveProperty(GO_POSITION_MODE_PROPERTY_NAME_STR)
            && fp->isAuto(GO_POSITION_MODE_PROPERTY_NAME_STR);
        fp->updateState();
        if (restoreAutoPosition && !fp->isAuto(GO_POSITION_MODE_PROPERTY_NAME_STR)) {
            fp->toAuto(GO_POSITION_MODE_PROPERTY_NAME_STR);
            fp->clearChanged(GO_POSITION_MODE_PROPERTY_NAME_STR);
        }
        // The title margin is a pixel amount inside a normalized Position: a title
        // that fit the old height is clipped by a shorter one. Re-pack it for the new
        // size (the axes gives the room back when the figure grows again).
        if (restoreAutoPosition) {
            if (auto* axis = dynamic_cast<GOAxis*>(fp)) {
                axis->rePackTitleArea();
                axis->markStateDirty();
            }
        }
    }
}
//=============================================================================
void
GOFigure::refreshChildrenLayout()
{
    if (!m_childrenLayoutStale) {
        return;
    }
    m_childrenLayoutStale = false;
    updateResizeChildren(this);
}
//=============================================================================
void
GOFigure::resizeGL(int width, int height)
{
    if ((m_width == width) && (m_height == height)) {
        return;
    }
    _resized = true;
    double geoX = 0;
    double geoY = 0;
    double geoW = 0;
    double geoH = 0;
    double frameTopOffset = 0;
    if (!m_win->hostFrameGeometry(geoX, geoY, geoW, geoH, frameTopOffset)) {
        return; // headless: resize goes through setRenderingSize
    }
    std::wstring units = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    double devicePixelRatio = findScalarDoubleProperty(GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR);
    if (devicePixelRatio <= 0) {
        devicePixelRatio = 1.0;
    }
    setRenderingSize(width, height, devicePixelRatio);
    double logicalWidth = static_cast<double>(width) / devicePixelRatio;
    double logicalHeight = static_cast<double>(height) / devicePixelRatio;
    double transformedY = transformY(
        geoY - frameTopOffset, logicalHeight, GraphicsEnvironment::currentScreenHeight());

    if (!stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        int screenWidth = 0;
        int screenHeight = 0;
        GraphicsEnvironment::currentScreenResolution(screenWidth, screenHeight);
        double xPos = geoX;
        double yPos = transformedY;
        const bool docked = m_win->isDockedHost();
        if (docked) {
            xPos = 1;
            yPos = 1;
        }
        std::vector<double> position = convertFromPixels(
            units, xPos, yPos, logicalWidth, logicalHeight, screenWidth, screenHeight);
        if (!docked) {
            keepRequestedPosition(this, position);
        }
        setFourVectorDefault(
            GO_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
        setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, position[0], position[1],
            position[2], position[3]);
        setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, position[0], position[1],
            position[2], position[3]);
        if (m_hasExposedPositionOverride && isFourVector(m_positionOverride)
            && isFourVector(m_innerPositionOverride) && isFourVector(m_outerPositionOverride)) {
            m_positionOverride[2] = position[2];
            m_positionOverride[3] = position[3];
            m_innerPositionOverride[2] = position[2];
            m_innerPositionOverride[3] = position[3];
            m_outerPositionOverride[2] = position[2];
            m_outerPositionOverride[3] = position[3];
            m_paperPositionOverride.clear();
        }
    }
    // setRenderingSize flagged the children layout stale; updateState re-derives it.
    updateState();
    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)findProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->pushEvent(this, L"SizeChangedData", L"SizeChanged");
    }
}
//=============================================================================
bool
GOFigure::resized()
{
    return _resized;
}
//=============================================================================
double
GOFigure::transformY(double y, double heightFrame, double screenHeight)
{
    double transformedY = screenHeight - heightFrame - y;
    return transformedY;
}
//=============================================================================
void
GOFigure::refreshPositionProperties(bool forceUpdate)
{
    std::vector<std::wstring> positionNames = { GO_POSITION_PROPERTY_NAME_STR,
        GO_INNER_POSITION_PROPERTY_NAME_STR, GO_OUTER_POSITION_PROPERTY_NAME_STR };
    if (m_hasExposedPositionOverride) {
        if (hasChanged(positionNames)) {
            m_hasExposedPositionOverride = false;
        } else if (isFourVector(m_positionOverride) && isFourVector(m_innerPositionOverride)
            && isFourVector(m_outerPositionOverride)) {
            static_cast<GOFourVectorProperty*>(findProperty(GO_POSITION_PROPERTY_NAME_STR))
                ->value(m_positionOverride[0], m_positionOverride[1], m_positionOverride[2],
                    m_positionOverride[3]);
            static_cast<GOFourVectorProperty*>(findProperty(GO_INNER_POSITION_PROPERTY_NAME_STR))
                ->value(m_innerPositionOverride[0], m_innerPositionOverride[1],
                    m_innerPositionOverride[2], m_innerPositionOverride[3]);
            static_cast<GOFourVectorProperty*>(findProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR))
                ->value(m_outerPositionOverride[0], m_outerPositionOverride[1],
                    m_outerPositionOverride[2], m_outerPositionOverride[3]);
            if (isFourVector(m_paperPositionOverride)) {
                static_cast<GOFourVectorProperty*>(
                    findProperty(GO_PAPER_POSITION_PROPERTY_NAME_STR))
                    ->value(m_paperPositionOverride[0], m_paperPositionOverride[1],
                        m_paperPositionOverride[2], m_paperPositionOverride[3]);
            } else {
                refreshPaperPosition();
            }
            return;
        }
    }
    syncFigureUnitsIfChanged(this);
    if (m_win->isHeadlessHost()) {
        syncSimplePositionTriplet(this);
        // The web panel has its own rendering dimensions. Keep Position as the
        // logical coordinate space requested by the script, so pixel uicontrols and
        // plotted content share the same layout.
        syncRenderingSizeFromPosition(this);
        clearChanged(positionNames);
        refreshPaperPosition();
        return;
    }
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        syncSimplePositionTriplet(this);
        syncRenderingSizeFromPosition(this);
        refreshPaperPosition();
        return;
    }
    double geoX = 0;
    double geoY = 0;
    double geoW = 0;
    double geoH = 0;
    double geoFrameTop = 0;
    const bool hasGeometry = m_win->hostFrameGeometry(geoX, geoY, geoW, geoH, geoFrameTop);
    std::wstring units = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    // m_width/m_height are in m_renderScale units (device pixels once the GL canvas
    // resized, logical pixels set from Position before that).
    double width = m_width > 0 ? m_width / m_renderScale : (hasGeometry ? geoW : 0);
    double height = m_height > 0 ? m_height / m_renderScale : (hasGeometry ? geoH : 0);

    int screenWidth = 0;
    int screenHeight = 0;
    GraphicsEnvironment::currentScreenResolution(screenWidth, screenHeight);
    const bool docked = m_win->isDockedHost();
    double xPos = 1;
    double yPos = 1;
    if (!docked && hasGeometry) {
        xPos = geoX;
        yPos = transformY(geoY - geoFrameTop, height, GraphicsEnvironment::currentScreenHeight());
    }
    // A docked figure has no screen position: Position is the drawing area inside
    // its desktop panel, [1 1 width height].
    std::vector<double> position
        = convertFromPixels(units, xPos, yPos, width, height, screenWidth, screenHeight);
    if (!docked) {
        keepRequestedPosition(this, position);
        // A figure explicitly positioned off-screen keeps its requested position even
        // when the window manager clamps the window back on screen, so findfigs can
        // detect it. When the requested position is off-screen and the window reports
        // an on-screen location, keep the request and engage the exposed-position
        // override. An on-screen requested position is never overridden, so interactive
        // window moves stay authoritative.
        std::vector<double> requested = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        bool requestedOffscreen = false;
        if (requested.size() >= 4 && std::isfinite(requested[0]) && std::isfinite(requested[1])
            && screenWidth > 0 && screenHeight > 0
            && stringCheck(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIXELS_STR)) {
            requestedOffscreen = (requested[0] + requested[2] < 1.0)
                || (requested[1] + requested[3] < 1.0) || (requested[0] > screenWidth)
                || (requested[1] > screenHeight);
        }
        if (requestedOffscreen && position.size() >= 4
            && (std::abs(requested[0] - position[0]) > 0.5
                || std::abs(requested[1] - position[1]) > 0.5)) {
            position[0] = requested[0];
            position[1] = requested[1];
            position[2] = requested[2];
            position[3] = requested[3];
            m_positionOverride = position;
            m_innerPositionOverride = position;
            m_outerPositionOverride = position;
            m_paperPositionOverride.clear();
            m_hasExposedPositionOverride = true;
        }
    }
    for (const auto& name : positionNames) {
        auto* positionProperty = static_cast<GOFourVectorProperty*>(findProperty(name, false));
        if (positionProperty) {
            positionProperty->value(position[0], position[1], position[2], position[3]);
        }
    }
    refreshPaperPosition();
};
//=============================================================================
void
GOFigure::setExposedPositionOverride(const std::vector<double>& position,
    const std::vector<double>& innerPosition, const std::vector<double>& outerPosition,
    const std::vector<double>& paperPosition)
{
    m_hasExposedPositionOverride
        = isFourVector(position) && isFourVector(innerPosition) && isFourVector(outerPosition);
    m_positionOverride = position;
    m_innerPositionOverride = innerPosition;
    m_outerPositionOverride = outerPosition;
    m_paperPositionOverride = paperPosition;
    clearChanged({ GO_POSITION_PROPERTY_NAME_STR, GO_INNER_POSITION_PROPERTY_NAME_STR,
        GO_OUTER_POSITION_PROPERTY_NAME_STR });
    refreshPositionProperties(false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
