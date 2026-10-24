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
#include <cmath>
#include <memory>
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORoot.hpp"
#include "GOList.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "GraphicsEnvironment.hpp"
#include "GOColorVectorProperty.hpp"
#include "GraphicCallback.hpp"
#include "GOCommonProperties.hpp"
#include "GraphicsObjectFactory.hpp"
#include "StringHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "Evaluator.hpp"
#include "Interface.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static GORoot* _graphicsRootObject = nullptr;
//=============================================================================
static constexpr double ROOT_POINTS_PER_INCH = 72.0;
static constexpr double ROOT_CM_PER_INCH = 2.54;
static constexpr double ROOT_CHARACTER_WIDTH_PIXELS = 28.0 / 3.0;
static constexpr double ROOT_CHARACTER_HEIGHT_PIXELS = 20.0;
static const std::wstring DEFAULT_FIGURE_POSITION_PROPERTY_NAME = L"DefaultFigurePosition";
//=============================================================================
struct RootDefaultTarget
{
    std::wstring prefix;
    std::wstring type;
};
//=============================================================================
class BoolStateScope
{
public:
    BoolStateScope(bool& value, bool temporaryValue) : m_value(value), m_previousValue(value)
    {
        m_value = temporaryValue;
    }
    ~BoolStateScope() { m_value = m_previousValue; }

private:
    bool& m_value;
    bool m_previousValue;
};
//=============================================================================
static const std::vector<RootDefaultTarget>&
rootDefaultTargets()
{
    static const std::vector<RootDefaultTarget> targets = {
        { L"AnimatedLine", GO_PROPERTY_VALUE_ANIMATED_LINE_STR },
        { L"StackedLineChart", GO_PROPERTY_VALUE_STACKED_LINE_CHART_STR },
        { L"Tiledlayout", GO_PROPERTY_VALUE_TILED_LAYOUT_STR },
        { L"Colorbar", GO_PROPERTY_VALUE_COLORBAR_STR },
        { L"Contour", GO_PROPERTY_VALUE_CONTOUR_STR },
        { L"Figure", GO_PROPERTY_VALUE_FIGURE_STR },
        { L"Axes", GO_PROPERTY_VALUE_AXES_STR },
        { L"Line", GO_PROPERTY_VALUE_LINE_STR },
        { L"Text", GO_PROPERTY_VALUE_TEXT_STR },
        { L"Image", GO_PROPERTY_VALUE_IMAGE_STR },
        { L"Patch", GO_PROPERTY_VALUE_PATCH_STR },
        { L"Scatter", GO_PROPERTY_VALUE_SCATTER_STR },
        { L"Surface", GO_PROPERTY_VALUE_SURFACE_STR },
        { L"Uicontrol", GO_PROPERTY_VALUE_UICONTROL_STR },
        { L"Hggroup", GO_PROPERTY_VALUE_HGGROUP_STR },
        { L"Hgtransform", GO_PROPERTY_VALUE_HGTRANSFORM_STR },
    };
    return targets;
}
//=============================================================================
static bool
isRemoveValue(const ArrayOf& value)
{
    if (!value.isRowVectorCharacterArray() && !value.isScalarStringArray()) {
        return false;
    }
    return StringHelpers::iequals(value.asWideString(), L"remove");
}
//=============================================================================
static ArrayOf
currentDefaultFigurePosition()
{
    return ArrayOf::doubleRowVector(defaultFigurePosition());
}
//=============================================================================
static std::wstring
prefixForType(const std::wstring& type)
{
    for (const auto& target : rootDefaultTargets()) {
        if (target.type == type) {
            return target.prefix;
        }
    }
    return {};
}
//=============================================================================
static bool
splitDefaultPropertyName(
    const std::wstring& name, std::wstring& objectType, std::wstring& propertyName)
{
    constexpr size_t defaultPrefixLength = 7;
    if (name.size() <= defaultPrefixLength || name.rfind(L"Default", 0) != 0) {
        return false;
    }
    std::wstring suffix = name.substr(defaultPrefixLength);
    for (const auto& target : rootDefaultTargets()) {
        if (suffix.rfind(target.prefix, 0) == 0 && suffix.size() > target.prefix.size()) {
            objectType = target.type;
            propertyName = suffix.substr(target.prefix.size());
            return true;
        }
    }
    return false;
}
//=============================================================================
static std::wstring
defaultNameFor(const std::wstring& objectType, const std::wstring& propertyName)
{
    std::wstring prefix = prefixForType(objectType);
    if (prefix.empty()) {
        return {};
    }
    return L"Default" + prefix + propertyName;
}
//=============================================================================
static double
screenDpi()
{
    return GraphicsEnvironment::primaryScreenDpi();
}
//=============================================================================
static double
rootLengthFromPixels(const std::wstring& unit, double value, double extent)
{
    double dpi = screenDpi();
    if (unit == GO_PROPERTY_VALUE_INCHES_STR) {
        return value / dpi;
    }
    if (unit == GO_PROPERTY_VALUE_CENTIMETERS_STR) {
        return value * ROOT_CM_PER_INCH / dpi;
    }
    if (unit == GO_PROPERTY_VALUE_POINTS_STR) {
        return value * ROOT_POINTS_PER_INCH / dpi;
    }
    if (unit == GO_PROPERTY_VALUE_CHARACTERS_STR) {
        return value / ROOT_CHARACTER_WIDTH_PIXELS;
    }
    if (unit == GO_PROPERTY_VALUE_NORMALIZED_STR) {
        return extent == 0.0 ? 0.0 : value / extent;
    }
    return value;
}
//=============================================================================
static double
rootHeightFromPixels(const std::wstring& unit, double value, double extent)
{
    if (unit == GO_PROPERTY_VALUE_CHARACTERS_STR) {
        return value / ROOT_CHARACTER_HEIGHT_PIXELS;
    }
    return rootLengthFromPixels(unit, value, extent);
}
//=============================================================================
static double
rootLengthToPixels(const std::wstring& unit, double value, double extent)
{
    double dpi = screenDpi();
    if (unit == GO_PROPERTY_VALUE_INCHES_STR) {
        return value * dpi;
    }
    if (unit == GO_PROPERTY_VALUE_CENTIMETERS_STR) {
        return value * dpi / ROOT_CM_PER_INCH;
    }
    if (unit == GO_PROPERTY_VALUE_POINTS_STR) {
        return value * dpi / ROOT_POINTS_PER_INCH;
    }
    if (unit == GO_PROPERTY_VALUE_CHARACTERS_STR) {
        return value * ROOT_CHARACTER_WIDTH_PIXELS;
    }
    if (unit == GO_PROPERTY_VALUE_NORMALIZED_STR) {
        return value * extent;
    }
    return value;
}
//=============================================================================
static double
rootHeightToPixels(const std::wstring& unit, double value, double extent)
{
    if (unit == GO_PROPERTY_VALUE_CHARACTERS_STR) {
        return value * ROOT_CHARACTER_HEIGHT_PIXELS;
    }
    return rootLengthToPixels(unit, value, extent);
}
//=============================================================================
static std::vector<double>
screenRectangleInUnits(const std::wstring& units)
{
    double width = GraphicsEnvironment::currentScreenWidth();
    double height = GraphicsEnvironment::currentScreenHeight();
    if (units == GO_PROPERTY_VALUE_PIXELS_STR) {
        return { 1.0, 1.0, width, height };
    }
    return { 0.0, 0.0, rootLengthFromPixels(units, width, width),
        rootHeightFromPixels(units, height, height) };
}
//=============================================================================
static std::vector<double>
pointerFromScreenPosition(int pointX, int pointY, const std::wstring& units)
{
    double screenWidth = GraphicsEnvironment::currentScreenWidth();
    double screenHeight = GraphicsEnvironment::currentScreenHeight();
    double xPixels = (double)pointX + 1.0;
    double yPixels = screenHeight - (double)pointY;
    if (units == GO_PROPERTY_VALUE_PIXELS_STR) {
        return { xPixels, yPixels };
    }
    return { rootLengthFromPixels(units, xPixels - 1.0, screenWidth),
        rootHeightFromPixels(units, yPixels - 1.0, screenHeight) };
}
//=============================================================================
static std::pair<int, int>
screenPositionFromPointer(const std::vector<double>& pointer, const std::wstring& units)
{
    double screenWidth = GraphicsEnvironment::currentScreenWidth();
    double screenHeight = GraphicsEnvironment::currentScreenHeight();
    double xPixels = 1.0;
    double yPixels = 1.0;
    if (pointer.size() >= 2) {
        if (units == GO_PROPERTY_VALUE_PIXELS_STR) {
            xPixels = pointer[0];
            yPixels = pointer[1];
        } else {
            xPixels = rootLengthToPixels(units, pointer[0], screenWidth) + 1.0;
            yPixels = rootHeightToPixels(units, pointer[1], screenHeight) + 1.0;
        }
    }
    return { (int)std::round(xPixels - 1.0), (int)std::round(screenHeight - yPixels) };
}
//=============================================================================
static bool
handleIsVisibleForRoot(int64 handle)
{
    GraphicsObject* object = graphicsObjectFromHandle(handle, false);
    if (!object) {
        return false;
    }
    GOGenericProperty* property
        = object->findProperty(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, false);
    if (!property) {
        return true;
    }
    return StringHelpers::iequals(property->get().asWideString(), GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
static std::wstring
currentNumericFormatName()
{
    switch (NelsonConfiguration::getInstance()->getNumericFormatDisplay()) {
    case NLS_NUMERIC_FORMAT_LONG:
        return L"long";
    case NLS_NUMERIC_FORMAT_SHORTE:
        return L"shortE";
    case NLS_NUMERIC_FORMAT_LONGE:
        return L"longE";
    case NLS_NUMERIC_FORMAT_SHORTG:
        return L"shortG";
    case NLS_NUMERIC_FORMAT_LONGG:
        return L"longG";
    case NLS_NUMERIC_FORMAT_SHORTENG:
        return L"shortEng";
    case NLS_NUMERIC_FORMAT_LONGENG:
        return L"longEng";
    case NLS_NUMERIC_FORMAT_PLUS:
        return L"+";
    case NLS_NUMERIC_FORMAT_BANK:
        return L"bank";
    case NLS_NUMERIC_FORMAT_HEX:
        return L"hex";
    case NLS_NUMERIC_FORMAT_RATIONAL:
        return L"rational";
    case NLS_NUMERIC_FORMAT_SHORT:
    default:
        return L"short";
    }
}
//=============================================================================
static std::wstring
currentLineSpacingName()
{
    return NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_COMPACT
        ? L"compact"
        : L"loose";
}
//=============================================================================
static std::pair<double, double>
currentCommandWindowSize()
{
    double columns = (double)DEFAULT_CONSOLE_WIDTH;
    double rows = (double)DEFAULT_CONSOLE_HEIGHT;
    Evaluator* eval
        = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (eval) {
        Interface* io = eval->getInterface();
        if (io) {
            size_t width = io->getTerminalWidth();
            size_t height = io->getTerminalHeight();
            if (width > 0) {
                columns = (double)width;
            }
            if (height > 0) {
                rows = (double)height;
            }
        }
    }
    return { columns, rows };
}
//=============================================================================
GORoot::GORoot()
{
    constructProperties();
    setupDefaults();
    loadDefaultColorMap();
    loadDefaultAlphaMap();
    m_factoryDefaultProperties[GO_DEFAULT_FIGURE_COLORMAP_PROPERTY_NAME_STR]
        = findProperty(GO_DEFAULT_FIGURE_COLORMAP_PROPERTY_NAME_STR)->get();
    m_factoryDefaultProperties[GO_DEFAULT_FIGURE_ALPHAMAP_PROPERTY_NAME_STR]
        = findProperty(GO_DEFAULT_FIGURE_ALPHAMAP_PROPERTY_NAME_STR)->get();
    m_factoryDefaultProperties[L"DefaultAxesFontSize"] = ArrayOf::doubleScalar(10);
    m_factoryDefaultProperties[L"DefaultLineLineWidth"] = ArrayOf::doubleScalar(0.5);
    m_factoryDefaultProperties[L"DefaultTextInterpreter"]
        = ArrayOf::characterArray(GO_PROPERTY_VALUE_TEX_STR);
};
//=============================================================================
std::wstring
GORoot::getType()
{
    return GO_PROPERTY_VALUE_ROOT_STR;
}
//=============================================================================
void
GORoot::constructProperties()
{
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_POINTER_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CURRENT_FIGURE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CALLBACK_OBJECT_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SCREEN_DEPTH_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SCREEN_PIXELS_PER_INCH_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_SCREEN_SIZE_PROPERTY_NAME_STR, false);
    registerProperty(
        new GOTwoVectorProperty, GO_COMMAND_WINDOW_SIZE_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOStringProperty, GO_FORMAT_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOStringProperty, GO_FORMAT_SPACING_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOArrayOfProperty, GO_MONITOR_POSITIONS_PROPERTY_NAME_STR, false);
    registerProperty(
        new GOColorVectorProperty, GO_DEFAULT_FIGURE_COLORMAP_PROPERTY_NAME_STR, true, false);
    registerProperty(
        new GOVectorProperty, GO_DEFAULT_FIGURE_ALPHAMAP_PROPERTY_NAME_STR, true, false);
    registerCommonProperties(this);
    sortProperties();
};
//=============================================================================
void
GORoot::updateState()
{
    int pointerX = 0;
    int pointerY = 0;
    GraphicsEnvironment::pointerScreenPosition(pointerX, pointerY);
    std::wstring units = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    if (hasChanged(GO_POINTER_LOCATION_PROPERTY_NAME_STR)) {
        GOTwoVectorProperty* pos
            = (GOTwoVectorProperty*)findProperty(GO_POINTER_LOCATION_PROPERTY_NAME_STR, false);
        std::pair<int, int> screenPos = screenPositionFromPointer(pos->data(), units);
        GraphicsEnvironment::setPointerScreenPosition(screenPos.first, screenPos.second);
        clearChanged(GO_POINTER_LOCATION_PROPERTY_NAME_STR);
    } else {
        std::vector<double> pointer = pointerFromScreenPosition(pointerX, pointerY, units);
        setTwoVectorDefault(GO_POINTER_LOCATION_PROPERTY_NAME_STR, pointer[0], pointer[1]);
    }

    refreshScreenSizeProperty();
    refreshMonitorPositionsProperty();
    if (!hasChanged(GO_SCREEN_DEPTH_PROPERTY_NAME_STR)) {
        refreshScreenDepthProperty();
    }
    refreshCurrentFigureProperty();
    refreshCallbackObjectProperty();
    refreshChildrenProperty();
    refreshScreenPixelsPerInchProperty();
    refreshCommandWindowSizeProperty();
    refreshFormatProperties();
};
//=============================================================================
void
GORoot::setupDefaults()
{
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setTwoVectorDefault(GO_POINTER_LOCATION_PROPERTY_NAME_STR, 0, 0);
    setStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIXELS_STR);
    refreshCurrentFigureProperty();
    refreshScreenSizeProperty();
    refreshMonitorPositionsProperty();
    refreshScreenDepthProperty();
    refreshScreenPixelsPerInchProperty();
    refreshCommandWindowSizeProperty();
    refreshFormatProperties();
    initializeCommonProperties(this);
};
//=============================================================================
void
GORoot::loadDefaultColorMap()
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
        = (GOColorVectorProperty*)findProperty(GO_DEFAULT_FIGURE_COLORMAP_PROPERTY_NAME_STR);
    hcv->data(cmap);
}
//=============================================================================
void
GORoot::loadDefaultAlphaMap()
{
    std::vector<double> alphaMap;
    const int numElements = 64;
    for (int i = 0; i < numElements; ++i) {
        double value = static_cast<double>(i) / (numElements - 1);
        alphaMap.push_back(std::round(value * 10000.0) / 10000.0);
    }
    GOVectorProperty* hv
        = (GOVectorProperty*)findProperty(GO_DEFAULT_FIGURE_ALPHAMAP_PROPERTY_NAME_STR);
    hv->data(alphaMap);
}
//=============================================================================
void
GORoot::paintMe(RenderInterface& gc) { };
//=============================================================================
void
GORoot::refreshScreenPixelsPerInchProperty()
{
    setScalarDoubleDefault(GO_SCREEN_PIXELS_PER_INCH_PROPERTY_NAME_STR, screenDpi());
    clearChanged(GO_SCREEN_PIXELS_PER_INCH_PROPERTY_NAME_STR);
}
//=============================================================================
void
GORoot::refreshCommandWindowSizeProperty()
{
    std::pair<double, double> size = currentCommandWindowSize();
    setTwoVectorDefault(GO_COMMAND_WINDOW_SIZE_PROPERTY_NAME_STR, size.first, size.second);
    clearChanged(GO_COMMAND_WINDOW_SIZE_PROPERTY_NAME_STR);
}
//=============================================================================
void
GORoot::refreshFormatProperties()
{
    setStringDefault(GO_FORMAT_PROPERTY_NAME_STR, currentNumericFormatName());
    setStringDefault(GO_FORMAT_SPACING_PROPERTY_NAME_STR, currentLineSpacingName());
    clearChanged(GO_FORMAT_PROPERTY_NAME_STR);
    clearChanged(GO_FORMAT_SPACING_PROPERTY_NAME_STR);
}
//=============================================================================
void
GORoot::refreshScreenSizeProperty()
{
    std::vector<double> screen
        = screenRectangleInUnits(findStringProperty(GO_UNITS_PROPERTY_NAME_STR));
    setFourVectorDefault(
        GO_SCREEN_SIZE_PROPERTY_NAME_STR, screen[0], screen[1], screen[2], screen[3]);
}
//=============================================================================
void
GORoot::refreshMonitorPositionsProperty()
{
    std::vector<double> screen
        = screenRectangleInUnits(findStringProperty(GO_UNITS_PROPERTY_NAME_STR));
    GOArrayOfProperty* property = static_cast<GOArrayOfProperty*>(
        findProperty(GO_MONITOR_POSITIONS_PROPERTY_NAME_STR, false));
    if (property) {
        property->data(ArrayOf::doubleRowVector(screen));
    }
}
//=============================================================================
void
GORoot::refreshScreenDepthProperty()
{
    setScalarDoubleDefault(
        GO_SCREEN_DEPTH_PROPERTY_NAME_STR, GraphicsEnvironment::primaryScreenDepth());
}
//=============================================================================
void
GORoot::refreshCurrentFigureProperty()
{
    int64 currentFigure = getCurrentFigure();
    if (currentFigure != NO_FIGURE
        && (findStringProperty(GO_SHOW_HIDDEN_HANDLES_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_ON_STR
            || handleIsVisibleForRoot(currentFigure))) {
        setGoProperty(GO_CURRENT_FIGURE_PROPERTY_NAME_STR, getCurrentFigure());
    } else {
        GOGObjectsProperty* hp
            = (GOGObjectsProperty*)findProperty(GO_CURRENT_FIGURE_PROPERTY_NAME_STR);
        if (hp) {
            std::vector<int64> newval;
            hp->data(newval);
        }
    }
}
//=============================================================================
void
GORoot::refreshCallbackObjectProperty()
{
    GOGObjectsProperty* hp = static_cast<GOGObjectsProperty*>(
        findProperty(GO_CALLBACK_OBJECT_PROPERTY_NAME_STR, false));
    if (!hp) {
        return;
    }
    int64 callbackObject = getCurrentCallbackObject();
    if (callbackObject == HANDLE_UNASSIGNED_OBJECT || isDeletedGraphicsObject(callbackObject)) {
        hp->data({});
        return;
    }
    hp->data({ callbackObject });
}
//=============================================================================
void
GORoot::refreshChildrenProperty()
{
    std::vector<int64> children = getFigureGraphicsObjects();
    std::reverse(children.begin(), children.end());
    if (findStringProperty(GO_SHOW_HIDDEN_HANDLES_PROPERTY_NAME_STR) != GO_PROPERTY_VALUE_ON_STR) {
        std::vector<int64> visibleChildren;
        visibleChildren.reserve(children.size());
        for (int64 child : children) {
            if (handleIsVisibleForRoot(child)) {
                visibleChildren.push_back(child);
            }
        }
        children = visibleChildren;
    }
    GOGObjectsProperty* cp = (GOGObjectsProperty*)findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    cp->data(children);
}
//=============================================================================
bool
GORoot::isDefaultPropertyName(const std::wstring& name) const
{
    if (name == GO_DEFAULT_FIGURE_COLORMAP_PROPERTY_NAME_STR
        || name == GO_DEFAULT_FIGURE_ALPHAMAP_PROPERTY_NAME_STR) {
        return true;
    }
    std::wstring objectType;
    std::wstring propertyName;
    return splitDefaultPropertyName(name, objectType, propertyName);
}
//=============================================================================
bool
GORoot::getDefaultProperty(const std::wstring& name, ArrayOf& value) const
{
    auto overrideValue = m_defaultPropertyOverrides.find(name);
    if (overrideValue != m_defaultPropertyOverrides.end()) {
        value = overrideValue->second;
        return true;
    }
    if (name == DEFAULT_FIGURE_POSITION_PROPERTY_NAME) {
        value = currentDefaultFigurePosition();
        return true;
    }
    GOGenericProperty* registeredProperty = const_cast<GORoot*>(this)->findProperty(name, false);
    if (registeredProperty) {
        value = registeredProperty->get();
        return true;
    }
    auto factoryValue = m_factoryDefaultProperties.find(name);
    if (factoryValue != m_factoryDefaultProperties.end()) {
        value = factoryValue->second;
        return true;
    }
    std::wstring objectType;
    std::wstring propertyName;
    if (splitDefaultPropertyName(name, objectType, propertyName)) {
        const_cast<GORoot*>(this)->ensureFactoryDefaultsForType(objectType);
        factoryValue = m_factoryDefaultProperties.find(name);
        if (factoryValue != m_factoryDefaultProperties.end()) {
            value = factoryValue->second;
            return true;
        }
    }
    return false;
}
//=============================================================================
void
GORoot::ensureFactoryDefaultsForType(const std::wstring& objectType)
{
    if (objectType == GO_PROPERTY_VALUE_ROOT_STR || prefixForType(objectType).empty()
        || m_factoryDefaultsRememberedTypes.find(objectType)
            != m_factoryDefaultsRememberedTypes.end()) {
        return;
    }
    BoolStateScope collectingScope(m_collectingFactoryDefaults, true);
    std::unique_ptr<GraphicsObject> object(newGraphicsObjectForType(objectType, nullptr, true));
    if (!object) {
        m_factoryDefaultsRememberedTypes.insert(objectType);
        return;
    }
    BoolStateScope recordingScope(m_recordFactoryDefaultsNow, true);
    rememberFactoryDefaults(object.get());
}
//=============================================================================
void
GORoot::setDefaultProperty(const std::wstring& name, const ArrayOf& value)
{
    if (!isDefaultPropertyName(name)) {
        Error(L"Nelson:graphics:unrecognizedDefaultProperty", L"Unrecognized default property: %s",
            name);
    }
    if (isRemoveValue(value)) {
        removeDefaultProperty(name);
        return;
    }
    GOGenericProperty* registeredProperty = findProperty(name, false);
    if (registeredProperty) {
        registeredProperty->set(value);
    }
    m_defaultPropertyOverrides[name] = value;
}
//=============================================================================
void
GORoot::removeDefaultProperty(const std::wstring& name)
{
    if (!isDefaultPropertyName(name)) {
        Error(L"Nelson:graphics:unrecognizedDefaultProperty", L"Unrecognized default property: %s",
            name);
    }
    m_defaultPropertyOverrides.erase(name);
    if (name == GO_DEFAULT_FIGURE_COLORMAP_PROPERTY_NAME_STR) {
        loadDefaultColorMap();
    } else if (name == GO_DEFAULT_FIGURE_ALPHAMAP_PROPERTY_NAME_STR) {
        loadDefaultAlphaMap();
    }
}
//=============================================================================
void
GORoot::rememberFactoryDefaults(GraphicsObject* go)
{
    if (!go) {
        return;
    }
    std::wstring objectType = go->getType();
    if (objectType == GO_PROPERTY_VALUE_ROOT_STR || prefixForType(objectType).empty()) {
        return;
    }
    if (!m_recordFactoryDefaultsNow) {
        return;
    }
    if (m_factoryDefaultsRememberedTypes.find(objectType)
        != m_factoryDefaultsRememberedTypes.end()) {
        return;
    }
    for (const auto& propertyName : go->fieldnames()) {
        if (!go->isWritable(propertyName)) {
            continue;
        }
        if (dynamic_cast<GOGObjectsProperty*>(go->findProperty(propertyName, false))) {
            continue;
        }
        std::wstring defaultName = defaultNameFor(objectType, propertyName);
        if (defaultName == DEFAULT_FIGURE_POSITION_PROPERTY_NAME) {
            continue;
        }
        if (!defaultName.empty() && m_factoryDefaultProperties.count(defaultName) == 0) {
            m_factoryDefaultProperties[defaultName] = go->findProperty(propertyName)->get();
        }
    }
    m_factoryDefaultsRememberedTypes.insert(objectType);
}
//=============================================================================
void
GORoot::applyDefaultProperties(GraphicsObject* go)
{
    if (!go) {
        return;
    }
    if (m_collectingFactoryDefaults) {
        return;
    }
    std::wstring objectType = go->getType();
    for (const auto& entry : m_defaultPropertyOverrides) {
        std::wstring targetType;
        std::wstring propertyName;
        if (!splitDefaultPropertyName(entry.first, targetType, propertyName)
            || targetType != objectType || !go->haveProperty(propertyName)
            || !go->isWritable(propertyName)) {
            continue;
        }
        GOGenericProperty* property = go->findProperty(propertyName, false);
        if (!property) {
            continue;
        }
        property->set(entry.second);
        if (propertyName != GO_POSITION_PROPERTY_NAME_STR
            && propertyName != GO_INNER_POSITION_PROPERTY_NAME_STR
            && propertyName != GO_OUTER_POSITION_PROPERTY_NAME_STR) {
            property->clearModified();
        }
    }
}
//=============================================================================
void
GORoot::setCurrentFigureProperty(const ArrayOf& value)
{
    if (value.isEmpty()) {
        GOGObjectsProperty* property
            = static_cast<GOGObjectsProperty*>(findProperty(GO_CURRENT_FIGURE_PROPERTY_NAME_STR));
        property->data({});
        return;
    }
    int64 handle = value.asGraphicsObjectScalar();
    GOFigure* figure = findGOFigure(handle);
    if (!figure) {
        Error(L"Nelson:graphics:expectedFigure:figureExpected", L"Figure object expected.");
    }
    if (findStringProperty(GO_SHOW_HIDDEN_HANDLES_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_ON_STR
        || handleIsVisibleForRoot(handle)) {
        notifyCurrentFigureChanged(handle);
        setGoProperty(GO_CURRENT_FIGURE_PROPERTY_NAME_STR, handle);
    }
}
//=============================================================================
void
GORoot::resetProperties()
{
    m_defaultPropertyOverrides.clear();
    setupDefaults();
    loadDefaultColorMap();
    loadDefaultAlphaMap();
    clearAllChanged();
}
//=============================================================================
int64
graphicsRootObject()
{
    if (_graphicsRootObject == nullptr) {
        _graphicsRootObject = new GORoot();
    }
    _graphicsRootObject->updateState();
    return (int64)HANDLE_ROOT_OBJECT;
}
//=============================================================================
GORoot*
getGraphicsRootObject()
{
    if (_graphicsRootObject == nullptr) {
        _graphicsRootObject = new GORoot();
    }
    _graphicsRootObject->updateState();
    return _graphicsRootObject;
}
//=============================================================================
// Helvetica is rarely installed: on Windows it resolves to Arial, elsewhere the
// system substitution (Nimbus Sans, DejaVu Sans, ...) has taller metrics, which
// shifts every text-driven layout (title area, ticks, legends). Prefer a family
// metrically compatible with Arial so that figures match across platforms.
static std::wstring
helveticaSubstituteFontName()
{
#if defined(_WIN32)
    return L"Arial";
#else
    return GraphicsEnvironment::defaultSansSubstituteFontFamily();
#endif
}
//=============================================================================
std::wstring
resolveRootFontName(const std::wstring& fontName)
{
    if (StringHelpers::iequals(fontName, L"Helvetica")) {
        return helveticaSubstituteFontName();
    }
    if (!StringHelpers::iequals(fontName, L"FixedWidth")) {
        return fontName;
    }
    GORoot* root = getGraphicsRootObject();
    return root->findStringProperty(GO_FIXED_WIDTH_FONT_NAME_PROPERTY_NAME_STR);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
