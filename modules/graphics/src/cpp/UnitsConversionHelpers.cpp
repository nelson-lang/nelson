//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UnitsConversionHelpers.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Constants for unit conversions
const double DEFAULT_DPI = 96.0;
const double POINTS_PER_INCH = 72.0;
const double CM_PER_INCH = 2.54;
const double CHAR_WIDTH_PIXELS = 8.0;
const double CHAR_HEIGHT_PIXELS = 16.0;
const double DATA_UNIT_PIXELS = 1.0;
//=============================================================================
// Helper function to convert between different unit systems
struct UnitConversionFactors
{
    double xFactor;
    double yFactor;
};
//=============================================================================
UnitConversionFactors
getConversionFactors(
    const std::wstring& fromUnit, const std::wstring& toUnit, double width, double height)
{
    // Factors to convert from fromUnit to pixels
    double xFactorToPixels = 1.0;
    double yFactorToPixels = 1.0;

    // Convert source unit to pixels
    if (fromUnit == GO_PROPERTY_VALUE_NORMALIZED_STR) {
        xFactorToPixels = width;
        yFactorToPixels = height;
    } else if (fromUnit == GO_PROPERTY_VALUE_INCHES_STR) {
        xFactorToPixels = DEFAULT_DPI;
        yFactorToPixels = DEFAULT_DPI;
    } else if (fromUnit == GO_PROPERTY_VALUE_CENTIMETERS_STR) {
        xFactorToPixels = DEFAULT_DPI / CM_PER_INCH;
        yFactorToPixels = DEFAULT_DPI / CM_PER_INCH;
    } else if (fromUnit == GO_PROPERTY_VALUE_POINTS_STR) {
        xFactorToPixels = DEFAULT_DPI / POINTS_PER_INCH;
        yFactorToPixels = DEFAULT_DPI / POINTS_PER_INCH;
    } else if (fromUnit == GO_PROPERTY_VALUE_CHARACTERS_STR) {
        xFactorToPixels = CHAR_WIDTH_PIXELS;
        yFactorToPixels = CHAR_HEIGHT_PIXELS;
    } else if (fromUnit == GO_PROPERTY_VALUE_DATA_STR) {
        xFactorToPixels = DATA_UNIT_PIXELS;
        yFactorToPixels = DATA_UNIT_PIXELS;
    }

    // Factors to convert from pixels to target unit
    double xFactorFromPixels = 1.0;
    double yFactorFromPixels = 1.0;

    // Convert pixels to target unit
    if (toUnit == GO_PROPERTY_VALUE_NORMALIZED_STR) {
        xFactorFromPixels = 1.0 / width;
        yFactorFromPixels = 1.0 / height;
    } else if (toUnit == GO_PROPERTY_VALUE_INCHES_STR) {
        xFactorFromPixels = 1.0 / DEFAULT_DPI;
        yFactorFromPixels = 1.0 / DEFAULT_DPI;
    } else if (toUnit == GO_PROPERTY_VALUE_CENTIMETERS_STR) {
        xFactorFromPixels = CM_PER_INCH / DEFAULT_DPI;
        yFactorFromPixels = CM_PER_INCH / DEFAULT_DPI;
    } else if (toUnit == GO_PROPERTY_VALUE_POINTS_STR) {
        xFactorFromPixels = POINTS_PER_INCH / DEFAULT_DPI;
        yFactorFromPixels = POINTS_PER_INCH / DEFAULT_DPI;
    } else if (toUnit == GO_PROPERTY_VALUE_CHARACTERS_STR) {
        xFactorFromPixels = 1.0 / CHAR_WIDTH_PIXELS;
        yFactorFromPixels = 1.0 / CHAR_HEIGHT_PIXELS;
    } else if (toUnit == GO_PROPERTY_VALUE_DATA_STR) {
        xFactorFromPixels = 1.0 / DATA_UNIT_PIXELS;
        yFactorFromPixels = 1.0 / DATA_UNIT_PIXELS;
    }

    // Calculate combined conversion factors
    return { xFactorToPixels * xFactorFromPixels, yFactorToPixels * yFactorFromPixels };
}
//=============================================================================
// Unified conversion function for point values
std::pair<double, double>
convertUnitsPair(const std::wstring& fromUnit, const std::wstring& toUnit, double x, double y,
    double width, double height)
{
    // If units are the same, no conversion needed
    if (fromUnit == toUnit) {
        return { x, y };
    }

    // For other conversions, get conversion factors and apply them
    UnitConversionFactors factors = getConversionFactors(fromUnit, toUnit, width, height);
    return { x * factors.xFactor, y * factors.yFactor };
}
//=============================================================================
// Function for rectangle values
std::vector<double>
convertUnitsVector(const std::wstring& fromUnit, const std::wstring& toUnit, double x, double y,
    double w, double h, double width, double height)
{
    // If units are the same, no conversion needed
    if (fromUnit == toUnit) {
        return { x, y, w, h };
    }

    auto [newX, newY] = convertUnitsPair(fromUnit, toUnit, x, y, width, height);
    auto [newW, newH] = convertUnitsPair(fromUnit, toUnit, w, h, width, height);
    return { newX, newY, newW, newH };
}
//=============================================================================
// Maintain original function signatures
const std::pair<double, double>
convertToPixels(const std::wstring& fromUnit, double x, double y, double width, double height)
{
    return convertUnitsPair(fromUnit, GO_PROPERTY_VALUE_PIXELS_STR, x, y, width, height);
}
//=============================================================================
std::pair<double, double>
convertFromPixels(const std::wstring& toUnit, double x, double y, double width, double height)
{
    return convertUnitsPair(GO_PROPERTY_VALUE_PIXELS_STR, toUnit, x, y, width, height);
}
//=============================================================================
std::vector<double>
convertFromPixels(
    const std::wstring& toUnit, double x, double y, double w, double h, double width, double height)
{
    return convertUnitsVector(GO_PROPERTY_VALUE_PIXELS_STR, toUnit, x, y, w, h, width, height);
}
//=============================================================================
void
convertPosition(const std::wstring& from, const std::wstring& to, std::vector<double>& position,
    double width, double height)
{
    if (from == to) {
        return;
    }

    for (size_t i = 0; i < position.size(); i += 2) {
        auto [newX, newY] = convertUnitsPair(from, to, position[i], position[i + 1], width, height);
        position[i] = newX;
        position[i + 1] = newY;
    }
}
//=============================================================================
}
//=============================================================================
