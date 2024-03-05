//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOContour.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOColorInterpProperty.hpp"
#include "GOEgdeAlphaProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAxisHelpers.hpp"
#include "GOList.hpp"
#include "GOAxis.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GOContour::getType()
{
    return GO_PROPERTY_VALUE_CONTOUR_STR;
}
//=============================================================================
GOContour::GOContour()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOContour::~GOContour() { }
//=============================================================================
void
GOContour::constructProperties()
{
    registerProperty(new GOArrayOfProperty, GO_CONTOUR_MATRIX_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_FLOATING_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_LEVEL_LIST_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LEVEL_STEP_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LEVEL_STEP_PROPERTY_NAME_STR);
    registerProperty(new GOColorInterpProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_EDGE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Z_DATA_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOContour::setupDefaults()
{
    setRestrictedStringColorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, 0, 0, 0);
    setScalarDoubleDefault(GO_EDGE_ALPHA_PROPERTY_NAME_STR, 1);
    setRestrictedStringDefault(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LEVEL_STEP_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LEVEL_STEP_PROPERTY_NAME_STR, 0.);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_FLOATING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
}
//=============================================================================
/**
 * @brief Update the state of the GOContour object based on property changes.
 * If certain properties have changed, this function updates the state accordingly.
 */
void
GOContour::updateState()
{
    if (hasChanged(GO_LEVEL_LIST_PROPERTY_NAME_STR)) {
        toManual(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_X_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_LEVEL_STEP_PROPERTY_NAME_STR)) {
        toManual(GO_LEVEL_STEP_MODE_PROPERTY_NAME_STR);
    }
    // Calculate min and max values for Z data
    ArrayOf zData(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    double zMin = 0.;
    double zMax = 0.;
    minMaxVector((double*)zData.getDataPointer(), (int)zData.getElementCount(), zMin, zMax);

    // Generate X and Y data arrays
    ArrayOf xData(generateDataArray(GO_X_DATA_PROPERTY_NAME_STR, true));
    ArrayOf yData(generateDataArray(GO_Y_DATA_PROPERTY_NAME_STR, false));

    // Initialize levels vector
    std::vector<double> levels;

    if (stringCheck(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        // Use the provided levels
        levels = findVectorDoubleProperty(GO_LEVEL_LIST_PROPERTY_NAME_STR);
    } else if (stringCheck(GO_LEVEL_STEP_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        zData.promoteType(NLS_DOUBLE);
        double* pzData = (double*)zData.getDataPointer();
        double levelStep = findScalarDoubleProperty(GO_LEVEL_STEP_PROPERTY_NAME_STR);
        double min_val = pzData[0];
        double max_val = pzData[0];
        for (indexType k = 0; k < zData.getElementCount(); k++) {
            min_val = std::min(min_val, pzData[k]);
            max_val = std::max(max_val, pzData[k]);
        }

        // Calculating the level
        double lvl1 = std::ceil(min_val / levelStep) * levelStep;
        double lvl2 = std::floor(max_val / levelStep) * levelStep;
        if (lvl1 >= lvl2) {
            indexType rows = zData.getRows();
            indexType columns = zData.getColumns();
            for (int i = 0; i < rows; ++i) {
                std::vector<double> flattenedZ;

                for (int j = 0; j < columns; ++j) {
                    flattenedZ.push_back(pzData[j * rows + i]);
                }
                std::sort(flattenedZ.begin(), flattenedZ.end());
                levels.push_back(flattenedZ[flattenedZ.size() / 2]);
            }
        } else {
            levels.clear();
            for (double level = lvl1; level <= lvl2; level += levelStep) {
                levels.push_back(level);
            }
        }
        GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_LEVEL_LIST_PROPERTY_NAME_STR);
        hp->data(levels);
        hp->clearModified();
        toAuto(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR);
    } else {
        // Generate levels automatically
        std::list<double> llevels = getTicksInner(zMin, zMax, false, 10);
        levels.assign(llevels.begin(), llevels.end());

        // Remove extremes if they exist in levels
        if (!levels.empty()) {
            if (levels.front() == zMin) {
                levels.erase(levels.begin());
            }
            if (levels.back() == zMax) {
                levels.pop_back();
            }
        }

        // Update the property with the new levels
        std::vector<double> ulevels;
        ulevels.reserve(levels.size()); // Reserving space to prevent reallocations
        ulevels.insert(ulevels.end(), levels.begin(), levels.end());
        GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_LEVEL_LIST_PROPERTY_NAME_STR);
        hp->data(ulevels);
        hp->clearModified();
    }

    // Clear existing contour lines and Z values
    contourLines.clear();
    zValues.clear();

    // Generate contour lines for each level
    for (int i = 0; i < levels.size(); i++) {
        contourLines.push_back(contourGeneratorDriver(zData, levels[i], xData, yData));
        zValues.push_back(levels[i]);
    }

    // Rebuild contour matrix
    rebuildContourMatrix();
}
//=============================================================================
void
GOContour::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    double width(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    GORestrictedStringColorProperty* lc
        = (GORestrictedStringColorProperty*)findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR);
    bool floatflag = stringCheck(GO_FLOATING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    if (!stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
        gc.lineWidth(width);
        GOAxis* parent = (GOAxis*)getParentAxis();

        std::vector<double> xLim = parent->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
        std::vector<double> yLim = parent->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);

        double edgeAlpha = findScalarDoubleProperty(GO_EDGE_ALPHA_PROPERTY_NAME_STR);
        for (int i = (int)contourLines.size() - 1; i >= 0; i--) {
            contourLineSet cset(contourLines[i]);
            if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR)) {
                selectColorForRendering(gc, zValues[i], edgeAlpha);
            } else {
                std::vector<double> color = lc->colorSpec();
                color.push_back(edgeAlpha);
                gc.color(color);
            }
            for (int j = 0; j < cset.size(); j++) {
                contourLine aline(cset[j]);
                std::vector<double> xs, ys, zs;
                for (int k = 0; k < aline.size(); k++) {
                    xs.push_back(aline[k].x);
                    ys.push_back(aline[k].y);
                    if (floatflag) {
                        zs.push_back(zValues[i]);
                    } else {
                        zs.push_back(0);
                    }
                }
                std::vector<double> mxs, mys, mzs;
                parent->reMap(xs, ys, zs, mxs, mys, mzs);

                double axisMinX = xLim[0];
                double axisMaxX = xLim[1];
                double axisMinY = yLim[0];
                double axisMaxY = yLim[1];

                for (size_t i = 0; i < mxs.size(); ++i) {
                    mxs[i] = std::min(std::max(mxs[i], axisMinX), axisMaxX);
                    mys[i] = std::min(std::max(mys[i], axisMinY), axisMaxY);
                }

                gc.lineSeries(mxs, mys, mzs);
            }
        }
    }
}
//=============================================================================
std::vector<double>
GOContour::getLimits()
{
    updateState(); // Ensure the state is up-to-date

    // Initialize variables for storing minimum and maximum values
    double xMin = 0.;
    double xMax = 0.;
    double yMin = 0.;
    double yMax = 0.;
    double zMin = 0.;
    double zMax = 0.;
    double cMin = 0.;
    double cMax = 0.;

    // Flags to track initialization of each dimension
    bool xInitialized = false;
    bool yInitialized = false;
    bool zInitialized = false;

    // Check if zValues vector is not empty
    if (!zValues.empty()) {
        int i = 0;
        double firstZValue = zValues[0];
        // Iterate over each contour line set
        for (const auto& contourLineSet : contourLines) {
            double zValue = zValues[i++];
            // Iterate over each contour line
            for (const auto& aline : contourLineSet) {
                // Iterate over each point in the contour line
                for (const auto& point : aline) {
                    // Update x dimension limits if not initialized
                    if (!xInitialized && std::isfinite(point.x)) {
                        xMin = xMax = point.x;
                        xInitialized = true;
                    }
                    // Update y dimension limits if not initialized
                    if (!yInitialized && std::isfinite(point.y)) {
                        yMin = yMax = point.y;
                        yInitialized = true;
                    }
                    // Update z dimension limits if not initialized
                    if (!zInitialized) {
                        zMin = zMax = firstZValue;
                        zInitialized = true;
                    }
                    // Update x dimension limits if initialized
                    if (xInitialized && std::isfinite(point.x)) {
                        xMin = std::min(xMin, point.x);
                        xMax = std::max(xMax, point.x);
                    }
                    // Update y dimension limits if initialized
                    if (yInitialized && std::isfinite(point.y)) {
                        yMin = std::min(yMin, point.y);
                        yMax = std::max(yMax, point.y);
                    }
                    // Update z dimension limits
                    if (zInitialized) {
                        zMin = std::min(zMin, zValue);
                        zMax = std::max(zMax, zValue);
                    }
                }
            }
        }
    }

    // Initialize default values if dimensions are not initialized
    if (!xInitialized) {
        xMin = -1;
        xMax = 1;
    }
    if (!yInitialized) {
        yMin = -1;
        yMax = 1;
    }
    if (!zInitialized) {
        zMin = 0;
        zMax = 0;
    }

    // Color limits are set to z limits initially
    cMin = zMin;
    cMax = zMax;

    // Check if string property is off, then reset z limits
    if (stringCheck(GO_FLOATING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        zMin = 0.;
        zMax = 0.;
    }

    // Return the limits as a vector
    return { xMin, xMax, yMin, yMax, zMin, zMax, cMin, cMax, 0, 0 };
}
//=============================================================================
inline bool
operator==(const contourPoint& p1, const contourPoint& p2)
{
    return ((p1.x == p2.x) && (p1.y == p2.y));
}
//=============================================================================
inline contourLine
reverseLine(const contourLine& src)
{
    contourLine ret;
    int start = (int)src.size() - 1;
    if (start >= 0) {
        for (int i = start; i >= 0; i--) {
            if (i >= 0) {
                ret.push_back(src[i]);
            } else {
                return ret;
            }
        }
    }
    return ret;
}
//=============================================================================
inline bool
isConnectedLines(const contourLine& current, const contourLine& test)
{
    return ((current.front() == test.front()) || (current.front() == test.back())
        || (current.back() == test.front()) || (current.back() == test.back()));
}
//=============================================================================
inline void
joinLines(contourLine& current, const contourLine& toadd)
{
    if (current.front() == toadd.front()) {
        contourLine tmp = reverseLine(toadd);
        tmp.insert(tmp.end(), current.begin(), current.end());
        current = tmp;
    } else if (current.front() == toadd.back()) {
        contourLine tmp = toadd;
        tmp.insert(tmp.end(), current.begin(), current.end());
        current = tmp;
    } else if (current.back() == toadd.front()) {
        current.insert(current.end(), toadd.begin(), toadd.end());
    } else if (current.back() == toadd.back()) {
        contourLine tmp = reverseLine(toadd);
        current.insert(current.end(), tmp.begin(), tmp.end());
    }
}
//=============================================================================
inline void
drawContourLine(contourLineSet& lines, contourPoint p1, contourPoint p2)
{
    lines.push_back(contourLine { p1, p2 });
}
//=============================================================================
contourLineSet
GOContour::generateAllLines(ArrayOf m, double val)
{
    contourLineSet allLines; // Initialize the contour line set

    // Convert array elements to double type
    m.promoteType(NLS_DOUBLE);
    const double* func = (const double*)m.getDataPointer(); // Get pointer to array data
    int numy = int(m.getRows()); // Number of rows in the array
    int numx = int(m.getColumns()); // Number of columns in the array

    // Lambda function to retrieve function value at specified coordinates
    auto getMapValue = [&](int x, int y) { return func[(y) + (x)*numy]; };

    // Lambda function for linear interpolation between two values
    auto ainter = [&](double a, double b) { return (val - a) / (b - a); };

    // Iterate over each row of the array
    for (int row = 1; row < numy; row++) {
        // Define lambda functions for convenience
        auto fold = [&](int x) { return getMapValue(x, row - 1); };
        auto fnew = [&](int x) { return getMapValue(x, row); };
        auto aleft = [&](int i, int j) { return ((j)-1) + ainter(fold((i)-1), fnew((i)-1)); };
        auto top = [&](int i) { return ((i)-1) + ainter(fnew(i - 1), fnew(i)); };
        auto bot = [&](int i) { return ((i)-1) + ainter(fold(i - 1), fold(i)); };
        auto right = [&](int i, int j) { return ((j)-1) + ainter(fold(i), fnew(i)); };

        // Iterate over each column of the array
        for (int col = 1; col < numx; col++) {
            int l = 0; // Initialize case index
            // Determine the case based on function values at cell corners
            if (fold(col) >= val) {
                l += 1;
            }
            if (fold(col - 1) >= val) {
                l += 2;
            }
            if (fnew(col) >= val) {
                l += 4;
            }
            if (fnew(col - 1) >= val) {
                l += 8;
            }
            // Switch statement to handle each case
            switch (l) {
            case 1:
            case 14: { // Case 1 and 14: Contour lines from bottom to right
                drawContourLine(allLines, contourPoint((double)bot(col), (double)(row - 1)),
                    contourPoint((double)col, (double)right(col, row)));
            } break;
            case 2:
            case 13: { // Case 2 and 13: Contour lines from left to bottom
                drawContourLine(allLines, contourPoint((double)(col - 1), (double)aleft(col, row)),
                    contourPoint((double)bot(col), (double)(row - 1)));
            } break;
            case 3:
            case 12: { // Case 3 and 12: Contour lines from left to right
                drawContourLine(allLines, contourPoint((double)(col - 1), (double)aleft(col, row)),
                    contourPoint((double)col, (double)right(col, row)));
            } break;
            case 4:
            case 11: { // Case 4 and 11: Contour lines from top to right
                drawContourLine(allLines, contourPoint((double)top(col), (double)row),
                    contourPoint((double)col, (double)right(col, row)));
            } break;
            case 5:
            case 10: { // Case 5 and 10: Contour lines from bottom to top
                drawContourLine(allLines, contourPoint((double)bot(col), (double)(row - 1)),
                    contourPoint((double)top(col), (double)row));
            } break;
            case 6:
            case 9: { // Case 6 and 9: Contour lines crossing horizontally
                // Interpolate to find the intersection point
                double x0 = ainter(fold(col - 1), fold(col));
                double x1 = ainter(fnew(col - 1), fnew(col));
                double y0 = ainter(fold(col - 1), fnew(col - 1));
                double y1 = ainter(fold(col), fnew(col));
                double y = (x0 * (y1 - y0) + y0) / (1.0 - (x1 - x0) * (y1 - y0));
                double x = y * (x1 - x0) + x0;
                // Interpolate function values at the intersection point
                double fx1 = getMapValue(col - 1, row - 1)
                    + x * (getMapValue(col, row - 1) - getMapValue(col - 1, row - 1));
                double fx2 = getMapValue(col - 1, row)
                    + x * (getMapValue(col, row) - getMapValue(col - 1, row));
                double f = fx1 + y * (fx2 - fx1);
                // Determine which segments to draw based on function values
                if (f == val) { // Contour passes through the intersection point
                    drawContourLine(allLines, contourPoint((double)bot(col), (double)(row - 1)),
                        contourPoint((double)top(col), (double)row));
                    drawContourLine(allLines,
                        contourPoint((double)(col - 1), (double)aleft(col, row)),
                        contourPoint((double)col, (double)right(col, row)));
                } else if (((f > val) && (fnew(col) > val)) || ((f < val) && (fnew(col) < val))) {
                    drawContourLine(allLines,
                        contourPoint((double)(col - 1), (double)aleft(col, row)),
                        contourPoint((double)top(col), (double)row));
                    drawContourLine(allLines, contourPoint((double)bot(col), (double)(row - 1)),
                        contourPoint((double)col, (double)right(col, row)));
                } else {
                    drawContourLine(allLines,
                        contourPoint((double)(col - 1), (double)aleft(col, row)),
                        contourPoint((double)bot(col), (double)(row - 1)));
                    drawContourLine(allLines, contourPoint((double)top(col), (double)row),
                        contourPoint((double)col, (double)right(col, row)));
                }
            } break;
            case 7:
            case 8: { // Case 7 and 8: Contour lines crossing vertically
                drawContourLine(allLines, contourPoint((double)(col - 1), (double)aleft(col, row)),
                    contourPoint((double)top(col), (double)row));
            } break;
            } // End of switch
        } // End of column iteration
    } // End of row iteration

    // Return the set containing all generated contour lines
    return allLines;
}
//=============================================================================
contourLineSet
GOContour::bundleLines(contourLineSet& allLines)
{
    contourLineSet bundledLines;
    bundledLines.reserve(1);
    while (!allLines.empty()) {
        contourLine current = allLines.front();
        allLines.erase(allLines.begin());
        bool lineGrown = true;
        while (lineGrown) {
            lineGrown = false;
            auto it = allLines.begin();
            while (it != allLines.end()) {
                if (isConnectedLines(current, *it)) {
                    joinLines(current, *it);
                    it = allLines.erase(it);
                    lineGrown = true;
                } else {
                    ++it;
                }
            }
        }
        bundledLines.push_back(current);
    }
    return bundledLines;
}
//=============================================================================
contourLineSet
GOContour::adjustLines(
    contourLineSet& bundledLines, const ArrayOf& x, const ArrayOf& y, int numx, int numy)
{
    const double* xp = (const double*)(x.getDataPointer());
    const double* yp = (const double*)(y.getDataPointer());
#define X(a, b) xp[(b) + (a)*numy]
#define Y(a, b) yp[(b) + (a)*numy]

    contourLineSet adjustedLines;
    for (int i = 0; i < bundledLines.size(); i++) {
        for (int j = 0; j < bundledLines[i].size(); j++) {
            double ndx_x = bundledLines[i][j].x;
            double ndx_y = bundledLines[i][j].y;
            int idx_X = std::max(std::min((int)ndx_x, numx - 2), 0);
            int idx_Y = std::max(std::min((int)ndx_y, numy - 2), 0);
            double eps_x = ndx_x - idx_X;
            double eps_y = ndx_y - idx_Y;
            double xp_out_1 = X(idx_X, idx_Y) + eps_x * (X(idx_X + 1, idx_Y) - X(idx_X, idx_Y));
            double xp_out_2
                = X(idx_X, idx_Y + 1) + eps_x * (X(idx_X + 1, idx_Y + 1) - X(idx_X, idx_Y + 1));
            double xp_out = xp_out_1 + eps_y * (xp_out_2 - xp_out_1);
            double yp_out_1 = Y(idx_X, idx_Y) + eps_x * (Y(idx_X + 1, idx_Y) - Y(idx_X, idx_Y));
            double yp_out_2
                = Y(idx_X, idx_Y + 1) + eps_x * (Y(idx_X + 1, idx_Y + 1) - Y(idx_X, idx_Y + 1));
            double yp_out = yp_out_1 + eps_y * (yp_out_2 - yp_out_1);
            bundledLines[i][j].x = xp_out;
            bundledLines[i][j].y = yp_out;
        }
    }
    return bundledLines;
}
//=============================================================================
contourLineSet
GOContour::contourGeneratorDriver(ArrayOf m, double val, const ArrayOf& x, const ArrayOf& y)
{
    contourLineSet allLines = generateAllLines(m, val);
    contourLineSet bundledLines = bundleLines(allLines);
    return adjustLines(bundledLines, x, y, int(m.getColumns()), int(m.getRows()));
}
//=============================================================================
void
GOContour::rebuildContourMatrix()
{
    int pointcount = 0;
    int linecount = 0;
    for (int i = 0; i < contourLines.size(); i++) {
        for (int j = 0; j < contourLines[i].size(); j++) {
            linecount++;
            pointcount += (int)contourLines[i][j].size();
        }
    }
    int outcount = pointcount + linecount;
    double* output = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * outcount);
    ArrayOf out = ArrayOf(NLS_DOUBLE, Dimensions(2, outcount), output);
    for (int i = 0; i < contourLines.size(); i++) {
        for (int j = 0; j < contourLines[i].size(); j++) {
            *output++ = zValues[i];
            *output++ = contourLines[i][j].size();
            contourLine bline(contourLines[i][j]);
            for (int k = 0; k < bline.size(); k++) {
                *output++ = bline[k].x;
                *output++ = bline[k].y;
            }
        }
    }
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(GO_CONTOUR_MATRIX_PROPERTY_NAME_STR);
    hp->data(out);
}
//=============================================================================
/**
 * @brief Generates an array of data based on the given name and mode.
 *
 * This function generates a data array based on the specified name and mode. If the mode is manual,
 * it tries to find the property by name, and if the property is a vector of appropriate size, it
 * populates the array accordingly. Otherwise, it allocates a new array and fills it with either
 * column indices (if isXcoord is true) or row indices (if isXcoord is false).
 *
 * @param name The name of the property.
 * @param isXcoord Boolean flag indicating whether X coordinates are being processed.
 * @return An array containing the generated data.
 */
ArrayOf
GOContour::generateDataArray(const std::wstring& name, bool isXcoord)
{
    ArrayOf zData(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    indexType zRows = zData.getRows();
    indexType zCols = zData.getColumns();

    // Check mode and process accordingly
    if (stringCheck(name + L"Mode", GO_PROPERTY_VALUE_MANUAL_STR)) {
        ArrayOf cData(findArrayOfProperty(name));

        // Process if cData is a vector of appropriate size
        if (cData.isVector()
            && ((isXcoord && (cData.getElementCount() == zCols))
                || (!isXcoord && (cData.getElementCount() == zRows)))) {
            cData.promoteType(NLS_DOUBLE);
            const double* qp = static_cast<const double*>(cData.getDataPointer());
            double* dp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, zRows * zCols));
            ArrayOf mat(NLS_DOUBLE, Dimensions(zRows, zCols), dp);

            // Fill the array based on isXcoord flag
            for (int i = 0; i < zCols; i++) {
                for (int j = 0; j < zRows; j++) {
                    *dp++ = isXcoord ? qp[i] : qp[j];
                }
            }
            return mat;
        } else if (cData.is2D() && (cData.getRows() == zRows) && (cData.getColumns() == zCols)) {
            return cData;
        }
    }

    // If mode is not manual or appropriate cData is not found, generate default data array
    double* dp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, zRows * zCols));
    ArrayOf mat(NLS_DOUBLE, Dimensions(zRows, zCols), dp);
    for (int i = 0; i < zCols; i++) {
        for (int j = 0; j < zRows; j++) {
            *dp++ = isXcoord ? i + 1 : j + 1;
        }
    }
    return mat;
}
//=============================================================================
void
GOContour::selectColorForRendering(RenderInterface& gc, double zValue, double edgeAlpha)
{
    // Retrieve color map and axis information
    std::vector<double> colorMap(
        getParentFigure()->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR));
    GOAxis* axis = getParentAxis();
    std::vector<double> cLim(axis->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR));
    double cLimMin = std::min(cLim[0], cLim[1]);
    double cLimMax = std::max(cLim[0], cLim[1]);

    // Calculate index for color selection based on zValue
    int colorMapLength = static_cast<int>(colorMap.size() / 3);
    int idx = static_cast<int>((zValue - cLimMin) / (cLimMax - cLimMin) * (colorMapLength - 1));
    idx = std::min(colorMapLength - 1, std::max(0, idx));

    // Extract color components from color map
    std::vector<double> color
        = { colorMap[3 * idx], colorMap[3 * idx + 1], colorMap[3 * idx + 2], edgeAlpha };

    // Set the selected color for rendering
    gc.color(color);
}
//=============================================================================
}
//=============================================================================
