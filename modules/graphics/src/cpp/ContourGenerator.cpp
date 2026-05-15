//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ContourGenerator.hpp"
#include "GOAxisHelpers.hpp"
#include "ParallelSort.hpp"
#include <algorithm>
#include <cmath>
#include <list>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    inline bool
    samePoint(const contourPoint& p1, const contourPoint& p2)
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
                ret.push_back(src[i]);
            }
        }
        return ret;
    }
    //=============================================================================
    inline bool
    isConnectedLines(const contourLine& current, const contourLine& test)
    {
        return ((samePoint(current.front(), test.front()))
            || (samePoint(current.front(), test.back()))
            || (samePoint(current.back(), test.front()))
            || (samePoint(current.back(), test.back())));
    }
    //=============================================================================
    inline void
    joinLines(contourLine& current, const contourLine& toadd)
    {
        if (samePoint(current.front(), toadd.front())) {
            contourLine tmp = reverseLine(toadd);
            tmp.insert(tmp.end(), current.begin(), current.end());
            current = tmp;
        } else if (samePoint(current.front(), toadd.back())) {
            contourLine tmp = toadd;
            tmp.insert(tmp.end(), current.begin(), current.end());
            current = tmp;
        } else if (samePoint(current.back(), toadd.front())) {
            current.insert(current.end(), toadd.begin(), toadd.end());
        } else if (samePoint(current.back(), toadd.back())) {
            contourLine tmp = reverseLine(toadd);
            current.insert(current.end(), tmp.begin(), tmp.end());
        }
    }
    //=============================================================================
    inline void
    drawContourLine(contourLineSet& lines, contourPoint p1, contourPoint p2)
    {
        if (std::isfinite(p1.x) && std::isfinite(p1.y) && std::isfinite(p2.x)
            && std::isfinite(p2.y)) {
            lines.push_back(contourLine { p1, p2 });
        }
    }
    //=============================================================================
    contourLineSet
    generateAllLines(ArrayOf m, double val)
    {
        contourLineSet allLines;

        m.promoteType(NLS_DOUBLE);
        const double* func = static_cast<const double*>(m.getDataPointer());
        int numy = int(m.getRows());
        int numx = int(m.getColumns());

        auto getMapValue = [&](int x, int y) { return func[(y) + (x)*numy]; };
        auto ainter = [&](double a, double b) { return (val - a) / (b - a); };

        for (int row = 1; row < numy; row++) {
            auto fold = [&](int x) { return getMapValue(x, row - 1); };
            auto fnew = [&](int x) { return getMapValue(x, row); };
            auto aleft = [&](int i, int j) { return ((j)-1) + ainter(fold((i)-1), fnew((i)-1)); };
            auto top = [&](int i) { return ((i)-1) + ainter(fnew(i - 1), fnew(i)); };
            auto bot = [&](int i) { return ((i)-1) + ainter(fold(i - 1), fold(i)); };
            auto right = [&](int i, int j) { return ((j)-1) + ainter(fold(i), fnew(i)); };

            for (int col = 1; col < numx; col++) {
                if (!std::isfinite(fold(col)) || !std::isfinite(fold(col - 1))
                    || !std::isfinite(fnew(col)) || !std::isfinite(fnew(col - 1))) {
                    continue;
                }
                int l = 0;
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
                switch (l) {
                case 1:
                case 14: {
                    drawContourLine(allLines, contourPoint((double)bot(col), (double)(row - 1)),
                        contourPoint((double)col, (double)right(col, row)));
                } break;
                case 2:
                case 13: {
                    drawContourLine(allLines,
                        contourPoint((double)(col - 1), (double)aleft(col, row)),
                        contourPoint((double)bot(col), (double)(row - 1)));
                } break;
                case 3:
                case 12: {
                    drawContourLine(allLines,
                        contourPoint((double)(col - 1), (double)aleft(col, row)),
                        contourPoint((double)col, (double)right(col, row)));
                } break;
                case 4:
                case 11: {
                    drawContourLine(allLines, contourPoint((double)top(col), (double)row),
                        contourPoint((double)col, (double)right(col, row)));
                } break;
                case 5:
                case 10: {
                    drawContourLine(allLines, contourPoint((double)bot(col), (double)(row - 1)),
                        contourPoint((double)top(col), (double)row));
                } break;
                case 6:
                case 9: {
                    double x0 = ainter(fold(col - 1), fold(col));
                    double x1 = ainter(fnew(col - 1), fnew(col));
                    double y0 = ainter(fold(col - 1), fnew(col - 1));
                    double y1 = ainter(fold(col), fnew(col));
                    double denom = 1.0 - (x1 - x0) * (y1 - y0);
                    if (denom == 0) {
                        break;
                    }
                    double y = (x0 * (y1 - y0) + y0) / denom;
                    double x = y * (x1 - x0) + x0;
                    double fx1 = getMapValue(col - 1, row - 1)
                        + x * (getMapValue(col, row - 1) - getMapValue(col - 1, row - 1));
                    double fx2 = getMapValue(col - 1, row)
                        + x * (getMapValue(col, row) - getMapValue(col - 1, row));
                    double f = fx1 + y * (fx2 - fx1);
                    if (f == val) {
                        drawContourLine(allLines, contourPoint((double)bot(col), (double)(row - 1)),
                            contourPoint((double)top(col), (double)row));
                        drawContourLine(allLines,
                            contourPoint((double)(col - 1), (double)aleft(col, row)),
                            contourPoint((double)col, (double)right(col, row)));
                    } else if (((f > val) && (fnew(col) > val))
                        || ((f < val) && (fnew(col) < val))) {
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
                case 8: {
                    drawContourLine(allLines,
                        contourPoint((double)(col - 1), (double)aleft(col, row)),
                        contourPoint((double)top(col), (double)row));
                } break;
                }
            }
        }

        return allLines;
    }
    //=============================================================================
    contourLineSet
    bundleLines(contourLineSet& allLines)
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
    adjustLines(
        contourLineSet& bundledLines, const ArrayOf& x, const ArrayOf& y, int numx, int numy)
    {
        const double* xp = static_cast<const double*>(x.getDataPointer());
        const double* yp = static_cast<const double*>(y.getDataPointer());
#define X(a, b) xp[(b) + (a) * numy]
#define Y(a, b) yp[(b) + (a) * numy]

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
#undef X
#undef Y
        return bundledLines;
    }
    //=============================================================================
    void
    finiteMinMax(ArrayOf zData, double& zMin, double& zMax, bool& found)
    {
        zData.promoteType(NLS_DOUBLE);
        const double* pzData = static_cast<const double*>(zData.getDataPointer());
        found = false;
        zMin = 0;
        zMax = 0;
        for (indexType k = 0; k < zData.getElementCount(); k++) {
            double value = pzData[k];
            if (!std::isfinite(value)) {
                continue;
            }
            if (!found) {
                zMin = value;
                zMax = value;
                found = true;
            } else {
                zMin = std::min(zMin, value);
                zMax = std::max(zMax, value);
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOf
contourCoordinateData(const ArrayOf& zDataInput, const ArrayOf& coordDataInput, bool isXcoord)
{
    ArrayOf zData(zDataInput);
    indexType zRows = zData.getRows();
    indexType zCols = zData.getColumns();

    if (!coordDataInput.isEmpty()) {
        ArrayOf cData(coordDataInput);
        if (cData.isVector()
            && ((isXcoord && (cData.getElementCount() == zCols))
                || (!isXcoord && (cData.getElementCount() == zRows)))) {
            cData.promoteType(NLS_DOUBLE);
            const double* qp = static_cast<const double*>(cData.getDataPointer());
            double* dp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, zRows * zCols));
            ArrayOf mat(NLS_DOUBLE, Dimensions(zRows, zCols), dp);
            for (int i = 0; i < zCols; i++) {
                for (int j = 0; j < zRows; j++) {
                    *dp++ = isXcoord ? qp[i] : qp[j];
                }
            }
            return mat;
        }
        if (cData.is2D() && (cData.getRows() == zRows) && (cData.getColumns() == zCols)) {
            cData.promoteType(NLS_DOUBLE);
            return cData;
        }
    }

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
std::vector<double>
defaultContourLevels(ArrayOf zData)
{
    double zMin = 0;
    double zMax = 0;
    bool found = false;
    finiteMinMax(zData, zMin, zMax, found);
    if (!found || zMin == zMax) {
        return {};
    }

    std::list<double> llevels = getTicksInner(zMin, zMax, false, 10);
    std::vector<double> levels(llevels.begin(), llevels.end());
    if (!levels.empty() && levels.front() == zMin) {
        levels.erase(levels.begin());
    }
    if (!levels.empty() && levels.back() == zMax) {
        levels.pop_back();
    }
    return levels;
}
//=============================================================================
contourLineSet
generateContourLines(ArrayOf zData, double level, const ArrayOf& xData, const ArrayOf& yData)
{
    contourLineSet allLines = generateAllLines(zData, level);
    contourLineSet bundledLines = bundleLines(allLines);
    return adjustLines(bundledLines, xData, yData, int(zData.getColumns()), int(zData.getRows()));
}
//=============================================================================
ArrayOf
buildContourMatrix(const contourLineCollection& contourLines, const std::vector<double>& levels)
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
    if (outcount == 0) {
        return ArrayOf::emptyConstructor(Dimensions(2, 0));
    }
    double* output = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * outcount));
    ArrayOf out = ArrayOf(NLS_DOUBLE, Dimensions(2, outcount), output);
    for (int i = 0; i < contourLines.size(); i++) {
        for (int j = 0; j < contourLines[i].size(); j++) {
            *output++ = levels[i];
            *output++ = static_cast<double>(contourLines[i][j].size());
            contourLine bline(contourLines[i][j]);
            for (int k = 0; k < bline.size(); k++) {
                *output++ = bline[k].x;
                *output++ = bline[k].y;
            }
        }
    }
    return out;
}
//=============================================================================
ArrayOf
buildContourMatrix(
    ArrayOf zData, const ArrayOf& xData, const ArrayOf& yData, const std::vector<double>& levels)
{
    contourLineCollection contourLines;
    contourLines.reserve(levels.size());
    for (double level : levels) {
        contourLines.push_back(generateContourLines(zData, level, xData, yData));
    }
    return buildContourMatrix(contourLines, levels);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
