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
#include "ParallelSort.hpp"
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <limits>
#include <tuple>
#include <unordered_map>
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
    constexpr int NO_EDGE = -1;
    constexpr int NO_SEGMENT = -1;
    //=============================================================================
    // One piece of level line, with the grid edge each of its two ends sits on.
    //
    // bundleLines() uses the edges to rejoin pieces where the level runs through a grid vertex:
    // four pieces then meet at the same coordinates and only the edges say which one continues
    // which. An interior edge belongs to two cells, so it always joins exactly two pieces.
    struct contourSegment
    {
        contourPoint p1;
        contourPoint p2;
        int edge1;
        int edge2; // NO_EDGE when the piece is a lone vertex
    };
    //=============================================================================
    typedef std::vector<contourSegment> contourSegmentSet;
    //=============================================================================
    // Emits the segment a cell contributes. p1 and p2 are ordered so that the part of the cell
    // above the level is on the left of the walk. `reversed` swaps them for the complementary cell,
    // so the segments of one line meet head to tail.
    inline void
    drawContourLine(contourSegmentSet& segments, contourPoint p1, int edge1, contourPoint p2,
        int edge2, bool reversed = false)
    {
        if (std::isfinite(p1.x) && std::isfinite(p1.y) && std::isfinite(p2.x)
            && std::isfinite(p2.y)) {
            if (reversed) {
                segments.push_back(contourSegment { p2, p1, edge2, edge1 });
            } else {
                segments.push_back(contourSegment { p1, p2, edge1, edge2 });
            }
        }
    }
    //=============================================================================
    // A crossing no cell can turn into a segment: it sits on an edge of a cell put out of use by a
    // NaN corner. bundleLines() folds it into the line of a neighbour reaching the same edge, and
    // keeps one copy of the pair left by two unusable cells.
    inline void
    drawContourVertex(contourSegmentSet& segments, contourPoint p, int edge)
    {
        if (std::isfinite(p.x) && std::isfinite(p.y)) {
            segments.push_back(contourSegment { p, p, edge, NO_EDGE });
        }
    }
    //=============================================================================
    contourSegmentSet
    generateAllLines(ArrayOf m, double val)
    {
        contourSegmentSet allLines;

        m.promoteType(NLS_DOUBLE);
        const double* func = static_cast<const double*>(m.dataPointer());
        int numy = int(m.rows());
        int numx = int(m.columns());

        auto getMapValue = [&](int x, int y) { return func[(y) + (x)*numy]; };
        // Side of the level taken by a corner that holds exactly the level.
        //
        // Above, so a plateau on the level keeps its region and the line runs along the plateau
        // rim. A surface that never goes below the level would get no line that way, so it reads
        // the plateau as below and draws the rim. Either way the crossings sit on the corner, so a
        // level touching one grid point comes out as a line of a single vertex.
        bool anyBelow = false;
        for (indexType k = 0; k < m.elementCount(); k++) {
            if (func[k] < val) {
                anyBelow = true;
                break;
            }
        }
        auto isAbove = [&](double v) { return anyBelow ? (v >= val) : (v > val); };
        // Numbering of the grid edges: the horizontal ones row by row, then the vertical ones
        // column by column. Two neighbouring cells give their shared edge the same number.
        int horizontalEdges = numy * (numx - 1);
        auto horizontalEdge = [&](int y, int col) { return (y) * (numx - 1) + (col - 1); };
        auto verticalEdge
            = [&](int x, int row) { return horizontalEdges + (x) * (numy - 1) + (row - 1); };
        // Where the level cuts an edge, as a fraction from the first end to the second.
        //
        // An infinite corner still compares to the level, so an edge with one infinite end is cut
        // like any other and the crossing sits on the finite end. The division gives that when the
        // infinity is the second end; the first-end case is handled explicitly. With both ends
        // infinite the crossing stays on the end the grid reaches first: the left end of a row, the
        // top end of a column.
        auto ainter = [&](double a, double b, bool alongRow) {
            if (std::isfinite(a)) {
                return (val - a) / (b - a);
            }
            if (std::isfinite(b)) {
                return 1.;
            }
            return alongRow ? 0. : 1.;
        };
        // An edge is cut when its two ends can be compared to the level and
        // fall on either side of it.
        auto isCut = [&](double a, double b) {
            return !std::isnan(a) && !std::isnan(b) && (isAbove(a) != isAbove(b));
        };

        for (int row = 1; row < numy; row++) {
            auto fold = [&](int x) { return getMapValue(x, row - 1); };
            auto fnew = [&](int x) { return getMapValue(x, row); };
            auto aleft
                = [&](int i, int j) { return ((j)-1) + ainter(fold((i)-1), fnew((i)-1), false); };
            auto top = [&](int i) { return ((i)-1) + ainter(fnew(i - 1), fnew(i), true); };
            auto bot = [&](int i) { return ((i)-1) + ainter(fold(i - 1), fold(i), true); };
            auto right = [&](int i, int j) { return ((j)-1) + ainter(fold(i), fnew(i), false); };
            auto bottomPoint = [&](int i) { return contourPoint(bot(i), (double)(row - 1)); };
            auto topPoint = [&](int i) { return contourPoint(top(i), (double)row); };
            auto leftPoint = [&](int i) { return contourPoint((double)(i - 1), aleft(i, row)); };
            auto rightPoint = [&](int i) { return contourPoint((double)i, right(i, row)); };

            for (int col = 1; col < numx; col++) {
                double bottomLeft = fold(col - 1);
                double bottomRight = fold(col);
                double topLeft = fnew(col - 1);
                double topRight = fnew(col);
                // Only a NaN corner takes a cell out of use: an infinite one
                // still says which side of the level it is on.
                if (std::isnan(bottomLeft) || std::isnan(bottomRight) || std::isnan(topLeft)
                    || std::isnan(topRight)) {
                    // Such a cell has no inside for the level line to cross, but an edge with
                    // comparable ends still carries a crossing. It joins no segment, so it is
                    // written on its own.
                    if (isCut(bottomLeft, bottomRight)) {
                        drawContourVertex(allLines, bottomPoint(col), horizontalEdge(row - 1, col));
                    }
                    if (isCut(topLeft, topRight)) {
                        drawContourVertex(allLines, topPoint(col), horizontalEdge(row, col));
                    }
                    if (isCut(bottomLeft, topLeft)) {
                        drawContourVertex(allLines, leftPoint(col), verticalEdge(col - 1, row));
                    }
                    if (isCut(bottomRight, topRight)) {
                        drawContourVertex(allLines, rightPoint(col), verticalEdge(col, row));
                    }
                    continue;
                }
                int bottomEdge = horizontalEdge(row - 1, col);
                int topEdge = horizontalEdge(row, col);
                int leftEdge = verticalEdge(col - 1, row);
                int rightEdge = verticalEdge(col, row);
                int l = 0;
                if (isAbove(bottomRight)) {
                    l += 1;
                }
                if (isAbove(bottomLeft)) {
                    l += 2;
                }
                if (isAbove(topRight)) {
                    l += 4;
                }
                if (isAbove(topLeft)) {
                    l += 8;
                }
                // The cell is cut on two of its edges (four at a saddle). Each segment leaves
                // oriented with the side above the level on the left.
                switch (l) {
                case 1:
                case 14: {
                    // bottom-right corner cut off, above the level when l == 1
                    drawContourLine(allLines, rightPoint(col), rightEdge, bottomPoint(col),
                        bottomEdge, l == 14);
                } break;
                case 2:
                case 13: {
                    // bottom-left corner cut off, above the level when l == 2
                    drawContourLine(
                        allLines, bottomPoint(col), bottomEdge, leftPoint(col), leftEdge, l == 13);
                } break;
                case 3:
                case 12: {
                    // cut across, bottom row above the level when l == 3
                    drawContourLine(
                        allLines, rightPoint(col), rightEdge, leftPoint(col), leftEdge, l == 12);
                } break;
                case 4:
                case 11: {
                    // top-right corner cut off, above the level when l == 4
                    drawContourLine(
                        allLines, topPoint(col), topEdge, rightPoint(col), rightEdge, l == 11);
                } break;
                case 5:
                case 10: {
                    // cut across, right column above the level when l == 5
                    drawContourLine(
                        allLines, topPoint(col), topEdge, bottomPoint(col), bottomEdge, l == 10);
                } break;
                case 6:
                case 9: {
                    // A saddle: the cell is cut on all four edges and the two arcs can pair the
                    // crossings either way. One pairing goes round the bottom-left and top-right
                    // corners, the other round the top-left and bottom-right pair; the pair left
                    // alone keeps the middle of the cell.
                    //
                    // The walk goes round bottom-left and top-right when the mean of the four
                    // corners lands between the level and the top-left corner, level end excluded.
                    // The mean alone gives the same answer everywhere except past the top-left
                    // corner: a corner far beyond the level drags the mean beyond that corner too,
                    // and the line goes back to the other pair. A mean with no value (opposite
                    // infinities) rounds the bottom-left and top-right pair.
                    bool topLeftAbove = (l == 9);
                    double topLeftCorner = fnew(col - 1);
                    double mean = (fold(col - 1) + fold(col) + fnew(col - 1) + fnew(col)) / 4.;
                    bool cutsBottomLeftAndTopRight = !((mean <= std::min(topLeftCorner, val))
                        || (mean > std::max(topLeftCorner, val)));
                    if (cutsBottomLeftAndTopRight) {
                        // bottom-left and top-right corners cut off
                        drawContourLine(allLines, bottomPoint(col), bottomEdge, leftPoint(col),
                            leftEdge, topLeftAbove);
                        drawContourLine(allLines, topPoint(col), topEdge, rightPoint(col),
                            rightEdge, topLeftAbove);
                    } else {
                        // top-left and bottom-right corners cut off
                        drawContourLine(allLines, leftPoint(col), leftEdge, topPoint(col), topEdge,
                            !topLeftAbove);
                        drawContourLine(allLines, rightPoint(col), rightEdge, bottomPoint(col),
                            bottomEdge, !topLeftAbove);
                    }
                } break;
                case 7:
                case 8: {
                    // top-left corner cut off, above the level when l == 8
                    drawContourLine(
                        allLines, leftPoint(col), leftEdge, topPoint(col), topEdge, l == 7);
                } break;
                }
            }
        }

        return allLines;
    }
    //=============================================================================
    // Where a line is opened. The walk reads the horizontal edges of the top row, then the bottom
    // row, left to right; then the vertical edges, column by column from the left and each column
    // from the top; then the horizontal edges inside the grid. A line that stops at the edge of the
    // grid is opened at whichever of its two ends comes first in that order.
    inline std::tuple<int, int, int>
    edgeScanOrder(int edge, int numy, int numx)
    {
        int horizontalEdges = numy * (numx - 1);
        if (edge < horizontalEdges) {
            int y = edge / (numx - 1);
            int col = edge % (numx - 1);
            return std::make_tuple(((y == 0) || (y == numy - 1)) ? 0 : 2, -y, col);
        }
        int vertical = edge - horizontalEdges;
        int x = vertical / (numy - 1);
        int row = vertical % (numy - 1);
        return std::make_tuple(1, x, -row);
    }
    //=============================================================================
    // Threads the pieces left by the cells into whole lines.
    //
    // Two pieces continue one another when they reach the same grid edge, which an interior edge
    // lets exactly two of them do. A line grows one direction at a time with nothing to choose.
    // Lines with two ends go first, each opened at the end the scan order reaches first; what is
    // left closes on itself and is opened anywhere.
    contourLineSet
    bundleLines(contourSegmentSet& allLines, int numy, int numx)
    {
        contourLineSet bundledLines;
        if (allLines.empty() || (numy < 2) || (numx < 2)) {
            return bundledLines;
        }
        std::unordered_map<int, std::pair<int, int>> reaching;
        reaching.reserve(allLines.size() * 2);
        std::vector<bool> used(allLines.size(), false);
        auto attach = [&](int edge, int index) {
            if (edge == NO_EDGE) {
                return;
            }
            auto found = reaching.find(edge);
            if (found == reaching.end()) {
                reaching.emplace(edge, std::make_pair(index, NO_SEGMENT));
            } else if (found->second.second == NO_SEGMENT) {
                found->second.second = index;
            }
        };
        for (size_t k = 0; k < allLines.size(); k++) {
            if (allLines[k].edge2 == NO_EDGE) {
                // The same crossing twice, left by the two unusable cells an
                // edge separates: one copy of it is the line.
                auto found = reaching.find(allLines[k].edge1);
                if ((found != reaching.end()) && (allLines[found->second.first].edge2 == NO_EDGE)) {
                    used[k] = true;
                    continue;
                }
            }
            attach(allLines[k].edge1, (int)k);
            attach(allLines[k].edge2, (int)k);
        }
        std::vector<int> starts;
        starts.reserve(reaching.size());
        for (const auto& entry : reaching) {
            starts.push_back(entry.first);
        }
        auto isLoneVertex = [&](int index) { return allLines[index].edge2 == NO_EDGE; };
        // An edge where an unusable cell left a lone crossing is where a line cut short by the NaN
        // ends. Such a line is opened at that gap and walked out to the grid, so a gap edge is
        // preferred as a starting point over an ordinary end.
        auto isGapEdge = [&](int edge) {
            auto found = reaching.find(edge);
            if (found == reaching.end()) {
                return false;
            }
            int a = found->second.first;
            int b = found->second.second;
            return ((a != NO_SEGMENT) && isLoneVertex(a)) || ((b != NO_SEGMENT) && isLoneVertex(b));
        };
        std::sort(starts.begin(), starts.end(), [&](int a, int b) {
            bool gapA = isGapEdge(a);
            bool gapB = isGapEdge(b);
            if (gapA != gapB) {
                return gapA;
            }
            return edgeScanOrder(a, numy, numx) < edgeScanOrder(b, numy, numx);
        });
        auto pieceAt = [&](int edge) {
            if (edge == NO_EDGE) {
                return NO_SEGMENT;
            }
            auto found = reaching.find(edge);
            if (found == reaching.end()) {
                return NO_SEGMENT;
            }
            if ((found->second.second != NO_SEGMENT) && !used[found->second.second]) {
                return found->second.second;
            }
            if ((found->second.first != NO_SEGMENT) && !used[found->second.first]) {
                return found->second.first;
            }
            return NO_SEGMENT;
        };
        // What to leave an edge by. A piece that can carry the line on comes
        // before a lone vertex, which is where a line stops; of two that can,
        // the one the later cell left is the one a closed line leaves by.
        auto startPieceAt = [&](int edge) {
            auto found = reaching.find(edge);
            if (found == reaching.end()) {
                return NO_SEGMENT;
            }
            int later = found->second.second;
            int earlier = found->second.first;
            if ((later != NO_SEGMENT) && !used[later] && !isLoneVertex(later)) {
                return later;
            }
            if ((earlier != NO_SEGMENT) && !used[earlier] && !isLoneVertex(earlier)) {
                return earlier;
            }
            return pieceAt(edge);
        };
        auto walkFrom = [&](int index, int from) {
            used[index] = true;
            // A lone vertex on the edge the line starts from is the crossing the
            // line already starts at, seen from the unusable cell next door.
            auto holds = reaching.find(from);
            if (holds != reaching.end()) {
                int other
                    = (holds->second.first == index) ? holds->second.second : holds->second.first;
                if ((other != NO_SEGMENT) && !used[other] && isLoneVertex(other)) {
                    used[other] = true;
                }
            }
            contourLine line;
            int end = NO_EDGE;
            if (allLines[index].edge1 == from) {
                line.push_back(allLines[index].p1);
                line.push_back(allLines[index].p2);
                end = allLines[index].edge2;
            } else {
                line.push_back(allLines[index].p2);
                line.push_back(allLines[index].p1);
                end = allLines[index].edge1;
            }
            while ((end != NO_EDGE) && (end != from)) {
                int next = pieceAt(end);
                if (next == NO_SEGMENT) {
                    break;
                }
                used[next] = true;
                if (allLines[next].edge1 == end) {
                    line.push_back(allLines[next].p2);
                    end = allLines[next].edge2;
                } else {
                    line.push_back(allLines[next].p1);
                    end = allLines[next].edge1;
                }
            }
            // A cell with a corner exactly on the level is cut twice at that
            // corner and walked through without moving. The vertex belongs to the
            // line once; a line that never moves at all is that one vertex.
            contourLine trimmed;
            trimmed.reserve(line.size());
            for (const contourPoint& point : line) {
                if (trimmed.empty() || !samePoint(trimmed.back(), point)) {
                    trimmed.push_back(point);
                }
            }
            bundledLines.push_back(trimmed);
        };
        // An edge only one piece can carry a line through is an end of that
        // line; the lone vertex an unusable cell may have left there stops the
        // line rather than carrying it on.
        auto carriesOn
            = [&](int index) { return (index != NO_SEGMENT) && !isLoneVertex(index) ? 1 : 0; };
        for (int edge : starts) {
            const std::pair<int, int>& holds = reaching[edge];
            if (carriesOn(holds.first) + carriesOn(holds.second) > 1) {
                continue;
            }
            int index = startPieceAt(edge);
            if (index == NO_SEGMENT) {
                continue;
            }
            walkFrom(index, edge);
        }
        for (int edge : starts) {
            for (int index = startPieceAt(edge); index != NO_SEGMENT; index = startPieceAt(edge)) {
                // A closed loop can be opened anywhere, but its direction is fixed: entering the
                // first piece at its own start edge walks it p1 -> p2, the orientation every piece
                // was emitted with (above the level on the left). A loop around a maximum is then
                // counter-clockwise and one around a minimum clockwise.
                walkFrom(index, allLines[index].edge1);
            }
        }
        return bundledLines;
    }
    //=============================================================================
    contourLineSet
    adjustLines(
        contourLineSet& bundledLines, const ArrayOf& x, const ArrayOf& y, int numx, int numy)
    {
        const double* xp = static_cast<const double*>(x.dataPointer());
        const double* yp = static_cast<const double*>(y.dataPointer());
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
        const double* pzData = static_cast<const double*>(zData.dataPointer());
        found = false;
        zMin = 0;
        zMax = 0;
        for (indexType k = 0; k < zData.elementCount(); k++) {
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
    indexType zRows = zData.rows();
    indexType zCols = zData.columns();

    if (!coordDataInput.isEmpty()) {
        ArrayOf cData(coordDataInput);
        if (cData.isVector()
            && ((isXcoord && (cData.elementCount() == zCols))
                || (!isXcoord && (cData.elementCount() == zRows)))) {
            cData.promoteType(NLS_DOUBLE);
            const double* qp = static_cast<const double*>(cData.dataPointer());
            double* dp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, zRows * zCols));
            ArrayOf mat(NLS_DOUBLE, Dimensions(zRows, zCols), dp);
            for (int i = 0; i < zCols; i++) {
                for (int j = 0; j < zRows; j++) {
                    *dp++ = isXcoord ? qp[i] : qp[j];
                }
            }
            return mat;
        }
        if (cData.is2D() && (cData.rows() == zRows) && (cData.columns() == zCols)) {
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
// Automatic contour levels are the multiples of a single round step.
//
// The step is the smallest of 1, 2 or 5 times a power of ten that cuts the
// data range into fewer than twelve slices: a range of 11.9 keeps a step of 1
// while a range of 12 moves up to 2. Contour levels and axis ticks look alike
// but do not agree, so this rule stays independent of getTicksInner().
static double
automaticLevelStep(double zMin, double zMax)
{
    double range = zMax - zMin;
    if (!std::isfinite(range) || range <= 0) {
        return 0;
    }
    constexpr double MAX_SLICES = 12.;
    constexpr double SLICE_TOLERANCE = 1e-12;
    constexpr double MULTIPLIERS[] = { 1., 2., 5. };
    double firstDecade = std::floor(std::log10(range / MAX_SLICES));
    for (int decade = 0; decade < 4; ++decade) {
        double magnitude = std::pow(10., firstDecade + decade);
        for (double multiplier : MULTIPLIERS) {
            double step = multiplier * magnitude;
            if (range <= step * MAX_SLICES * (1. - SLICE_TOLERANCE)) {
                return step;
            }
        }
    }
    return range;
}
//=============================================================================
// Brings a level back to the decimal it stands for: index * step drifts by an
// ulp as soon as the step is not a binary fraction, and a level list is read
// back and compared by users.
static double
roundLevelToStep(double level, double step)
{
    if (step >= 1.) {
        return level;
    }
    double decimals = std::pow(10., -std::floor(std::log10(step)));
    double scaled = level * decimals;
    if (!std::isfinite(scaled) || std::fabs(scaled) >= 1e15) {
        return level;
    }
    return std::round(scaled) / decimals;
}
//=============================================================================
double
defaultContourLevelStep(ArrayOf zData)
{
    double zMin = 0;
    double zMax = 0;
    bool found = false;
    finiteMinMax(zData, zMin, zMax, found);
    if (!found) {
        return 0;
    }
    return automaticLevelStep(zMin, zMax);
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
    double step = automaticLevelStep(zMin, zMax);
    if (step <= 0) {
        return {};
    }
    // A multiple of the step that lands exactly on a bound is a level, except
    // at the bottom of data whose minimum is positive or zero: there the first
    // level is the next multiple up. The contour level rule puts that
    // frontier at zero, as if the minimum were nudged away from the origin
    // before the first level is picked.
    constexpr double INDEX_TOLERANCE = 1e-10;
    constexpr double BOUND_TOLERANCE = 4. * std::numeric_limits<double>::epsilon();
    double lowQuotient = zMin / step;
    double highQuotient = zMax / step;
    double lowIndex
        = std::ceil(lowQuotient - INDEX_TOLERANCE * std::max(1., std::fabs(lowQuotient)));
    double highIndex
        = std::floor(highQuotient + INDEX_TOLERANCE * std::max(1., std::fabs(highQuotient)));
    if (zMin >= 0 && (lowIndex * step) <= (zMin + std::fabs(zMin) * BOUND_TOLERANCE)) {
        lowIndex += 1.;
    }
    std::vector<double> levels;
    if (highIndex < lowIndex) {
        return levels;
    }
    levels.reserve(static_cast<size_t>(highIndex - lowIndex) + 1);
    for (double index = lowIndex; index <= highIndex; index += 1.) {
        double level = roundLevelToStep(index * step, step);
        if (std::fabs(level) <= std::fabs(step) * 1e-12) {
            level = 0.;
        }
        levels.push_back(level);
    }
    return levels;
}
//=============================================================================
contourLineSet
generateContourLines(ArrayOf zData, double level, const ArrayOf& xData, const ArrayOf& yData)
{
    int numy = int(zData.rows());
    int numx = int(zData.columns());
    contourSegmentSet allLines = generateAllLines(zData, level);
    contourLineSet bundledLines = bundleLines(allLines, numy, numx);
    return adjustLines(bundledLines, xData, yData, numx, numy);
}
//=============================================================================
namespace {
    //=============================================================================
    inline void
    hashCombine(std::size_t& seed, std::size_t value)
    {
        seed ^= value + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
    }
    //=============================================================================
    void
    hashArrayValues(std::size_t& seed, const ArrayOf& arrayInput)
    {
        ArrayOf value(arrayInput);
        value.promoteType(NLS_DOUBLE);
        hashCombine(seed, static_cast<std::size_t>(value.rows()));
        hashCombine(seed, static_cast<std::size_t>(value.columns()));
        indexType count = value.elementCount();
        if (count == 0) {
            return;
        }
        const double* data = static_cast<const double*>(value.dataPointer());
        std::hash<double> hasher;
        for (indexType k = 0; k < count; k++) {
            hashCombine(seed, hasher(data[k]));
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
std::size_t
contourInputsSignature(const ArrayOf& zData, const ArrayOf& xData, const ArrayOf& yData,
    const std::vector<double>& levels)
{
    std::size_t seed = 0x1234567uLL;
    hashArrayValues(seed, zData);
    hashArrayValues(seed, xData);
    hashArrayValues(seed, yData);
    std::hash<double> hasher;
    hashCombine(seed, levels.size());
    for (double level : levels) {
        hashCombine(seed, hasher(level));
    }
    return seed;
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
        return ArrayOf::empty(Dimensions(2, 0));
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
