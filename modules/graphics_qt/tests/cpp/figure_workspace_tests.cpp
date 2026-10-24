//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FigureWorkspaceHelpers.hpp"
#include <cstdlib>
#include <iostream>
//=============================================================================
namespace {
void
require(bool condition, const char* message)
{
    if (!condition) {
        std::cerr << message << '\n';
        std::exit(EXIT_FAILURE);
    }
}
//=============================================================================
void
testNavigationOrdering()
{
    std::deque<QJsonObject> queue;
    Nelson::enqueueFigureCommand(
        queue, { { "action", "view" }, { "axesHandle", 1 }, { "azimuth", 20 } });
    Nelson::enqueueFigureCommand(
        queue, { { "action", "view" }, { "axesHandle", 1 }, { "azimuth", 40 } });
    Nelson::enqueueFigureCommand(queue, { { "action", "fit" }, { "axesHandle", 1 } });
    Nelson::enqueueFigureCommand(
        queue, { { "action", "view" }, { "axesHandle", 1 }, { "azimuth", 60 } });
    require(queue.size() == 3, "view coalescing must preserve the Fit barrier");
    require(queue[0]["azimuth"].toInt() == 40, "latest held view must precede Fit");
    require(queue[1]["action"] == "fit", "Fit must remain between gestures");
    Nelson::enqueueFigureCommand(
        queue, { { "action", "view" }, { "axesHandle", 2 }, { "azimuth", 80 } });
    require(queue.size() == 4, "different subplots must never coalesce");
    queue.clear();
    Nelson::enqueueFigureCommand(
        queue, { { "action", "pan" }, { "axesHandle", 1 }, { "dxFrac", 0.1 }, { "dyFrac", 0.2 } });
    Nelson::enqueueFigureCommand(
        queue, { { "action", "pan" }, { "axesHandle", 1 }, { "dxFrac", 0.3 }, { "dyFrac", -0.1 } });
    require(queue.size() == 1, "pending pan must stay bounded");
    require(std::abs(queue[0]["dxFrac"].toDouble() - 0.4) < 1e-12,
        "pan must accumulate horizontal displacement");
    require(std::abs(queue[0]["dyFrac"].toDouble() - 0.1) < 1e-12,
        "pan must accumulate vertical displacement");
    queue.clear();
    Nelson::enqueueFigureCommand(
        queue, { { "action", "zoom" }, { "axesHandle", 1 }, { "factor", 1.25 } });
    Nelson::enqueueFigureCommand(
        queue, { { "action", "zoom" }, { "axesHandle", 1 }, { "factor", 0.8 } });
    Nelson::enqueueFigureCommand(queue, { { "action", "restore" }, { "axesHandle", 1 } });
    require(queue.size() == 2 && queue[0]["factor"].toDouble() == 1,
        "opposite wheel deltas must cancel before Restore");
    queue.clear();
    Nelson::enqueueFigureCommand(queue,
        { { "action", "view" }, { "axesHandle", 1 }, { "historyGroup", "first" },
            { "azimuth", 10 } });
    Nelson::enqueueFigureCommand(queue,
        { { "action", "view" }, { "axesHandle", 1 }, { "historyGroup", "second" },
            { "azimuth", 20 } });
    require(queue.size() == 2, "different gestures must keep distinct history entries");
}
//=============================================================================
void
testInspectorParsing()
{
    bool valid = false;
    const QJsonArray previous { 1, 2, 3 };
    auto parsed = Nelson::parseFigureProperty("[ 4 5 6 ]", previous, valid);
    require(
        valid && parsed.toArray() == QJsonArray({ 4, 5, 6 }), "space-separated vectors must parse");
    Nelson::parseFigureProperty("[1, 2]", previous, valid);
    require(!valid, "wrong vector dimensions must be rejected");
    Nelson::parseFigureProperty("[1, null, 3]", previous, valid);
    require(!valid, "null must not be accepted as a numeric component");
    Nelson::parseFigureProperty("nan", QJsonValue(1.0), valid);
    require(!valid, "non-finite scalars must be rejected");
    parsed = Nelson::parseFigureProperty("none", QJsonValue("flat"), valid);
    require(valid && parsed == "none", "color modes must remain strings");
    parsed = Nelson::parseFigureProperty("[0.2, 0.4, 0.6]", QJsonValue("flat"), valid, true);
    require(valid && parsed.toArray().size() == 3, "color modes must be editable as RGB vectors");
    parsed = Nelson::parseFigureProperty("[A title]", QJsonValue("Title"), valid);
    require(valid && parsed == "[A title]", "brackets in titles must remain text");
    parsed = Nelson::parseFigureProperty("none", QJsonArray({ 1, 0, 0 }), valid, true);
    require(valid && parsed == "none", "RGB colors must be editable as color modes");
    require(Nelson::figureObjectArray(QJsonObject { { "handle", 7 } }).size() == 1,
        "single-object state responses must retain their object");
}
//=============================================================================
} // namespace
//=============================================================================
int
main()
{
    testNavigationOrdering();
    testInspectorParsing();
    return EXIT_SUCCESS;
}
//=============================================================================
