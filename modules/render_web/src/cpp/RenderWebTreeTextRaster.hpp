//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Software rasterization of the uitree node hierarchy and the uitextarea
// multi-line text, so a headless getframe/exportapp shows the node rows and real
// line breaks the Qt widgets draw instead of a blank box / one joined line.
//=============================================================================
// Skip one JSON value (string / number / object / array / literal), advancing pos
// past it. Used to walk the nested uitree payload without a JSON library.
static void
skipJsonValue(const std::string& json, size_t& pos)
{
    skipJsonSpaces(json, pos);
    if (pos >= json.size()) {
        return;
    }
    const char c = json[pos];
    if (c == '"') {
        std::string tmp;
        parseJsonString(json, pos, tmp);
        return;
    }
    if (c == '[' || c == '{') {
        int depth = 0;
        while (pos < json.size()) {
            const char ch = json[pos];
            if (ch == '"') {
                std::string tmp;
                parseJsonString(json, pos, tmp);
                continue;
            }
            if (ch == '[' || ch == '{') {
                ++depth;
                ++pos;
            } else if (ch == ']' || ch == '}') {
                --depth;
                ++pos;
                if (depth == 0) {
                    return;
                }
            } else {
                ++pos;
            }
        }
        return;
    }
    while (pos < json.size() && json[pos] != ',' && json[pos] != '}' && json[pos] != ']') {
        ++pos;
    }
}
//=============================================================================
struct TreeRasterRow
{
    int depth = 0;
    std::string text;
    bool hasChildren = false;
};
//=============================================================================
// Flatten an array of {text, children} tree nodes (pre-order) into rows carrying
// their depth and whether they have children (for the expand marker).
static void
parseTreeNodeArray(
    const std::string& json, size_t& pos, int depth, std::vector<TreeRasterRow>& rows)
{
    skipJsonSpaces(json, pos);
    if (pos >= json.size() || json[pos] != '[') {
        return;
    }
    ++pos;
    for (;;) {
        skipJsonSpaces(json, pos);
        if (pos >= json.size() || json[pos] == ']') {
            if (pos < json.size()) {
                ++pos;
            }
            return;
        }
        if (json[pos] != '{') {
            break;
        }
        ++pos;
        TreeRasterRow row;
        row.depth = depth;
        size_t childrenPos = std::string::npos;
        for (;;) {
            skipJsonSpaces(json, pos);
            if (pos >= json.size() || json[pos] == '}') {
                if (pos < json.size()) {
                    ++pos;
                }
                break;
            }
            std::string key;
            if (json[pos] == '"') {
                parseJsonString(json, pos, key);
            } else {
                ++pos;
                continue;
            }
            skipJsonSpaces(json, pos);
            if (pos < json.size() && json[pos] == ':') {
                ++pos;
            }
            skipJsonSpaces(json, pos);
            if (key == "text" && pos < json.size() && json[pos] == '"') {
                parseJsonString(json, pos, row.text);
            } else if (key == "children") {
                row.hasChildren = true;
                childrenPos = pos;
                skipJsonValue(json, pos);
            } else {
                skipJsonValue(json, pos);
            }
            skipJsonSpaces(json, pos);
            if (pos < json.size() && json[pos] == ',') {
                ++pos;
            }
        }
        rows.push_back(row);
        if (childrenPos != std::string::npos) {
            size_t childPos = childrenPos;
            parseTreeNodeArray(json, childPos, depth + 1, rows);
        }
        skipJsonSpaces(json, pos);
        if (pos < json.size() && json[pos] == ',') {
            ++pos;
        }
    }
}
//=============================================================================
static void
rasterizeTreeControl(RasterCanvas& canvas, const WebUIControl& control, double fontSize,
    const RasterColor& fg, int supersample)
{
    rasterizeControlBox(canvas, control.x * supersample, control.y * supersample,
        control.w * supersample, control.h * supersample, parseCssColor("rgb(255,255,255)"),
        parseCssColor("rgb(143,143,143)"));
    std::vector<TreeRasterRow> rows;
    size_t nodesPos = findJsonArrayMember(control.extraJson, "nodes");
    if (nodesPos != std::string::npos) {
        parseTreeNodeArray(control.extraJson, nodesPos, 0, rows);
    }
    const double rowHeight = std::max(fontSize + 6.0, 18.0);
    const double indentWidth = 16.0;
    const RasterColor triangle = parseCssColor("rgb(90,90,90)");
    for (size_t i = 0; i < rows.size(); ++i) {
        const double rowY = control.y + 4.0 + static_cast<double>(i) * rowHeight;
        if (rowY + rowHeight > control.y + control.h) {
            break;
        }
        const double x = control.x + 6.0 + rows[i].depth * indentWidth;
        const double cy = rowY + rowHeight / 2.0;
        if (rows[i].hasChildren) {
            rasterizeArrow(canvas, x + 4.0, cy, 4.0, 3.0, 'd', triangle, supersample);
        }
        rasterizeControlLabel(
            canvas, control, x + 14.0, cy + fontSize * 0.35, "left", rows[i].text, fg);
    }
}
//=============================================================================
// Field text that honours embedded newlines: a single line is vertically
// centred (uieditfield); multiple lines are laid out top-down (uitextarea).
static void
rasterizeEditText(RasterCanvas& canvas, const WebUIControl& control, const std::string& text,
    const RasterColor& fg, double fontSize, double baseline, int supersample)
{
    if (text.find('\n') == std::string::npos) {
        rasterizeControlLabel(canvas, control, control.x + 6.0, baseline, "left", text, fg);
        return;
    }
    const double lineHeight = fontSize * 1.35;
    double y = control.y + 4.0 + fontSize;
    size_t start = 0;
    for (;;) {
        const size_t nl = text.find('\n', start);
        const std::string line
            = (nl == std::string::npos) ? text.substr(start) : text.substr(start, nl - start);
        rasterizeControlLabel(canvas, control, control.x + 6.0, y, "left", line, fg);
        y += lineHeight;
        if (nl == std::string::npos || y > control.y + control.h) {
            break;
        }
        start = nl + 1;
    }
}
//=============================================================================
