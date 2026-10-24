//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
static void
skipJsonSpaces(const std::string& s, size_t& pos)
{
    while (pos < s.size() && std::isspace(static_cast<unsigned char>(s[pos]))) {
        ++pos;
    }
}
//=============================================================================
static bool
parseJsonString(const std::string& s, size_t& pos, std::string& out)
{
    if (pos >= s.size() || s[pos] != '"') {
        return false;
    }
    out.clear();
    for (++pos; pos < s.size(); ++pos) {
        const char ch = s[pos];
        if (ch == '"') {
            ++pos;
            return true;
        }
        if (ch != '\\') {
            out.push_back(ch);
            continue;
        }
        if (++pos >= s.size()) {
            return false;
        }
        const char esc = s[pos];
        if (esc == '"' || esc == '\\' || esc == '/') {
            out.push_back(esc);
        } else if (esc == 'n') {
            out.push_back('\n');
        } else if (esc == 't') {
            out.push_back('\t');
        } else if (esc == 'u') {
            if (pos + 4 >= s.size()) {
                return false;
            }
            out.push_back('?');
            pos += 4;
        }
    }
    return false;
}
//=============================================================================
static size_t
findJsonArrayMember(const std::string& json, const char* name)
{
    const std::string key = std::string("\"") + name + "\"";
    size_t pos = json.find(key);
    if (pos == std::string::npos) {
        return pos;
    }
    pos = json.find(':', pos + key.size());
    if (pos == std::string::npos) {
        return pos;
    }
    ++pos;
    skipJsonSpaces(json, pos);
    return pos < json.size() && json[pos] == '[' ? pos : std::string::npos;
}
//=============================================================================
static std::vector<std::string>
parseJsonStringArray(const std::string& json, size_t& pos)
{
    std::vector<std::string> result;
    if (pos >= json.size() || json[pos] != '[') {
        return result;
    }
    for (++pos;;) {
        skipJsonSpaces(json, pos);
        if (pos >= json.size()) {
            return result;
        }
        if (json[pos] == ']') {
            ++pos;
            return result;
        }
        std::string text;
        if (!parseJsonString(json, pos, text)) {
            return result;
        }
        result.push_back(text);
        skipJsonSpaces(json, pos);
        if (pos < json.size() && json[pos] == ',') {
            ++pos;
        }
    }
}
//=============================================================================
static std::vector<std::vector<std::string>>
parseTableRows(const std::string& json)
{
    std::vector<std::vector<std::string>> rows;
    size_t pos = findJsonArrayMember(json, "rows");
    if (pos == std::string::npos) {
        return rows;
    }
    for (++pos;;) {
        skipJsonSpaces(json, pos);
        if (pos >= json.size() || json[pos] == ']') {
            return rows;
        }
        const size_t beforeRow = pos;
        std::vector<std::string> row = parseJsonStringArray(json, pos);
        if (pos == beforeRow) {
            return rows;
        }
        rows.push_back(std::move(row));
        skipJsonSpaces(json, pos);
        if (pos < json.size() && json[pos] == ',') {
            ++pos;
        }
    }
}
//=============================================================================
static std::vector<std::string>
parseTableColumns(const std::string& json)
{
    size_t pos = findJsonArrayMember(json, "columns");
    return pos == std::string::npos ? std::vector<std::string>() : parseJsonStringArray(json, pos);
}
//=============================================================================
static std::vector<double>
parseJsonNumberOrNullArray(const std::string& json, const char* name)
{
    std::vector<double> result;
    size_t pos = findJsonArrayMember(json, name);
    if (pos == std::string::npos) {
        return result;
    }
    for (++pos;;) {
        skipJsonSpaces(json, pos);
        if (pos >= json.size()) {
            return result;
        }
        if (json[pos] == ']') {
            ++pos;
            return result;
        }
        if (json.compare(pos, 4, "null") == 0) {
            result.push_back(0.0);
            pos += 4;
        } else {
            char* end = nullptr;
            double value = std::strtod(json.c_str() + pos, &end);
            if (end == json.c_str() + pos) {
                return result;
            }
            result.push_back(std::isfinite(value) && value > 0.0 ? value : 0.0);
            pos = static_cast<size_t>(end - json.c_str());
        }
        skipJsonSpaces(json, pos);
        if (pos < json.size() && json[pos] == ',') {
            ++pos;
        }
    }
}
//=============================================================================
static std::vector<double>
tableColumnWidths(const std::string& json, size_t columnCount, double controlWidth)
{
    std::vector<double> parsed = parseJsonNumberOrNullArray(json, "columnWidths");
    if (parsed.size() < columnCount) {
        parsed.resize(columnCount, 0.0);
    }
    double fixedWidth = 0.0;
    size_t autoCount = 0;
    bool hasFixedWidth = false;
    for (size_t c = 0; c < columnCount; ++c) {
        if (parsed[c] > 0.0) {
            fixedWidth += parsed[c];
            hasFixedWidth = true;
        } else {
            ++autoCount;
        }
    }
    if (!hasFixedWidth) {
        return std::vector<double>(columnCount, controlWidth / static_cast<double>(columnCount));
    }
    if (autoCount > 0) {
        const double remaining = std::max(0.0, controlWidth - fixedWidth);
        const double autoWidth
            = remaining > 0.0 ? remaining / static_cast<double>(autoCount) : 76.0;
        for (size_t c = 0; c < columnCount; ++c) {
            if (parsed[c] <= 0.0) {
                parsed[c] = autoWidth;
            }
        }
    }
    return parsed;
}
//=============================================================================
static void
rasterizeTableCellText(RasterCanvas& canvas, const WebUIControl& control, double left, double top,
    double cellWidth, double cellHeight, const std::string& text, const RasterColor& color)
{
    if (cellWidth <= 7.0 || cellHeight <= 7.0 || text.empty()) {
        return;
    }
    const double baseline = top + cellHeight / 2.0 + control.fontSize * 0.35;
    rasterizeControlLabel(canvas, control, left + 5.0, baseline, "left", text, color);
}
//=============================================================================
static void
rasterizeTableControl(RasterCanvas& canvas, const WebUIControl& control, int supersample)
{
    const std::vector<std::string> columns = parseTableColumns(control.extraJson);
    const std::vector<std::vector<std::string>> rows = parseTableRows(control.extraJson);
    size_t columnCount = columns.size();
    for (const auto& row : rows) {
        columnCount = std::max(columnCount, row.size());
    }
    if (columnCount == 0) {
        return;
    }

    const double x = control.x * supersample;
    const double y = control.y * supersample;
    const double w = control.w * supersample;
    const double h = control.h * supersample;
    const RasterColor border = parseCssColor("rgb(143,143,143)");
    const RasterColor grid = parseCssColor("rgb(210,210,210)");
    const RasterColor headerFill = parseCssColor("rgb(242,242,242)");
    rasterizeControlBox(canvas, x, y, w, h, parseCssColor("rgb(255,255,255)"), border);

    const double fontSize = std::max(8.0, control.fontSize > 0 ? control.fontSize : 12.0);
    const double headerHeight = std::min(control.h, std::max(fontSize + 8.0, 22.0));
    const double rowHeight = std::max(fontSize + 8.0, 20.0);
    const double gutterWidth = std::min(control.w * 0.5, 34.0);
    const double dataX = control.x + gutterWidth;
    const std::vector<double> columnWidths
        = tableColumnWidths(control.extraJson, columnCount, std::max(0.0, control.w - gutterWidth));
    canvas.fillRect(x + 1.0, y + 1.0, std::max(0.0, w - 2.0),
        std::max(0.0, headerHeight * supersample - 1.0), headerFill);
    canvas.fillRect(x + 1.0, (control.y + headerHeight) * supersample,
        std::max(0.0, gutterWidth * supersample - 1.0),
        std::max(0.0, h - headerHeight * supersample - 1.0), headerFill);
    canvas.strokePolyline({ { dataX * supersample, y }, { dataX * supersample, y + h } },
        1.0 * supersample, grid, {});
    double left = dataX;
    for (size_t c = 0; c < columnCount; ++c) {
        const double columnWidth = columnWidths[c];
        if (c < columns.size()) {
            rasterizeTableCellText(
                canvas, control, left, control.y, columnWidth, headerHeight, columns[c], border);
        }
        const double lineX = (left + columnWidth) * supersample;
        canvas.strokePolyline({ { lineX, y }, { lineX, y + h } }, 1.0 * supersample, grid, {});
        left += columnWidth;
    }
    canvas.strokePolyline({ { x, (control.y + headerHeight) * supersample },
                              { x + w, (control.y + headerHeight) * supersample } },
        1.0 * supersample, grid, {});

    const RasterColor textColor = controlColor(control.foreground, "rgb(0,0,0)");
    const size_t visibleRows
        = static_cast<size_t>(std::max(0.0, std::floor((control.h - headerHeight) / rowHeight)));
    for (size_t r = 0; r < rows.size() && r < visibleRows; ++r) {
        const double top = control.y + headerHeight + static_cast<double>(r) * rowHeight;
        if ((r % 2) == 1) {
            canvas.fillRect((dataX + 1.0) * supersample, top * supersample,
                std::max(0.0, (control.x + control.w - dataX - 1.0) * supersample),
                rowHeight * supersample, parseCssColor("rgb(248,248,248)"));
        }
        canvas.strokePolyline(
            { { x, (top + rowHeight) * supersample }, { x + w, (top + rowHeight) * supersample } },
            1.0 * supersample, grid, {});
        rasterizeControlLabel(canvas, control, control.x + gutterWidth / 2.0,
            top + rowHeight / 2.0 + fontSize * 0.35, "center", std::to_string(r + 1), border);
        double cellLeft = dataX;
        for (size_t c = 0; c < rows[r].size() && c < columnCount; ++c) {
            rasterizeTableCellText(
                canvas, control, cellLeft, top, columnWidths[c], rowHeight, rows[r][c], textColor);
            cellLeft += columnWidths[c];
        }
    }
}
//=============================================================================
