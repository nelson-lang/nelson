//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CSVRangeConverter.hpp"
#include <cctype>
#include <cmath>
#include <stdexcept>
//=============================================================================
namespace Nelson {
//=============================================================================
double
CSVRangeConverter::columnToNumber(const std::string& column, bool& failed)
{
    failed = true;
    double result = 0;

    for (char c : column) {
        if (!isalpha(c)) {
            failed = true;
            return result;
        }
        c = toupper(c);
        result = result * 26 + (c - 'A' + 1);
    }

    failed = false;
    return result;
}
//=============================================================================
double
CSVRangeConverter::extractRow(const std::string& cell, bool& failed)
{
    size_t i = 0;
    while (i < cell.length() && isalpha(cell[i])) {
        i++;
    }

    if (i == cell.length()) {
        failed = true;
        return std::nan("");
    }

    try {
        failed = false;
        return std::stoi(cell.substr(i));
    } catch (...) {
        failed = true;
        return std::nan("");
    }
}
//=============================================================================
std::string
CSVRangeConverter::extractColumn(const std::string& cell, bool& failed)
{
    std::string column;

    for (char c : cell) {
        if (isalpha(c)) {
            column += c;
        } else {
            break;
        }
    }

    failed = column.empty();
    return column;
}
//=============================================================================
std::vector<double>
CSVRangeConverter::convertRange(const std::string& range, bool& failed)
{
    try {
        // Split range into start and end cells
        size_t separatorPos = range.find("..");
        if (separatorPos == std::string::npos) {
            failed = true;
            return {};
        }

        std::string startCell = range.substr(0, separatorPos);
        std::string endCell = range.substr(separatorPos + 2);

        // Convert start cell
        double startRow = extractRow(startCell, failed);
        if (failed) {
            return {};
        }

        double startCol = columnToNumber(extractColumn(startCell, failed), failed);
        if (failed) {
            return {};
        }

        // Convert end cell
        double endRow = extractRow(endCell, failed);
        if (failed) {
            return {};
        }

        double endCol = columnToNumber(extractColumn(endCell, failed), failed);
        if (failed) {
            return {};
        }
        return { startRow - 1, startCol - 1, endRow - 1, endCol - 1 };

    } catch (const std::exception&) {
        failed = true;
        return {};
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
