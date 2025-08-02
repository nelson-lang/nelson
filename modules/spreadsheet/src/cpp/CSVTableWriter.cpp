//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <fstream>
#include "CSVTableWriter.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
CSVWriter::CSVWriter(
    const std::wstring& fname, char delimiter, bool writeVariableNames, bool append)
    : filename(fname), delimiter(delimiter), writeVariableNames(writeVariableNames), append(append)
{
}
//=============================================================================
void
CSVWriter::addColumn(const std::string& header, const std::vector<std::string>& data)
{
    headers.push_back(header);
    columns.push_back(data);
}
//=============================================================================
bool
CSVWriter::writeToFile(std::wstring& errorMessage)
{
#ifdef _MSC_VER
    FILE* file = _wfopen(filename.c_str(), append ? L"a" : L"w");
#else
    FILE* file = fopen(wstring_to_utf8(filename).c_str(), append ? "a" : "w");
#endif
    if (!file) {
        errorMessage = _W("Could not open file.");
        return false;
    }

    if (writeVariableNames) {
        for (size_t i = 0; i < headers.size(); ++i) {
            fmt::fprintf(file, "%s", headers[i]);
            if (i < headers.size() - 1) {
                fmt::fprintf(file, "%c", delimiter);
            }
        }
        fmt::fprintf(file, "\n");
    }

    size_t rowCount = columns.empty() ? 0 : columns[0].size();

    for (size_t i = 0; i < rowCount * columns.size(); ++i) {
        size_t row = i / columns.size();
        size_t col = i % columns.size();
        fmt::fprintf(file, "%s", columns[col][row]);
        if (col < columns.size() - 1) {
            fmt::fprintf(file, "%c", delimiter);
        } else {
            fmt::fprintf(file, "\n");
        }
    }
    fclose(file);
    return true;
}
//=============================================================================
}
//=============================================================================
