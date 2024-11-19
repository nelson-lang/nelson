//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
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
    std::ios::openmode openmode = append ? std::ios::app : std::ios::trunc;
#ifdef _MSC_VER
    std::ofstream file(filename, openmode);
#else
    std::ofstream file(wstring_to_utf8(filename), openmode);
#endif
    if (!file.is_open()) {
        errorMessage = _W("Could not open file.");
        return false;
    }

    if (writeVariableNames) {
        // Write headers
        for (size_t i = 0; i < headers.size(); ++i) {
            file << headers[i];
            if (i < headers.size() - 1) {
                file << delimiter;
            }
        }
        file << "\n";
    }

    size_t rowCount = columns.empty() ? 0 : columns[0].size();
    for (size_t row = 0; row < rowCount; ++row) {
        for (size_t col = 0; col < columns.size(); ++col) {
            file << columns[col][row];
            if (col < columns.size() - 1) {
                file << delimiter;
            }
        }
        file << "\n";
    }
    file.close();
    return true;
}
//=============================================================================
}
//=============================================================================
