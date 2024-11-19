//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <complex>
#include "WriteTable.hpp"
#include "CSVTableWriter.hpp"
#include "XmlTableWriter.hpp"
#include "ITableWriter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class TableWriter
{
private:
    //=============================================================================
    std::string
    doubleComplexToString(std::complex<double> value, bool ignoreImagIfZero);
    //=============================================================================
    std::string
    doubleToString(double value);
    //=============================================================================
    std::string
    duplicateChar(const std::string& str, char ch);
    //=============================================================================
    std::string
    formatStringValueQuoteString(const std::string& value);
    //=============================================================================
    std::string
    formatStringValue(const std::string& value, const writeTableOptions& options);
    //=============================================================================
    void
    handleCellColumn(ITableWriter& writer, const ArrayOf& columnData, const std::string& columnName,
        const writeTableOptions& options);
    //=============================================================================
    void
    handleStringColumn(ITableWriter& writer, const ArrayOf& columnData,
        const std::string& columnName, const writeTableOptions& options);
    //=============================================================================
    void
    handleDoubleComplexColumn(ITableWriter& writer, const ArrayOf& columnData,
        const std::string& columnName, const writeTableOptions& options);
    //=============================================================================
    void
    handleDoubleColumn(ITableWriter& writer, const ArrayOf& columnData,
        const std::string& columnName, const writeTableOptions& options);
    //=============================================================================
    void
    handleLogicalColumn(ITableWriter& writer, const ArrayOf& columnData,
        const std::string& columnName, const writeTableOptions& options);
    //=============================================================================
    void
    handleCharacterColumn(ITableWriter& writer, const ArrayOf& columnData,
        const std::string& columnName, const writeTableOptions& options);
    //=============================================================================
    void
    processCharacterMatrix(const ArrayOf& columnData, std::vector<std::string>& column,
        const writeTableOptions& options);
    //=============================================================================
    void
    writeRowNames(
        ITableWriter& writer, const wstringVector& rowNames, const writeTableOptions& options);
    //=============================================================================
public:
    //=============================================================================
    void
    writeTableToFile(const ArrayOf& table, const std::wstring& filename,
        const writeTableOptions& options, std::wstring& errorMessage);
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
