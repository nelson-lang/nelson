//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include "ITableWriter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class CSVWriter : public ITableWriter
{
private:
    std::wstring filename;
    std::vector<std::string> headers;
    std::vector<std::vector<std::string>> columns;
    char delimiter;
    bool writeVariableNames;
    bool append;
    //=============================================================================
public:
    CSVWriter(const std::wstring& fname, char delimiter, bool writeVariableNames, bool append);
    //=============================================================================
    void
    addColumn(const std::string& header, const std::vector<std::string>& data);
    //=============================================================================
    bool
    writeToFile(std::wstring& errorMessage);
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
