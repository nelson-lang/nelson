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
class XMLWriter : public ITableWriter
{
private:
    std::wstring filename;
    std::vector<std::string> headers;
    std::vector<std::vector<std::string>> columns;
    std::string rootElement;
    std::string rowElement;
    std::string attributeSuffix;
    bool withWriteRowNames;
    //=============================================================================
    std::string
    escapeXML(const std::string& input);
    //=============================================================================
public:
    XMLWriter(const std::wstring& fname, const std::string& root = "table",
        const std::string& row = "row", bool withWriteRowNames = false,
        const std::string AttributeSuffix = "Attribute");
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
