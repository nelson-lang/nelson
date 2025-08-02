//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <algorithm>
#include "XmlTableWriter.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
XMLWriter::escapeXML(const std::string& input)
{
    std::string result;
    for (char c : input) {
        switch (c) {
        case '&':
            result += "&amp;";
            break;
        case '<':
            result += "&lt;";
            break;
        case '>':
            result += "&gt;";
            break;
        case '"':
            result += "&quot;";
            break;
        case '\'':
            result += "&apos;";
            break;
        default:
            result += c;
        }
    }
    return result;
}
//=============================================================================
XMLWriter::XMLWriter(const std::wstring& fname, const std::string& root, const std::string& row,
    bool withWriteRowNames, const std::string AttributeSuffix)
    : filename(fname)
    , rootElement(root)
    , rowElement(row)
    , withWriteRowNames(withWriteRowNames)
    , attributeSuffix(AttributeSuffix)
{
}
//=============================================================================
void
XMLWriter::addColumn(const std::string& header, const std::vector<std::string>& data)
{
    headers.push_back(header);
    columns.push_back(data);
}
//=============================================================================
static bool
endsWith(const std::string& fullString, const std::string& ending)
{
    if (ending.size() > fullString.size()) {
        return false;
    }
    return std::equal(ending.rbegin(), ending.rend(), fullString.rbegin());
}
//=============================================================================
static std::string
getPrefix(const std::string& fullString, const std::string& ending)
{
    if (endsWith(fullString, ending)) {
        return fullString.substr(0, fullString.size() - ending.size());
    }
    return "";
}
//=============================================================================
bool
XMLWriter::writeToFile(std::wstring& errorMessage)
{
    std::ios::openmode openmode = std::ios::trunc;
#ifdef _MSC_VER
    std::ofstream file(filename, openmode);
#else
    std::ofstream file(wstring_to_utf8(filename), openmode);
#endif
    if (!file.is_open()) {
        errorMessage = L"Could not open file.";
        return false;
    }

    file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    file << "     <" << rootElement << ">\n";

    size_t rowCount = columns.empty() ? 0 : columns[0].size();

    for (size_t row = 0; row < rowCount; ++row) {
        if (withWriteRowNames) {
            file << "         <" << rowElement << " Row=\"" << escapeXML(columns[0][row]) << "\"";

            // Add attributes
            for (size_t col = 0; col < columns.size(); ++col) {
                std::string header = headers[col];
                std::string prefix = getPrefix(header, attributeSuffix);
                if (!prefix.empty()) {
                    std::string value = columns[col][row];
                    std::replace(prefix.begin(), prefix.end(), ' ', '_');
                    file << " " << prefix << "=\"" << escapeXML(value) << "\"";
                }
            }
            file << ">\n";
            for (size_t col = 1; col < columns.size(); ++col) {
                std::string header = headers[col];
                if (getPrefix(header, attributeSuffix).empty()) {
                    std::string value = columns[col][row];
                    std::replace(header.begin(), header.end(), ' ', '_');
                    file << "             <" << header << ">" << escapeXML(value) << "</" << header
                         << ">\n";
                }
            }
        } else {
            file << "         <" << rowElement;
            // Add attributes
            for (size_t col = 0; col < columns.size(); ++col) {
                std::string header = headers[col];
                std::string prefix = getPrefix(header, attributeSuffix);
                if (!prefix.empty()) {
                    std::string value = columns[col][row];
                    std::replace(prefix.begin(), prefix.end(), ' ', '_');
                    file << " " << prefix << "=\"" << escapeXML(value) << "\"";
                }
            }
            file << ">\n";

            // Add regular elements
            for (size_t col = 0; col < columns.size(); ++col) {
                std::string header = headers[col];
                if (getPrefix(header, attributeSuffix).empty()) {
                    std::string value = columns[col][row];
                    std::replace(header.begin(), header.end(), ' ', '_');
                    file << "             <" << header << ">" << escapeXML(value) << "</" << header
                         << ">\n";
                }
            }
        }
        file << "         </" << rowElement << ">\n";
    }

    file << "     </" << rootElement << ">\n";
    file << "     ";

    file.close();
    return true;
}
//=============================================================================
}
//=============================================================================
