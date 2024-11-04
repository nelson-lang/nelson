//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <regex>
#include <nlohmann/json.hpp>
#include "poToJson.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class POEntry
{
public:
    std::string msgid;
    std::string msgstr;
};
//=============================================================================
static std::string
unescapeString(const std::string& input)
{
    std::string result;
    for (size_t i = 0; i < input.length(); ++i) {
        if (input[i] == '\\') {
            if (i + 1 < input.length()) {
                switch (input[i + 1]) {
                case 'n':
                    result += '\n';
                    break;
                case 't':
                    result += '\t';
                    break;
                case 'r':
                    result += '\r';
                    break;
                case '"':
                    result += '"';
                    break;
                case '\\':
                    result += '\\';
                    break;
                default:
                    result += input[i + 1];
                }
                ++i;
            }
        } else {
            result += input[i];
        }
    }
    return result;
}
//=============================================================================
static std::string
extractQuotedString(const std::string& line)
{
    std::regex quoted("\"(.*)\"");
    std::smatch match;
    if (std::regex_search(line, match, quoted) && match.size() > 1) {
        return match[1].str();
    }
    return "";
}
//=============================================================================
static POEntry
parsePOEntry(std::vector<std::string>& lines, size_t& currentLine)
{
    POEntry entry;
    std::string currentField;

    while (currentLine < lines.size()) {
        const std::string& line = lines[currentLine];

        if (line.empty() || line[0] == '#') {
            currentLine++;
            continue;
        }

        if (line.find("msgid") == 0) {
            currentField = "msgid";
            entry.msgid = extractQuotedString(line.substr(5));
            currentLine++;

            // Handle multi-line msgid
            while (currentLine < lines.size() && lines[currentLine][0] == '"') {
                entry.msgid += extractQuotedString(lines[currentLine]);
                currentLine++;
            }
        } else if (line.find("msgstr") == 0) {
            currentField = "msgstr";
            entry.msgstr = extractQuotedString(line.substr(6));
            currentLine++;

            // Handle multi-line msgstr
            while (currentLine < lines.size() && lines[currentLine][0] == '"') {
                entry.msgstr += extractQuotedString(lines[currentLine]);
                currentLine++;
            }

            // Entry is complete
            break;
        } else {
            currentLine++;
        }
    }

    entry.msgid = unescapeString(entry.msgid);
    entry.msgstr = unescapeString(entry.msgstr);

    return entry;
}
//=============================================================================
bool
poToJson(
    const std::wstring& poFilename, const std::wstring& jsonFilename, std::wstring& errorMessage)
{
    // Read PO file
#ifdef _MSC_VER
    std::wifstream inFile(poFilename);
#else
    std::ifstream inFile(wstring_to_utf8(poFilename));
#endif
    if (!inFile) {
        errorMessage = _W("Cannot open input file: ") + poFilename;
        return false;
    }

    std::vector<std::string> lines;
    std::wstring wline;
    std::string line;
#ifdef _MSC_VER
    while (std::getline(inFile, wline)) {
#else
    while (std::getline(inFile, line)) {
#endif

#ifdef _MSC_VER
        lines.push_back(wstring_to_utf8(wline));
#else
        lines.push_back(line);
#endif
    }
    inFile.close();

    // Parse PO entries
    std::vector<POEntry> entries;
    size_t currentLine = 0;

    while (currentLine < lines.size()) {
        POEntry entry = parsePOEntry(lines, currentLine);
        if (!entry.msgid.empty() || !entry.msgstr.empty()) {
            entries.push_back(entry);
        }
    }

    // Create JSON
    nlohmann::ordered_json jsonOutput;
    for (const auto& entry : entries) {
        jsonOutput[entry.msgid] = entry.msgstr;
    }

// Write JSON file
#ifdef _MSC_VER
    std::wofstream outFile(jsonFilename);
#else
    std::ofstream outFile(wstring_to_utf8(jsonFilename));
#endif
    if (!outFile) {
        errorMessage = _W("Cannot open output file: ") + jsonFilename;
        return false;
    }
#ifdef _MSC_VER
    outFile << utf8_to_wstring(jsonOutput.dump(2));
    outFile << utf8_to_wstring("\n");
#else
    outFile << jsonOutput.dump(2);
    outFile << "\n";
#endif
    outFile.close();
    return true;
}
//=============================================================================
}
//=============================================================================
