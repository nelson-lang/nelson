//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerSuppressions.hpp"
#include "CodeAnalyzerAstUtilities.hpp"
#include <algorithm>
#include <cctype>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    std::string
    toUpperAscii(std::string value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return static_cast<char>(std::toupper(c)); });
        return value;
    }
    //=============================================================================
    stringVector
    splitSuppressionIds(const std::string& value)
    {
        stringVector ids;
        std::string current;
        for (char c : value) {
            if (c == ',' || std::isspace(static_cast<unsigned char>(c))) {
                std::string id = CodeAnalyzerAst::trim(current);
                if (!id.empty()) {
                    ids.push_back(id);
                }
                current.clear();
            } else {
                current.push_back(c);
            }
        }
        std::string id = CodeAnalyzerAst::trim(current);
        if (!id.empty()) {
            ids.push_back(id);
        }
        return ids;
    }
    //=============================================================================
    bool
    parseSuppressionDirective(const std::string& comment, CodeAnalyzerSuppression& suppression)
    {
        size_t marker = comment.find("%#ok");
        if (marker == std::string::npos) {
            return false;
        }
        size_t first = comment.find('<', marker);
        size_t last = comment.find('>', first == std::string::npos ? marker : first);
        if (first == std::string::npos || last == std::string::npos || last <= first + 1) {
            suppression.all = true;
            return true;
        }
        for (const std::string& id :
            splitSuppressionIds(comment.substr(first + 1, last - first - 1))) {
            suppression.ids.insert(toUpperAscii(id));
        }
        return true;
    }
    //=============================================================================
} // namespace
//=============================================================================
CodeAnalyzerSuppressions
collectCodeAnalyzerSuppressions(const stringVector& lines)
{
    CodeAnalyzerSuppressions suppressions;
    for (size_t k = 0; k < lines.size(); ++k) {
        size_t comment = CodeAnalyzerAst::codeAnalyzerCommentStart(lines[k]);
        if (comment == std::string::npos) {
            continue;
        }
        CodeAnalyzerSuppression suppression;
        if (parseSuppressionDirective(lines[k].substr(comment), suppression)) {
            suppressions[static_cast<int>(k + 1)] = std::move(suppression);
        }
    }
    return suppressions;
}
//=============================================================================
bool
isCodeAnalyzerDiagnosticSuppressed(
    const CodeAnalyzerSuppressions& suppressions, int line, const std::string& checkId)
{
    auto found = suppressions.find(line);
    if (found != suppressions.end()) {
        return found->second.all || found->second.ids.find(checkId) != found->second.ids.end();
    }
    found = suppressions.find(line - 1);
    return found != suppressions.end()
        && (found->second.all || found->second.ids.find(checkId) != found->second.ids.end());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
