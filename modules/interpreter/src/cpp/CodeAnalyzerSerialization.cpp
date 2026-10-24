//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzer.hpp"
#include "CodeAnalyzerRules.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include <filesystem>
#include <memory>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    std::string
    sarifArtifactUri(const std::wstring& filename)
    {
        namespace fs = std::filesystem;
        fs::path path(filename);
        if (path.is_absolute()) {
            std::error_code error;
            fs::path relative = fs::relative(path, fs::current_path(), error);
#ifdef _WIN32
            const fs::path::string_type parentPrefix = L"..";
#else
            const fs::path::string_type parentPrefix = "..";
#endif
            if (!error && !relative.empty() && relative.native().rfind(parentPrefix, 0) != 0) {
                path = relative;
            }
        }
        return path.generic_string();
    }
    //=============================================================================
    std::string
    sarifLevel(const std::string& severity)
    {
        if (severity == "error") {
            return "error";
        }
        if (severity == "warning") {
            return "warning";
        }
        return "note";
    }
    //=============================================================================
    nlohmann::json
    textEditToJsonObject(const CodeAnalyzerTextEdit& edit)
    {
        nlohmann::json item;
        item["range"]["startLine"] = edit.lineStart;
        item["range"]["startColumn"] = edit.columnStart;
        item["range"]["endLine"] = edit.lineEnd;
        item["range"]["endColumn"] = edit.columnEnd;
        item["replacementText"] = edit.replacementText;
        return item;
    }
    //=============================================================================
    nlohmann::json
    fixToJsonObject(const CodeAnalyzerFix& fix)
    {
        nlohmann::json item;
        item["title"] = fix.title;
        item["safe"] = fix.safe;
        item["edits"] = nlohmann::json::array();
        for (const auto& edit : fix.edits) {
            item["edits"].push_back(textEditToJsonObject(edit));
        }
        return item;
    }
    //=============================================================================
    nlohmann::json
    diagnosticRange(const CodeAnalyzerDiagnostic& diagnostic)
    {
        nlohmann::json range;
        range["startLine"] = diagnostic.lineStart;
        range["startColumn"] = diagnostic.columnStart;
        range["endLine"] = diagnostic.lineEnd;
        range["endColumn"] = diagnostic.columnEnd;
        return range;
    }
    //=============================================================================
    nlohmann::json
    diagnosticToJsonObject(const CodeAnalyzerDiagnostic& diagnostic)
    {
        nlohmann::json item;
        item["file"] = wstring_to_utf8(diagnostic.file);
        item["id"] = diagnostic.checkId;
        item["ruleName"] = diagnostic.ruleName;
        item["severity"] = diagnostic.severity;
        item["category"] = diagnostic.category;
        item["fixability"] = diagnostic.fixability;
        item["message"] = diagnostic.message;
        item["range"] = diagnosticRange(diagnostic);
        item["line"] = diagnostic.lineStart;
        item["column"] = diagnostic.columnStart;
        item["suppressed"] = diagnostic.suppressed;
        item["fingerprint"] = diagnostic.fingerprint;
        item["helpUri"] = diagnostic.helpUri;
        item["fixes"] = nlohmann::json::array();
        for (const auto& fix : diagnostic.fixes) {
            item["fixes"].push_back(fixToJsonObject(fix));
        }
        return item;
    }
    //=============================================================================
    nlohmann::json
    sarifFix(const CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerFix& fix)
    {
        nlohmann::json item;
        item["description"]["text"] = fix.title;
        item["artifactChanges"] = nlohmann::json::array();
        nlohmann::json change;
        change["artifactLocation"]["uri"] = sarifArtifactUri(diagnostic.file);
        change["replacements"] = nlohmann::json::array();
        for (const auto& edit : fix.edits) {
            nlohmann::json replacement;
            replacement["deletedRegion"]["startLine"] = edit.lineStart;
            replacement["deletedRegion"]["startColumn"] = edit.columnStart;
            replacement["deletedRegion"]["endLine"] = edit.lineEnd;
            replacement["deletedRegion"]["endColumn"] = edit.columnEnd;
            replacement["insertedContent"]["text"] = edit.replacementText;
            change["replacements"].push_back(replacement);
        }
        item["artifactChanges"].push_back(change);
        return item;
    }
    //=============================================================================
    ArrayOf
    cellFromDoubles(const std::vector<double>& values)
    {
        Dimensions dims(values.size(), 1);
        if (values.empty()) {
            return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
        }
        auto elements = std::make_unique<ArrayOf[]>(values.size());
        for (size_t k = 0; k < values.size(); ++k) {
            elements[k] = ArrayOf::doubleScalar(values[k]);
        }
        ArrayOf result(NLS_CELL_ARRAY, dims, elements.get());
        elements.release();
        return result;
    }
    //=============================================================================
    ArrayOf
    cellFromLogicals(const std::vector<bool>& values)
    {
        Dimensions dims(values.size(), 1);
        if (values.empty()) {
            return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
        }
        auto elements = std::make_unique<ArrayOf[]>(values.size());
        for (size_t k = 0; k < values.size(); ++k) {
            elements[k] = ArrayOf::logicalScalar(values[k]);
        }
        ArrayOf result(NLS_CELL_ARRAY, dims, elements.get());
        elements.release();
        return result;
    }
    //=============================================================================
    ArrayOf
    cellFromArrayOfVector(const ArrayOfVector& values)
    {
        Dimensions dims(values.size(), 1);
        if (values.empty()) {
            return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
        }
        auto elements = std::make_unique<ArrayOf[]>(values.size());
        for (size_t k = 0; k < values.size(); ++k) {
            elements[k] = values[k];
        }
        ArrayOf result(NLS_CELL_ARRAY, dims, elements.get());
        elements.release();
        return result;
    }
    //=============================================================================
    ArrayOf
    fixesToStruct(const CodeAnalyzerDiagnostic& diagnostic)
    {
        stringVector fields = { "Title", "Safe", "LineStart", "LineEnd", "ColumnStart", "ColumnEnd",
            "ReplacementText" };
        std::vector<CodeAnalyzerTextEdit> edits;
        stringVector titles;
        std::vector<bool> safe;
        for (const auto& fix : diagnostic.fixes) {
            for (const auto& edit : fix.edits) {
                titles.push_back(fix.title);
                safe.push_back(fix.safe);
                edits.push_back(edit);
            }
        }
        if (edits.empty()) {
            Dimensions dims(0, 1);
            return ArrayOf::emptyStruct(fields, dims);
        }
        std::vector<double> lineStart;
        std::vector<double> lineEnd;
        std::vector<double> columnStart;
        std::vector<double> columnEnd;
        stringVector replacementText;
        for (const auto& edit : edits) {
            lineStart.push_back(edit.lineStart);
            lineEnd.push_back(edit.lineEnd);
            columnStart.push_back(edit.columnStart);
            columnEnd.push_back(edit.columnEnd);
            replacementText.push_back(edit.replacementText);
        }
        ArrayOfVector values;
        values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(titles);
        values << cellFromLogicals(safe);
        values << cellFromDoubles(lineStart);
        values << cellFromDoubles(lineEnd);
        values << cellFromDoubles(columnStart);
        values << cellFromDoubles(columnEnd);
        values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(replacementText);
        return ArrayOf::structArray(fields, values);
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOf
codeAnalyzerDiagnosticsToStruct(const std::vector<CodeAnalyzerDiagnostic>& diagnostics)
{
    stringVector fields = { "Location", "Severity", "Fixability", "Description", "CheckID",
        "LineStart", "LineEnd", "ColumnStart", "ColumnEnd", "FullFilename", "Suppressed", "Rule",
        "Category", "Message", "File", "RuleName", "HelpURI", "Fingerprint", "Fixes" };
    if (diagnostics.empty()) {
        Dimensions dims(0, 1);
        return ArrayOf::emptyStruct(fields, dims);
    }
    stringVector location;
    stringVector severity;
    stringVector fixability;
    stringVector description;
    stringVector checkId;
    std::vector<double> lineStart;
    std::vector<double> lineEnd;
    std::vector<double> columnStart;
    std::vector<double> columnEnd;
    wstringVector fullFilename;
    std::vector<bool> suppressed;
    stringVector category;
    stringVector ruleName;
    stringVector helpUri;
    stringVector fingerprint;
    ArrayOfVector fixes;
    for (const auto& diagnostic : diagnostics) {
        std::string file = wstring_to_utf8(diagnostic.file);
        location.push_back(file + ":" + std::to_string(diagnostic.lineStart) + ":"
            + std::to_string(diagnostic.columnStart));
        severity.push_back(diagnostic.severity);
        fixability.push_back(diagnostic.fixability);
        description.push_back(diagnostic.message);
        checkId.push_back(diagnostic.checkId);
        lineStart.push_back(diagnostic.lineStart);
        lineEnd.push_back(diagnostic.lineEnd);
        columnStart.push_back(diagnostic.columnStart);
        columnEnd.push_back(diagnostic.columnEnd);
        fullFilename.push_back(diagnostic.file);
        suppressed.push_back(diagnostic.suppressed);
        category.push_back(diagnostic.category);
        ruleName.push_back(diagnostic.ruleName);
        helpUri.push_back(diagnostic.helpUri);
        fingerprint.push_back(diagnostic.fingerprint);
        fixes.push_back(fixesToStruct(diagnostic));
    }
    ArrayOfVector values;
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(location);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(severity);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(fixability);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(description);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(checkId);
    values << cellFromDoubles(lineStart);
    values << cellFromDoubles(lineEnd);
    values << cellFromDoubles(columnStart);
    values << cellFromDoubles(columnEnd);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(fullFilename);
    values << cellFromLogicals(suppressed);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(checkId);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(category);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(description);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(fullFilename);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(ruleName);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(helpUri);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(fingerprint);
    values << cellFromArrayOfVector(fixes);
    return ArrayOf::structArray(fields, values);
}
//=============================================================================
std::string
codeAnalyzerDiagnosticsToJson(const std::vector<CodeAnalyzerDiagnostic>& diagnostics)
{
    nlohmann::json result = nlohmann::json::array();
    for (const auto& diagnostic : diagnostics) {
        result.push_back(diagnosticToJsonObject(diagnostic));
    }
    return result.dump();
}
//=============================================================================
std::string
codeAnalyzerDiagnosticsToSarif(const std::vector<CodeAnalyzerDiagnostic>& diagnostics)
{
    nlohmann::json root;
    root["version"] = "2.1.0";
    nlohmann::json run;
    run["tool"]["driver"]["name"] = "nelson-lint";
    run["tool"]["driver"]["informationUri"]
        = "https://nelson-lang.github.io/nelson-gitbook/releases/en_US/latest/index.html";
    run["tool"]["driver"]["rules"] = nlohmann::json::array();
    for (const auto& rule : codeAnalyzerRules()) {
        nlohmann::json ruleObject;
        ruleObject["id"] = rule.id;
        ruleObject["name"] = rule.name;
        ruleObject["shortDescription"]["text"] = rule.description;
        ruleObject["helpUri"] = rule.helpUri;
        ruleObject["properties"]["category"] = rule.category;
        ruleObject["defaultConfiguration"]["level"] = sarifLevel(rule.defaultSeverity);
        run["tool"]["driver"]["rules"].push_back(ruleObject);
    }
    run["results"] = nlohmann::json::array();
    for (const auto& diagnostic : diagnostics) {
        nlohmann::json result;
        result["ruleId"] = diagnostic.checkId;
        result["level"] = sarifLevel(diagnostic.severity);
        result["message"]["text"] = diagnostic.message;
        result["locations"] = nlohmann::json::array();
        nlohmann::json location;
        location["physicalLocation"]["artifactLocation"]["uri"] = sarifArtifactUri(diagnostic.file);
        location["physicalLocation"]["region"]["startLine"] = diagnostic.lineStart;
        location["physicalLocation"]["region"]["startColumn"] = diagnostic.columnStart;
        if (diagnostic.lineEnd > 0) {
            location["physicalLocation"]["region"]["endLine"] = diagnostic.lineEnd;
        }
        if (diagnostic.columnEnd > 0) {
            location["physicalLocation"]["region"]["endColumn"] = diagnostic.columnEnd;
        }
        result["locations"].push_back(location);
        if (!diagnostic.fingerprint.empty()) {
            result["partialFingerprints"]["nelsonDiagnostic"] = diagnostic.fingerprint;
        }
        if (diagnostic.suppressed) {
            result["suppressions"]
                = nlohmann::json::array({ { { "kind", "inSource" }, { "status", "accepted" } } });
        }
        if (!diagnostic.fixes.empty()) {
            result["fixes"] = nlohmann::json::array();
            for (const auto& fix : diagnostic.fixes) {
                result["fixes"].push_back(sarifFix(diagnostic, fix));
            }
        }
        run["results"].push_back(result);
    }
    root["runs"] = nlohmann::json::array({ run });
    return root.dump();
}
//=============================================================================
std::string
codeAnalyzerDiagnosticsToText(const std::vector<CodeAnalyzerDiagnostic>& diagnostics)
{
    std::ostringstream out;
    for (const auto& d : diagnostics) {
        out << wstring_to_utf8(d.file) << ":" << d.lineStart << ":" << d.columnStart << ": "
            << d.severity << " " << d.checkId << " " << d.message;
        if (d.suppressed) {
            out << " [suppressed]";
        }
        out << "\n";
    }
    return out.str();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
