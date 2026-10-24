//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "CodeAnalyzerSyntaxRules.hpp"
#include "LexerContext.hpp"
#include <algorithm>
#include <cstdio>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    CodeAnalyzerDiagnostic
    makeDiagnostic(const std::filesystem::path& path, const std::string& id,
        const std::string& severity, const std::string& category, const std::string& fixability,
        const std::string& message, int line, int column = 1)
    {
        CodeAnalyzerDiagnostic diagnostic;
        diagnostic.file = path.wstring();
        diagnostic.checkId = id;
        diagnostic.severity = severity;
        diagnostic.category = category;
        diagnostic.fixability = fixability;
        diagnostic.message = message;
        diagnostic.lineStart = line;
        diagnostic.lineEnd = line;
        diagnostic.columnStart = column;
        diagnostic.columnEnd = column;
        return diagnostic;
    }
    //=============================================================================
    void
    appendParseError(const std::filesystem::path& path, const ParserContext& context,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        appendDiagnostic(makeDiagnostic(path, "NLS0001", "error", "Syntax", "none",
            context.diagnostic.message.empty() ? "Invalid syntax." : context.diagnostic.message,
            std::max(context.diagnostic.line, 1), std::max(context.diagnostic.column, 1)));
    }
    //=============================================================================
    ParserContext
    parseSourceFile(const std::filesystem::path& path,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic,
        bool& emittedDiagnostic)
    {
        ParserContext context;
#ifdef _MSC_VER
        FILE* fp = _wfopen(path.wstring().c_str(), L"rb");
#else
        FILE* fp = fopen(path.string().c_str(), "rb");
#endif
        if (fp == nullptr) {
            context.parserState = ParseError;
            appendDiagnostic(makeDiagnostic(
                path, "NLS0001", "error", "Syntax", "none", "File cannot be read.", 1, 1));
            emittedDiagnostic = true;
            return context;
        }
        LexerContext lexerContext;
        context = parseFileResult(lexerContext, fp, path.generic_string());
        fclose(fp);
        return context;
    }
    //=============================================================================
    ParserContext
    parseSourceText(const std::string& content)
    {
        LexerContext lexerContext;
        return parseStringResult(lexerContext, content);
    }
    //=============================================================================
} // namespace
//=============================================================================
ParserContext
applyCodeAnalyzerSyntaxRules(const std::filesystem::path& path, const std::string& content,
    bool parseFromText, const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
{
    bool emittedDiagnostic = false;
    ParserContext context = parseFromText
        ? parseSourceText(content)
        : parseSourceFile(path, appendDiagnostic, emittedDiagnostic);
    if (context.state() == ParseError && !emittedDiagnostic) {
        appendParseError(path, context, appendDiagnostic);
    }
    return context;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
