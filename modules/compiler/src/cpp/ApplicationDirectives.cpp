//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ApplicationDependencies.hpp"
#include "ClassdefParserTokenizer.hpp"
#include <fstream>
#include <iterator>
#include <string_view>
//=============================================================================
namespace Nelson::Application {
//=============================================================================
namespace {
    std::string_view
    trim(std::string_view text)
    {
        const auto first = text.find_first_not_of(" \t\r\n");
        if (first == std::string_view::npos) {
            return {};
        }
        return text.substr(first, text.find_last_not_of(" \t\r\n") - first + 1);
    }
    //=============================================================================
    void
    appendDirective(std::vector<Reference>& result, std::string_view text, int line)
    {
        const auto end = text.find_first_of(" \t");
        const auto directive = text.substr(0, end);
        if (directive != "%#function" && directive != "%#exclude") {
            return;
        }
        require(end != std::string_view::npos, "A dependency directive requires a name.");
        text = trim(text.substr(end));
        require(!text.empty(), "A dependency directive requires a name.");
        while (!text.empty()) {
            std::string name;
            if (text.front() == '\'' || text.front() == '"') {
                const char quote = text.front();
                size_t cursor = 1;
                for (; cursor < text.size(); ++cursor) {
                    if (text[cursor] == quote) {
                        if (cursor + 1 == text.size() || text[cursor + 1] != quote) {
                            break;
                        }
                        ++cursor;
                    }
                    name += text[cursor];
                }
                require(cursor < text.size(), "Unterminated dependency directive name.");
                text.remove_prefix(cursor + 1);
                require(text.empty() || text.front() == ' ' || text.front() == '\t',
                    "Dependency directive names must be separated by whitespace.");
            } else {
                const auto space = text.find_first_of(" \t");
                name = std::string(text.substr(0, space));
                text = space == std::string_view::npos ? std::string_view() : text.substr(space);
            }
            require(!name.empty() && name.find('\0') == std::string::npos,
                "Invalid dependency directive name.");
            require(result.size() < 10000, "Dependency directive count limit exceeded.");
            result.push_back(
                { name, directive == "%#exclude" ? "pragma-exclude" : "pragma-function", line });
            text = trim(text);
        }
    }
    //=============================================================================
    std::vector<Reference>
    parseDirectives(const std::string& source)
    {
        using namespace classdef_detail;
        std::vector<ClassdefToken> tokens;
        std::string error;
        require(tokenizeClassdefSource(source, tokens, error),
            "Cannot tokenize application dependency directives.");
        // Keep only trivia using the existing tokenizer's exact source spans.
        // This also masks quoted pragma-like text and transpose expressions.
        auto trivia = source;
        for (const auto& token : tokens) {
            if (token.type == ClassdefTokenType::Newline
                || token.type == ClassdefTokenType::EndOfInput) {
                continue;
            }
            for (size_t k = token.offset; k < token.offset + token.text.size(); ++k) {
                if (trivia[k] != '\n' && trivia[k] != '\r') {
                    trivia[k] = ' ';
                }
            }
        }
        std::vector<Reference> result;
        size_t offset = 0;
        int line = 1;
        int blockDepth = 0;
        while (offset < source.size()) {
            const auto end = source.find('\n', offset);
            const auto length = (end == std::string::npos ? source.size() : end) - offset;
            const auto original = trim(std::string_view(source).substr(offset, length));
            const auto comment = trim(std::string_view(trivia).substr(offset, length));
            if (original == "%{" && comment == original) {
                ++blockDepth;
            } else if (blockDepth > 0) {
                if (original == "%}") {
                    --blockDepth;
                }
            } else if (comment.substr(0, 2) == "%#") {
                appendDirective(result, comment, line);
            }
            if (end == std::string::npos) {
                break;
            }
            offset = end + 1;
            ++line;
        }
        return result;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::vector<Reference>
readSourceDirectives(const std::filesystem::path& source)
{
    if (source.extension() != ".m") {
        return {};
    }
    std::ifstream input(source, std::ios::binary);
    require(input.good(), "Cannot read application directives.");
    const std::string text((std::istreambuf_iterator<char>(input)), {});
    require(!input.bad(), "Cannot read application directives.");
    if (text.find("%#function") == std::string::npos
        && text.find("%#exclude") == std::string::npos) {
        return {};
    }
    try {
        return parseDirectives(text);
    } catch (const std::exception& error) {
        throw std::runtime_error(source.generic_u8string() + ": " + error.what());
    }
}
//=============================================================================
} // namespace Nelson::Application
//=============================================================================
