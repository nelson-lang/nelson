//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cstring>
#include <cwchar>
#include <filesystem>
#include <memory>
#include <new>
#include <system_error>
#include <vector>
#include "ReadLines.hpp"
#include "MapFileRead.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct LineSpan
{
    size_t start;
    size_t length;
};
//=============================================================================
static std::vector<LineSpan>
splitSpans(const wchar_t* text, size_t begin, size_t n, const wstringVector& lineEndings);
static LineSpan
trimSpan(const wchar_t* text, LineSpan span, const std::wstring& whitespace, bool leading,
    bool trailing);
static void
applyRules(const wchar_t* text, std::vector<LineSpan>& spans, const ReadLinesOptions& options);
static ArrayOf
spansToStringArray(const wchar_t* text, const std::vector<LineSpan>& spans);
//=============================================================================
ArrayOf
ReadLines(const std::wstring& filename, const ReadLinesOptions& options)
{
    std::filesystem::path fileAsPath(filename);
    std::error_code ec;
    if (std::filesystem::is_directory(fileAsPath, ec)) {
        Error(L"Nelson:readlines:folderNotSupported",
            L"Expected a single file. Folders are not supported.");
    }
    if (!std::filesystem::is_regular_file(fileAsPath, ec)) {
        Error(L"Nelson:readlines:fileNotFound",
            L"Unable to find or open '%s'. Check the path and filename or file permissions.",
            filename);
    }
    std::wstring content;
    std::wstring errorMessage;
    // An empty line ending keeps the terminators of the file for the split.
    if (!MapFileRead(filename, L"", options.encoding, content, errorMessage)) {
        Error(L"Nelson:readlines:readFailed", errorMessage);
    }
    return SplitTextLines(content, options);
}
//=============================================================================
ArrayOf
SplitTextLines(const std::wstring& text, const ReadLinesOptions& options)
{
    const wchar_t* data = text.data();
    constexpr wchar_t BYTE_ORDER_MARK = 0xFEFF;
    size_t begin = (!text.empty() && data[0] == BYTE_ORDER_MARK) ? 1 : 0;
    std::vector<LineSpan> spans = splitSpans(data, begin, text.size(), options.lineEndings);
    applyRules(data, spans, options);
    return spansToStringArray(data, spans);
}
//=============================================================================
std::vector<LineSpan>
splitSpans(const wchar_t* text, size_t begin, size_t n, const wstringVector& lineEndings)
{
    std::vector<const std::wstring*> endings;
    for (const std::wstring& ending : lineEndings) {
        if (!ending.empty()) {
            endings.push_back(&ending);
        }
    }
    std::stable_sort(endings.begin(), endings.end(),
        [](const std::wstring* a, const std::wstring* b) { return a->size() > b->size(); });
    bool asciiFirst[128] = { false };
    std::wstring otherFirst;
    for (const std::wstring* ending : endings) {
        wchar_t c = (*ending)[0];
        if (static_cast<unsigned int>(c) < 128) {
            asciiFirst[c] = true;
        } else if (otherFirst.find(c) == std::wstring::npos) {
            otherFirst.push_back(c);
        }
    }
    std::vector<LineSpan> spans;
    size_t start = begin;
    size_t i = begin;
    while (i < n) {
        wchar_t c = text[i];
        bool candidate = (static_cast<unsigned int>(c) < 128)
            ? asciiFirst[c]
            : (otherFirst.find(c) != std::wstring::npos);
        size_t matched = 0;
        if (candidate) {
            for (const std::wstring* ending : endings) {
                size_t len = ending->size();
                if (len <= n - i && std::wmemcmp(text + i, ending->data(), len) == 0) {
                    matched = len;
                    break;
                }
            }
        }
        if (matched == 0) {
            ++i;
            continue;
        }
        spans.push_back({ start, i - start });
        i += matched;
        start = i;
    }
    spans.push_back({ start, n - start });
    return spans;
}
//=============================================================================
LineSpan
trimSpan(
    const wchar_t* text, LineSpan span, const std::wstring& whitespace, bool leading, bool trailing)
{
    if (leading) {
        while (span.length > 0 && whitespace.find(text[span.start]) != std::wstring::npos) {
            ++span.start;
            --span.length;
        }
    }
    if (trailing) {
        while (span.length > 0
            && whitespace.find(text[span.start + span.length - 1]) != std::wstring::npos) {
            --span.length;
        }
    }
    return span;
}
//=============================================================================
void
applyRules(const wchar_t* text, std::vector<LineSpan>& spans, const ReadLinesOptions& options)
{
    bool checkBlank = options.emptyLineRule != ReadLinesEmptyLineRule::READ;
    bool leading = options.whitespaceRule == ReadLinesWhitespaceRule::TRIM
        || options.whitespaceRule == ReadLinesWhitespaceRule::TRIM_LEADING;
    bool trailing = options.whitespaceRule == ReadLinesWhitespaceRule::TRIM
        || options.whitespaceRule == ReadLinesWhitespaceRule::TRIM_TRAILING;
    if (!checkBlank && !leading && !trailing) {
        return;
    }
    // The empty text after a final line terminator is not a line to skip or reject.
    if (checkBlank && spans.back().length == 0) {
        spans.pop_back();
    }
    size_t kept = 0;
    for (size_t k = 0; k < spans.size(); ++k) {
        if (checkBlank && trimSpan(text, spans[k], options.whitespace, true, true).length == 0) {
            if (options.emptyLineRule == ReadLinesEmptyLineRule::SKIP) {
                continue;
            }
            Error(L"Nelson:readlines:emptyLine", L"Unexpected empty line detected on row %s.",
                std::to_wstring(k + 1));
        }
        spans[kept++] = trimSpan(text, spans[k], options.whitespace, leading, trailing);
    }
    spans.resize(kept);
}
//=============================================================================
ArrayOf
spansToStringArray(const wchar_t* text, const std::vector<LineSpan>& spans)
{
    Dimensions dims(spans.size(), 1);
    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements.reset(new ArrayOf[spans.size()]);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    // One small allocation pair per line: parallel on large files.
    // No exception may leave the parallel region: failures are flagged.
    const auto nbLines = static_cast<ompIndexType>(spans.size());
    bool allocationFailed = false;
    OMP_PARALLEL_FOR_LOOP(nbLines)
    for (ompIndexType k = 0; k < nbLines; ++k) {
        size_t len = spans[k].length;
        try {
            auto* chars = static_cast<charType*>(
                ArrayOf::allocateArrayOf(NLS_CHAR, len, stringVector(), false));
            if (len > 0) {
                memcpy(chars, text + spans[k].start, len * sizeof(charType));
            }
            elements[k] = ArrayOf(NLS_CHAR, Dimensions(len > 0 ? 1 : 0, len), chars);
        } catch (...) {
            allocationFailed = true;
        }
    }
    if (allocationFailed) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    ArrayOf res(NLS_STRING_ARRAY, dims, elements.get());
    elements.release();
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
