//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cwchar>
#include "WriteLines.hpp"
#include "characters_encoding.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
nativeLineEnding()
{
#ifdef _WIN32
    return L"\r\n";
#else
    return L"\n";
#endif
}
//=============================================================================
static bool
spanEndsWith(const TextSpan& span, const std::wstring& suffix)
{
    return span.length >= suffix.size()
        && std::wmemcmp(span.data + span.length - suffix.size(), suffix.data(), suffix.size()) == 0;
}
//=============================================================================
static bool
needsTrailingLineEnding(const std::vector<TextSpan>& lines, const std::wstring& lineEnding)
{
    // A final empty line already ends the text, except when it is the only line.
    const TextSpan& last = lines.back();
    if (last.length == 0) {
        return lines.size() == 1;
    }
    return !spanEndsWith(last, lineEnding);
}
//=============================================================================
void
WriteLines(const std::vector<TextSpan>& lines, const std::wstring& filename,
    const WriteLinesOptions& options)
{
    std::string lineEnding;
    appendUtf8(lineEnding, options.lineEnding.data(), options.lineEnding.size());
    bool always = options.trailingRule == WriteLinesTrailingRule::ALWAYS;
    bool automatic = options.trailingRule == WriteLinesTrailingRule::AUTO;

    std::string text;
    bool targetWasEmpty = true;
    if (options.append
        && (!lines.empty() || options.trailingRule != WriteLinesTrailingRule::NEVER)) {
        // Appended lines start on a new line of a non-empty file.
        std::string encodedEnding;
        if (!encodeUtf8Text(lineEnding, options.encoding, encodedEnding)) {
            encodedEnding.clear();
        }
        uint64_t fileSize = 0;
        bool endsWithLineEnding = fileEndsWith(filename, encodedEnding, fileSize);
        targetWasEmpty = fileSize == 0;
        if (!targetWasEmpty && !endsWithLineEnding) {
            text = lineEnding;
        }
    }
    if (lines.empty()) {
        if (always || (automatic && targetWasEmpty)) {
            text += lineEnding;
        }
    } else {
        size_t total = text.size() + lineEnding.size() * lines.size();
        for (const TextSpan& line : lines) {
            total += line.length;
        }
        text.reserve(total);
        for (size_t k = 0; k < lines.size(); ++k) {
            if (k > 0) {
                text += lineEnding;
            }
            appendUtf8(text, lines[k].data, lines[k].length);
        }
        if (always || (automatic && needsTrailingLineEnding(lines, options.lineEnding))) {
            text += lineEnding;
        }
    }
    switch (writeUtf8TextFile(filename, text, options.encoding, options.append)) {
    case TextWriteStatus::OK:
        break;
    case TextWriteStatus::ENCODING_FAILED:
        Error(L"Nelson:writelines:encodingFailed",
            L"The text cannot be converted to the encoding '%s'.",
            utf8_to_wstring(options.encoding));
        break;
    case TextWriteStatus::OPEN_FAILED:
        Error(L"Nelson:writelines:openFailed",
            L"File could not be opened for writing. Possible causes include invalid file name, "
            L"insufficient permissions, and insufficient disk space.");
        break;
    case TextWriteStatus::WRITE_FAILED:
        Error(L"Nelson:writelines:writeFailed", L"Unable to write all data to the file '%s'.",
            filename);
        break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
