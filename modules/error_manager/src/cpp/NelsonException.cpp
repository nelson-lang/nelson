//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <utility>
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "NelsonException.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Percent-encode a path so it can travel inside an OSC 8 hyperlink URI without
// colliding with the ':' line-number separator or being mangled by QUrl on the
// Qt side. Unreserved characters and '/' are kept; every other byte of the UTF-8
// encoding becomes %XX (decoded with decodeURIComponent / QUrl::fromPercentEncoding).
static std::wstring
percentEncodePath(const std::wstring& path)
{
    static const wchar_t* hex = L"0123456789ABCDEF";
    std::string utf8 = wstring_to_utf8(path);
    std::wstring encoded;
    encoded.reserve(utf8.size());
    for (unsigned char c : utf8) {
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'
            || c == '_' || c == '.' || c == '~' || c == '/') {
            encoded.push_back((wchar_t)c);
        } else {
            encoded.push_back(L'%');
            encoded.push_back(hex[(c >> 4) & 0x0F]);
            encoded.push_back(hex[c & 0x0F]);
        }
    }
    return encoded;
}
//=============================================================================
// Wrap a visible label in an OSC 8 hyperlink targeting nelson-open:?line=N&file=P.
// The GUI and webview command windows turn this into a click that opens the
// editor at that file and line; plain terminals never receive it (see the
// withHyperlinks argument threaded down from Interface::supportsHyperlinks).
static std::wstring
makeOpenHyperlink(const std::wstring& label, const std::wstring& filename, int line)
{
    if (filename.empty() || line <= 0) {
        return label;
    }
    // Query form (nelson-open:?line=N&file=...) so QUrl on the Qt side keeps the
    // components intact instead of mangling a bare ':' path separator.
    std::wstring uri
        = L"nelson-open:?line=" + std::to_wstring(line) + L"&file=" + percentEncodePath(filename);
    // OSC 8 ; ; <uri> ST <label> OSC 8 ; ; ST   (ST is ESC followed by backslash).
    return L"\x1b]8;;" + uri + L"\x1b\\" + label + L"\x1b]8;;\x1b\\";
}
//=============================================================================
static void
appendScriptLocation(std::wstring& message, const PositionScript& trace, bool withHyperlinks)
{
    if (trace.getLine() == 0) {
        return;
    }
    FileSystemWrapper::Path path(trace.getFilename());
    std::wstring filename;
    if (trace.getFilename().size() > 50) {
        filename = path.filename().wstring();
    } else {
        filename = path.wstring();
    }
    std::wstring lineStr = fmt::format(L"{:5d}", trace.getLine());
    std::wstring location = formatI18nMessage(
        Nelson::resolveMessageTemplate(L"Nelson:error_manager:atLine", L"at line %s of '%s'"),
        lineStr, filename);
    if (withHyperlinks) {
        location = makeOpenHyperlink(location, trace.getFilename(), trace.getLine());
    }
    message = message + location + L"\n";
}
//=============================================================================
NelsonException::NelsonException() { }
//=============================================================================
NelsonException::NelsonException(const std::string& msg_in, std::vector<PositionScript> positions,
    const std::string& identifier_in)
    : backtrace(std::move(positions))
    , identifier_(utf8_to_wstring(identifier_in))
    , msg(utf8_to_wstring(msg_in))
{
}
//=============================================================================
NelsonException::NelsonException(
    std::wstring msg_in, std::vector<PositionScript> positions, std::wstring identifier_in)
    : backtrace(std::move(positions)), identifier_(std::move(identifier_in)), msg(std::move(msg_in))
{
}
//=============================================================================
NelsonException::NelsonException(
    const std::string& msg_in, const PositionScript& position, const std::string& identifier_in)
    : identifier_(utf8_to_wstring(identifier_in)), msg(utf8_to_wstring(msg_in))
{
    this->backtrace.push_back(position);
}
//=============================================================================
NelsonException::NelsonException(
    const std::wstring& msg_in, const PositionScript& position, const std::wstring& identifier_in)
    : identifier_(identifier_in), msg(msg_in)
{
    this->backtrace.push_back(position);
}
//=============================================================================
NelsonException::NelsonException(const std::string& msg_in, const std::string& identifier_in)
    : identifier_(utf8_to_wstring(identifier_in)), msg(utf8_to_wstring(msg_in))
{
}
//=============================================================================
NelsonException::NelsonException(const std::wstring& msg_in, const std::wstring& identifier_in)
    : identifier_(identifier_in), msg(msg_in)
{
}
//=============================================================================
NelsonException::~NelsonException() { }
//=============================================================================
NelsonException::NelsonException(const NelsonException& copy)
{
    this->msg = copy.msg;
    this->identifier_ = copy.identifier_;
    this->backtrace = copy.backtrace;
    this->cause_ = copy.cause_;
    this->correction = copy.correction;
}
//=============================================================================
NelsonException::NelsonException(NelsonException&& move) noexcept
    : backtrace(std::move(move.backtrace))
    , cause_(std::move(move.cause_))
    , correction(std::move(move.correction))
    , identifier_(std::move(move.identifier_))
    , msg(std::move(move.msg))
{
}
//=============================================================================
NelsonException&
NelsonException::operator=(const NelsonException& copy)
{
    if (this == &copy) {
        return *this;
    }
    this->msg = copy.msg;
    this->identifier_ = copy.identifier_;
    this->backtrace = copy.backtrace;
    this->cause_ = copy.cause_;
    this->correction = copy.correction;
    return *this;
}
//=============================================================================
NelsonException&
NelsonException::operator=(NelsonException&& move) noexcept
{
    if (this == &move) {
        return *this;
    }
    this->msg = std::move(move.msg);
    this->identifier_ = std::move(move.identifier_);
    this->backtrace = std::move(move.backtrace);
    this->cause_ = std::move(move.cause_);
    this->correction = std::move(move.correction);
    return *this;
}
//=============================================================================
void
NelsonException::printMe(Interface* io) const
{
    if (!msg.empty()) {
        io->errorMessage(formattedErrorMessage(io->supportsHyperlinks()));
    }
}
//=============================================================================
bool
NelsonException::matches(const std::wstring& tst_msg)
{
    return (msg == tst_msg);
}
//=============================================================================
bool
NelsonException::matches(const std::string& tst_msg)
{
    return (msg == utf8_to_wstring(tst_msg));
}
//=============================================================================
void
NelsonException::setMessage(const std::wstring& message_in)
{
    msg = message_in;
}
//=============================================================================
void
NelsonException::setMessage(const std::string& message_in)
{
    msg = utf8_to_wstring(message_in);
}
//=============================================================================
int
NelsonException::getLine() const
{
    if (backtrace.empty()) {
        return -1;
    }
    return backtrace[0].getLine();
}
//=============================================================================
std::wstring
NelsonException::getFunctionName() const
{
    if (backtrace.empty()) {
        return L"";
    }
    return backtrace[0].getFunctionName();
}
//=============================================================================
std::wstring
NelsonException::getFilename() const
{
    if (backtrace.empty()) {
        return L"";
    }
    return backtrace[0].getFilename();
}
//=============================================================================
std::wstring
NelsonException::formattedErrorMessage(bool withHyperlinks) const
{
    std::wstring message = msg;
    const std::vector<PositionScript>& traces = backtrace;
    size_t nbTraces = traces.size();

    std::wstring functionName;
    std::wstring lineAsString;
    std::wstring headerFilename;
    int headerLine = 0;

    int i = -1;
    if (nbTraces != 0) {
        if (traces[0].getFunctionName() != L"error") {
            if (nbTraces > 1) {
                i = 0;
            }
        } else {
            if (nbTraces > 1) {
                if (traces[1].getFunctionName() != L"run") {
                    i = 1;
                }
            }
        }
    }

    if (i != -1) {
        size_t idx = i + 1;
        if (idx < nbTraces) {
            if (traces[idx].getFunctionName() != L"run") {
                functionName = traces[i].getFunctionName();
                headerFilename = traces[i].getFilename();
                if (traces[i].getLine() != 0) {
                    lineAsString = std::to_wstring(traces[i].getLine());
                    headerLine = traces[i].getLine();
                }
            }
        }
    }

    if (functionName.empty()) {
        message = L"\n" + _W("Error:") + L"\n" + message + L"\n";
    } else {
        std::wstring label;
        if (!lineAsString.empty()) {
            label = functionName + L" (" + _W("line") + L" " + lineAsString + L")";
        } else {
            label = functionName;
        }
        if (withHyperlinks && headerLine > 0) {
            label = makeOpenHyperlink(label, headerFilename, headerLine);
        }
        message = L"\n" + _W("Error in") + L" " + label + L"\n" + message + L"\n";
    }
    if (nbTraces > 0) {
        message = message + L"\n";
    }

    for (size_t k = 0; k < nbTraces; k++) {
        if (traces[k].isDirectScript()) {
            appendScriptLocation(message, traces[k], withHyperlinks);
        } else if (traces[k].getFunctionName() == L"run") {
            if ((k >= 1) && traces[k - 1].getLine() != 0) {
                size_t pos = k - 1;
                if (traces[pos].isDirectScript()) {
                    continue;
                }
                appendScriptLocation(message, traces[pos], withHyperlinks);
            }
        }
    }
    return message;
}
//=============================================================================
std::wstring
NelsonException::formattedWarningMessage(bool withHyperlinks) const
{
    if (!withHyperlinks) {
        return msg;
    }
    std::wstring message = msg;
    for (const PositionScript& trace : backtrace) {
        if (trace.getLine() == 0 || trace.getFilename().empty()) {
            continue;
        }
        std::wstring location = message.empty() ? std::wstring() : L"\n";
        appendScriptLocation(location, trace, withHyperlinks);
        if (!location.empty() && location.back() == L'\n') {
            location.pop_back();
        }
        message = message + location;
        break;
    }
    return message;
}
//=============================================================================
bool
NelsonException::isEmpty() const
{
    return backtrace.empty() && cause_.empty() && identifier_.empty() && msg.empty()
        && !hasCorrection();
}
//=============================================================================
void
NelsonException::setIdentifier(const std::wstring& identifier_in)
{
    this->identifier_ = identifier_in;
}
//=============================================================================
void
NelsonException::setIdentifier(const std::string& identifier_in)
{
    this->identifier_ = utf8_to_wstring(identifier_in);
}
//=============================================================================
void
NelsonException::clear()
{
    backtrace.clear();
    cause_.clear();
    correction.reset();
    identifier_.clear();
    msg.clear();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
