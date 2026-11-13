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
#include <filesystem>
#include "ExtractionGuard.hpp"
#include "ArchivePath.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Small outputs are never refused for their ratio (a few zeros compress very well).
static constexpr uint64_t ratioGrace = 1024 * 1024;
//=============================================================================
static uint64_t
availableSpace(const std::wstring& destination);
//=============================================================================
ExtractionGuard::ExtractionGuard(
    const ExtractionLimits& extractionLimits, const std::wstring& destination)
    : limits(extractionLimits)
{
    if (!destination.empty()) {
        freeSpace = availableSpace(destination);
    }
}
//=============================================================================
void
ExtractionGuard::checkTotal(uint64_t total, const std::string& subject) const
{
    if (total > limits.maxTotalSize) {
        throw ExtractionLimitError(
            ExtractionLimitKind::TotalSize, subject, total, limits.maxTotalSize);
    }
    if (total > freeSpace) {
        throw ExtractionLimitError(ExtractionLimitKind::FreeSpace, subject, total, freeSpace);
    }
}
//=============================================================================
void
ExtractionGuard::checkRatio(
    uint64_t size, uint64_t compressedSize, const std::string& subject) const
{
    if (limits.maxRatio <= 0.0 || size <= ratioGrace) {
        return;
    }
    const double allowed
        = limits.maxRatio * static_cast<double>(std::max<uint64_t>(compressedSize, 1));
    if (static_cast<double>(size) > allowed) {
        throw ExtractionLimitError(ExtractionLimitKind::Ratio, subject, size, compressedSize);
    }
}
//=============================================================================
void
ExtractionGuard::setSource(uint64_t compressedSize, const std::string& subject)
{
    sourceSize = compressedSize;
    sourceName = subject;
    written = 0;
}
//=============================================================================
void
ExtractionGuard::add(uint64_t count)
{
    written += count;
    checkTotal(written, sourceName);
    checkRatio(written, sourceSize, sourceName);
}
//=============================================================================
void
raiseExtractionLimitError(const ExtractionLimitError& error)
{
    const std::wstring subject = utf8_to_wstring(error.subject());
    const std::wstring needed = std::to_wstring(error.needed());
    const std::wstring allowed = std::to_wstring(error.allowed());
    switch (error.kind()) {
    case ExtractionLimitKind::TotalSize:
        Error(L"Nelson:file_archiver:extractedSizeLimit",
            L"Extracting %s needs %s bytes, more than MaxExtractedSize (%s bytes).", subject,
            needed, allowed);
        break;
    case ExtractionLimitKind::FreeSpace:
        Error(L"Nelson:file_archiver:notEnoughSpace",
            L"Extracting %s needs %s bytes, only %s bytes are free on disk.", subject, needed,
            allowed);
        break;
    case ExtractionLimitKind::Ratio:
    default:
        Error(L"Nelson:file_archiver:compressionRatioLimit",
            L"%s exceeds MaxCompressionRatio: %s bytes from %s compressed bytes.", subject, needed,
            allowed);
        break;
    }
    throw error;
}
//=============================================================================
static uint64_t
availableSpace(const std::wstring& destination)
{
#ifdef __EMSCRIPTEN__
    // The in-memory file system reports no meaningful disk space.
    return UINT64_MAX;
#else
    namespace fs = std::filesystem;
    std::error_code ec;
    fs::path path = toNativePath(destination);
    // Free space of the nearest folder that already exists.
    while (!path.empty() && !fs::exists(path, ec)) {
        const fs::path parent = path.parent_path();
        if (parent == path) {
            break;
        }
        path = parent;
    }
    if (path.empty()) {
        path = fs::current_path(ec);
    }
    const fs::space_info info = fs::space(path, ec);
    if (ec || info.available == static_cast<std::uintmax_t>(-1)) {
        return UINT64_MAX;
    }
    return static_cast<uint64_t>(info.available);
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
