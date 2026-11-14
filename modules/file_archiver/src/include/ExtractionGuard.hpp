//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <cstdint>
#include <stdexcept>
#include <string>
#include "nlsFile_archiver_exports.h"
//=============================================================================
// Limits against decompression bombs, shared by unzip, untar and the stream codecs.
namespace Nelson {
//=============================================================================
struct ExtractionLimits
{
    uint64_t maxTotalSize = UINT64_MAX; // bytes written by one extraction
    double maxRatio = 0.0; // uncompressed / compressed size, 0: no limit
};
//=============================================================================
enum class ExtractionLimitKind
{
    TotalSize,
    FreeSpace,
    Ratio
};
//=============================================================================
class NLSFILE_ARCHIVER_IMPEXP ExtractionLimitError : public std::runtime_error
{
public:
    ExtractionLimitError(
        ExtractionLimitKind kind, const std::string& subject, uint64_t needed, uint64_t allowed)
        : std::runtime_error("Extraction limit exceeded.")
        , limitKind(kind)
        , subjectName(subject)
        , neededBytes(needed)
        , allowedBytes(allowed)
    {
    }
    ExtractionLimitKind
    kind() const
    {
        return limitKind;
    }
    // UTF-8 entry or file name.
    const std::string&
    subject() const
    {
        return subjectName;
    }
    uint64_t
    needed() const
    {
        return neededBytes;
    }
    uint64_t
    allowed() const
    {
        return allowedBytes;
    }

private:
    ExtractionLimitKind limitKind;
    std::string subjectName;
    uint64_t neededBytes;
    uint64_t allowedBytes;
};
//=============================================================================
// Checks sizes announced before writing, or counts bytes while writing (streams).
class NLSFILE_ARCHIVER_IMPEXP ExtractionGuard
{
public:
    // destination: file or folder receiving the data, empty for memory (no disk check).
    ExtractionGuard(const ExtractionLimits& limits, const std::wstring& destination);
    // Refuses a total that exceeds the size limit or the free disk space.
    void
    checkTotal(uint64_t total, const std::string& subject) const;
    // Refuses size bytes stored in compressedSize bytes beyond the ratio limit.
    void
    checkRatio(uint64_t size, uint64_t compressedSize, const std::string& subject) const;
    // Streams: compressed size of the source and name used in errors.
    void
    setSource(uint64_t compressedSize, const std::string& subject);
    // Streams: counts written bytes, throws once a limit is passed.
    void
    add(uint64_t count);

private:
    ExtractionLimits limits;
    uint64_t freeSpace = UINT64_MAX;
    uint64_t sourceSize = 0;
    std::string sourceName;
    uint64_t written = 0;
};
//=============================================================================
// Raises the Nelson error matching a limit.
[[noreturn]] NLSFILE_ARCHIVER_IMPEXP void
raiseExtractionLimitError(const ExtractionLimitError& error);
//=============================================================================
} // namespace Nelson
//=============================================================================
