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
#include <functional>
#include <stdexcept>
#include <string>
#include "nlsFile_archiver_exports.h"
//=============================================================================
// Pieces shared by the stream codecs (Zstandard, gzip) and the archive readers.
namespace Nelson::ArchiveStreams {
//=============================================================================
enum class StreamErrorCode
{
    NotAvailable,
    InvalidLevel,
    InvalidData,
    TooLarge,
    ReadFailed,
    WriteFailed
};
//=============================================================================
class NLSFILE_ARCHIVER_IMPEXP StreamError : public std::runtime_error
{
public:
    StreamError(StreamErrorCode code, const std::string& message)
        : std::runtime_error(message), errorCode(code)
    {
    }
    StreamErrorCode
    code() const
    {
        return errorCode;
    }

private:
    StreamErrorCode errorCode;
};
//=============================================================================
constexpr uint64_t noSizeLimit = UINT64_MAX;
//=============================================================================
// Reads up to size bytes into buffer, 0 at the end of the input.
using Input = std::function<size_t(uint8_t* buffer, size_t size)>;
using Output = std::function<void(const uint8_t* data, size_t size)>;
//=============================================================================
// Pulls decompressed bytes; reset() decodes again once the caller restarted its input.
class NLSFILE_ARCHIVER_IMPEXP StreamDecoder
{
public:
    virtual ~StreamDecoder();
    // Up to capacity bytes, 0 at the end of the data.
    virtual size_t
    read(uint8_t* buffer, size_t capacity)
        = 0;
    virtual void
    reset()
        = 0;
};
//=============================================================================
// Pushes data to compress; finish() ends the stream.
class NLSFILE_ARCHIVER_IMPEXP StreamEncoder
{
public:
    virtual ~StreamEncoder();
    virtual void
    write(const uint8_t* data, size_t size)
        = 0;
    virtual void
    finish()
        = 0;
};
//=============================================================================
// Memory to reserve for an entry announced as declared bytes: archives can lie about sizes,
// so the buffer grows with the data actually read beyond a small first reservation.
NLSFILE_ARCHIVER_IMPEXP size_t
initialReservation(uint64_t declared, uint64_t maxSize);
// Input reading size bytes of data (kept alive by the caller).
NLSFILE_ARCHIVER_IMPEXP Input
memoryInput(const uint8_t* data, size_t size);
// Feeds every block of input to encoder, then finishes it.
NLSFILE_ARCHIVER_IMPEXP void
encodeAll(const Input& input, StreamEncoder& encoder);
// Writes every block of decoder to output.
NLSFILE_ARCHIVER_IMPEXP void
decodeAll(StreamDecoder& decoder, const Output& output);
// Runs transform from source into a temporary file renamed to destination on success.
NLSFILE_ARCHIVER_IMPEXP void
transformFile(const std::wstring& source, const std::wstring& destination,
    const std::function<void(const Input&, const Output&)>& transform);
//=============================================================================
} // namespace Nelson::ArchiveStreams
//=============================================================================
