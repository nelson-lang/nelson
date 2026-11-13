//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "streamFopenBuiltin.hpp"
#include "ArchiveFileBackends.hpp"
#include "ioBuiltinHelpers.hpp"
#include "streamBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// fid = __stream_fopen__(codec, source, mode, encoding, level, maxSize, workers, seekable)
// source: file name, uint8 bytes (reading) or [] (writing in memory); 'a' adds a stream to a file.
ArrayOfVector
Nelson::FileArchiverGateway::streamFopenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 8, 8);
    nargoutcheck(nLhs, 0, 1);
    const StreamCodec codec = streamCodec(argIn[0]);
    const bool zstd = codec == StreamCodec::Zstd;
    const ArrayOf& source = argIn[1];
    const wchar_t mode = fileMode(argIn[2], true);
    const bool writing = mode != L'r';
    const std::wstring encoding = checkedEncoding(argIn[3]);
    const bool isFile = source.isRowVectorCharacterArray() || source.isScalarStringArray();
    std::wstring filename;
    std::vector<uint8_t> bytes;
    if (isFile) {
        filename = source.asWideString();
    } else if (mode == L'a') {
        if (zstd) {
            Error(L"Nelson:io:zstd:appendNeedsFile", L"Appending needs a file name.");
        }
        Error(L"Nelson:io:gzip:appendNeedsFile", L"Appending needs a file name.");
    } else if (writing ? !source.isEmpty() : source.dataClass() != NLS_UINT8) {
        if (zstd) {
            Error(L"Nelson:io:zstd:invalidSource",
                L"Source must be a file name, uint8 data to read, or [] to write in memory.");
        }
        Error(L"Nelson:io:gzip:invalidSource",
            L"Source must be a file name, uint8 data to read, or [] to write in memory.");
    } else if (!writing) {
        const auto* data = static_cast<const uint8_t*>(source.dataPointer());
        bytes.assign(data, data + source.elementCount());
    }
    // Seekable writers cut frames of 1 MiB.
    constexpr uint32_t seekableFrameSize = 1024 * 1024;
    const bool seekable = zstd && argIn[7].asLogicalScalar();
    if (seekable && mode == L'a') {
        Error(L"Nelson:io:zstd:seekableAppend", L"Seekable data cannot be appended.");
    }
    FileBackend* backend = nullptr;
    try {
        if (writing) {
            backend = new CompressedWriteBackend(codec, filename,
                static_cast<int>(argIn[4].asDoubleScalar()), zstdWorkers(argIn[6], 0), mode == L'a',
                seekable ? seekableFrameSize : 0);
        } else {
            backend
                = new CompressedReadBackend(codec, filename, std::move(bytes), sizeLimit(argIn[5]));
        }
    } catch (const ArchiveStreams::StreamError& error) {
        raiseStreamError(error, codec, filename);
    }
    ArrayOfVector retval;
    retval << openFileIdentifier(backend, isFile ? filename : L"<bytes>", mode, encoding);
    return retval;
}
//=============================================================================
