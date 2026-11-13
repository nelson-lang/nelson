//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include <limits>
#include "streamDecompressFileBuiltin.hpp"
#include "streamBuiltinHelpers.hpp"
#include "ioBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "ArchivePath.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// __stream_decompressfile__(codec, source, destination, maxSize, maxCompressionRatio)
ArrayOfVector
Nelson::FileArchiverGateway::streamDecompressFileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 5, 5);
    nargoutcheck(nLhs, 0, 0);
    const StreamCodec codec = streamCodec(argIn[0]);
    const std::wstring source = argIn[1].asWideString();
    const std::wstring destination = argIn[2].asWideString();
    const uint64_t maxSize = sizeLimit(argIn[3]);
    // MaxSize is checked by the decoder; the guard adds the ratio and the free disk space.
    const ExtractionLimits limits = extractionLimits(
        ArrayOf::doubleScalar(std::numeric_limits<double>::infinity()), argIn[4]);
    ExtractionGuard guard(limits, destination);
    std::error_code ec;
    const auto sourceSize = std::filesystem::file_size(toNativePath(source), ec);
    guard.setSource(ec ? 0 : static_cast<uint64_t>(sourceSize), wstring_to_utf8(source));
    try {
        ArchiveStreams::transformFile(source, destination,
            [&](const ArchiveStreams::Input& reader, const ArchiveStreams::Output& writer) {
                auto decoder = makeDecoder(codec, reader, maxSize);
                ArchiveStreams::decodeAll(*decoder, [&](const uint8_t* data, size_t size) {
                    guard.add(size);
                    writer(data, size);
                });
            });
    } catch (const ArchiveStreams::StreamError& error) {
        raiseStreamError(error, codec,
            error.code() == ArchiveStreams::StreamErrorCode::ReadFailed ? source : destination);
    } catch (const ExtractionLimitError& error) {
        raiseExtractionLimitError(error);
    }
    return {};
}
//=============================================================================
