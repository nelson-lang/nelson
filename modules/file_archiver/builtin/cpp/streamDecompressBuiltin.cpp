//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include "streamDecompressBuiltin.hpp"
#include "streamBuiltinHelpers.hpp"
#include "ioBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// data = __stream_decompress__(codec, bytes, maxSize, maxCompressionRatio)
ArrayOfVector
Nelson::FileArchiverGateway::streamDecompressBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 4);
    nargoutcheck(nLhs, 0, 1);
    const StreamCodec codec = streamCodec(argIn[0]);
    if (argIn[1].dataClass() != NLS_UINT8) {
        Error(L"Nelson:io:invalidData", L"Data must be uint8.");
    }
    const auto* data = static_cast<const uint8_t*>(argIn[1].dataPointer());
    const uint64_t maxSize = sizeLimit(argIn[2]);
    // No disk involved: only the ratio is checked here, MaxSize by the decoder.
    ExtractionGuard guard(
        extractionLimits(
            ArrayOf::doubleScalar(std::numeric_limits<double>::infinity()), argIn[3]),
        L"");
    guard.setSource(argIn[1].elementCount(), "data");
    std::vector<uint8> result;
    try {
        auto decoder = makeDecoder(
            codec, ArchiveStreams::memoryInput(data, argIn[1].elementCount()), maxSize);
        ArchiveStreams::decodeAll(*decoder, [&](const uint8_t* chunk, size_t count) {
            guard.add(count);
            result.insert(result.end(), chunk, chunk + count);
        });
    } catch (const ArchiveStreams::StreamError& error) {
        raiseStreamError(error, codec);
    } catch (const ExtractionLimitError& error) {
        raiseExtractionLimitError(error);
    }
    ArrayOfVector retval;
    retval << ArrayOf::uint8RowVector(result);
    return retval;
}
//=============================================================================
