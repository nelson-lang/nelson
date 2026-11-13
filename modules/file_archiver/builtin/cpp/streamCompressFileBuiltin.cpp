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
#include "streamCompressFileBuiltin.hpp"
#include "streamBuiltinHelpers.hpp"
#include "ArchivePath.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// __stream_compressfile__(codec, source, destination, level, workers)
ArrayOfVector
Nelson::FileArchiverGateway::streamCompressFileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 5, 5);
    nargoutcheck(nLhs, 0, 0);
    const StreamCodec codec = streamCodec(argIn[0]);
    const std::wstring source = argIn[1].asWideString();
    const std::wstring destination = argIn[2].asWideString();
    const int level = static_cast<int>(argIn[3].asDoubleScalar());
    std::error_code ec;
    const auto size = std::filesystem::file_size(toNativePath(source), ec);
    const int workers = zstdWorkers(argIn[4], ec ? 0 : static_cast<uint64_t>(size));
    try {
        ArchiveStreams::transformFile(source, destination,
            [&](const ArchiveStreams::Input& reader, const ArchiveStreams::Output& writer) {
                auto encoder = makeEncoder(codec, writer, level, workers);
                ArchiveStreams::encodeAll(reader, *encoder);
            });
    } catch (const ArchiveStreams::StreamError& error) {
        raiseStreamError(error, codec,
            error.code() == ArchiveStreams::StreamErrorCode::ReadFailed ? source : destination);
    }
    return {};
}
//=============================================================================
