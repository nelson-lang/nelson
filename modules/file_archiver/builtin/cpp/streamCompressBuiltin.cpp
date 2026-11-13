//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "streamCompressBuiltin.hpp"
#include "streamBuiltinHelpers.hpp"
#include "zipBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// bytes = __stream_compress__(codec, data, level, workers)
ArrayOfVector
Nelson::FileArchiverGateway::streamCompressBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 4);
    nargoutcheck(nLhs, 0, 1);
    const StreamCodec codec = streamCodec(argIn[0]);
    // uint8 data is read in place, text is converted to UTF-8 first.
    std::vector<uint8_t> text;
    const uint8_t* data = nullptr;
    size_t size = 0;
    if (argIn[1].dataClass() == NLS_UINT8) {
        data = static_cast<const uint8_t*>(argIn[1].dataPointer());
        size = argIn[1].elementCount();
    } else {
        text = byteVector(argIn[1]);
        data = text.data();
        size = text.size();
    }
    const int level = static_cast<int>(argIn[2].asDoubleScalar());
    const int workers = zstdWorkers(argIn[3], size);
    std::vector<uint8> result;
    try {
        auto encoder = makeEncoder(
            codec,
            [&](const uint8_t* chunk, size_t count) {
                result.insert(result.end(), chunk, chunk + count);
            },
            level, workers);
        encoder->write(data, size);
        encoder->finish();
    } catch (const ArchiveStreams::StreamError& error) {
        raiseStreamError(error, codec);
    }
    ArrayOfVector retval;
    retval << ArrayOf::uint8RowVector(result);
    return retval;
}
//=============================================================================
