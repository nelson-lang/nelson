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
#include <cstdint>
#include <memory>
#include <string>
#include "ArrayOf.hpp"
#include "ArchiveStreams.hpp"
//=============================================================================
// Codec choice and error translation shared by the nelson.io.zstd and nelson.io.gzip builtins.
namespace Nelson::FileArchiverGateway {
//=============================================================================
enum class StreamCodec
{
    Zstd,
    Gzip
};
//=============================================================================
// 'zstd' or 'gzip'.
StreamCodec
streamCodec(const ArrayOf& name);
//=============================================================================
// Translates a StreamError into a Nelson error of the codec; path names the file for file errors.
[[noreturn]] void
raiseStreamError(
    const ArchiveStreams::StreamError& error, StreamCodec codec, const std::wstring& path = L"");
//=============================================================================
// Workers value: -1 ('auto') uses the computational threads for size >= 4 MiB, else 0.
int
zstdWorkers(const ArrayOf& value, uint64_t size);
//=============================================================================
std::unique_ptr<ArchiveStreams::StreamDecoder>
makeDecoder(StreamCodec codec, ArchiveStreams::Input input, uint64_t maxSize);
// workers is used by Zstandard only.
std::unique_ptr<ArchiveStreams::StreamEncoder>
makeEncoder(StreamCodec codec, ArchiveStreams::Output output, int level, int workers);
//=============================================================================
// A missing or empty file is accepted; otherwise it must start like data of the codec.
bool
startsLikeCodecFile(StreamCodec codec, const std::wstring& filename);
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
