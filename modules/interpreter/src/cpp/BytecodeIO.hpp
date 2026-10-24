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
#include "BinaryStream.hpp"
#include "BytecodeChunk.hpp"
#include <limits>
#include <stdexcept>
//=============================================================================
namespace Nelson::BytecodeIO {
constexpr size_t maxArtifactBytes = 256 * 1024 * 1024;
constexpr uint32_t formatVersion = 4;
inline void
require(bool valid, const char* message = "Invalid or truncated bytecode artifact.")
{
    if (!valid) {
        throw std::runtime_error(message);
    }
}
//=============================================================================
class Reader : public BinaryReader
{
public:
    explicit Reader(const std::string& data) : BinaryReader(data) { }
    void
    charge(size_t bytes)
    {
        require(bytes <= budget, "Bytecode allocation limit exceeded.");
        budget -= bytes;
    }

private:
    size_t budget = maxArtifactBytes;
};
//=============================================================================
template <class T>
T
read(Reader& reader)
{
    T value {};
    require(reader.readPod(value));
    return value;
}
//=============================================================================
inline bool
readBool(Reader& reader)
{
    const auto value = read<uint8_t>(reader);
    require(value <= 1);
    return value != 0;
}
//=============================================================================
inline std::string
readString(Reader& reader)
{
    std::string value;
    require(reader.readString(value));
    reader.charge(value.size());
    return value;
}
//=============================================================================
template <class T>
std::vector<T>
readVector(Reader& reader, size_t limit = 65536)
{
    const auto count = read<uint64_t>(reader);
    require(count <= limit && count <= maxArtifactBytes / sizeof(T));
    reader.charge(static_cast<size_t>(count) * sizeof(T));
    std::vector<T> value(static_cast<size_t>(count));
    for (auto& item : value) {
        require(reader.readPod(item));
    }
    return value;
}
//=============================================================================
inline stringVector
readStrings(Reader& reader)
{
    const auto count = read<uint64_t>(reader);
    require(count <= 65536);
    reader.charge(static_cast<size_t>(count) * sizeof(std::string));
    stringVector value;
    value.reserve(static_cast<size_t>(count));
    for (uint64_t i = 0; i < count; ++i) {
        value.push_back(readString(reader));
    }
    return value;
}
//=============================================================================
void
writeConstants(BinaryWriter& writer, const ConstantPool& pool);
ConstantPool
readConstants(Reader& reader);
void
writeChunk(BinaryWriter& writer, const BytecodeChunk& chunk);
std::unique_ptr<BytecodeChunk>
readChunk(Reader& reader);
void
validateChunk(const BytecodeChunk& chunk);
void
validateInstruction(const BytecodeChunk& chunk, size_t pc, const Instruction& instruction);
std::string
engineFingerprint();
std::string
readArtifactPayload(const std::wstring& filename);
void
writeArtifactPayload(const std::wstring& filename, const std::string& payload);
//=============================================================================
} // namespace Nelson::BytecodeIO
//=============================================================================
