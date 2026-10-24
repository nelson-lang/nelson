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
#include <cstring>
#include <string>
#include <type_traits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
// Minimal binary writer/reader that serializes Nelson values into a byte buffer.
//
// Contract:
//  - every count/length is written as a fixed-width uint64 so that 32-bit and
//    64-bit builds agree on the layout;
//  - scalars are written in host byte order: this format is only exchanged
//    between processes of the same build on the same machine (see the ipc
//    module), never persisted;
//  - the reader bounds-checks every read and returns false on underrun instead
//    of throwing, so failures never cross a module boundary as exceptions.
//=============================================================================
class BinaryWriter
{
public:
    explicit BinaryWriter(std::string& out) : buffer(out) { }
    //=============================================================================
    template <class T>
    void
    writePod(const T& value)
    {
        static_assert(std::is_trivially_copyable<T>::value, "writePod requires a POD type");
        buffer.append(reinterpret_cast<const char*>(&value), sizeof(T));
    }
    //=============================================================================
    void
    writeU8(uint8_t value)
    {
        buffer.push_back(static_cast<char>(value));
    }
    //=============================================================================
    void
    writeU64(uint64_t value)
    {
        writePod(value);
    }
    //=============================================================================
    void
    writeBytes(const void* data, size_t count)
    {
        if (count != 0) {
            buffer.append(reinterpret_cast<const char*>(data), count);
        }
    }
    //=============================================================================
    void
    writeString(const std::string& value)
    {
        writeU64(static_cast<uint64_t>(value.size()));
        writeBytes(value.data(), value.size());
    }
    //=============================================================================
    template <class T>
    void
    writeVector(const std::vector<T>& values)
    {
        static_assert(std::is_trivially_copyable<T>::value, "writeVector requires a POD element");
        writeU64(static_cast<uint64_t>(values.size()));
        if (!values.empty()) {
            writeBytes(values.data(), values.size() * sizeof(T));
        }
    }
    //=============================================================================
    void
    writeStringVector(const std::vector<std::string>& values)
    {
        writeU64(static_cast<uint64_t>(values.size()));
        for (const std::string& value : values) {
            writeString(value);
        }
    }
    //=============================================================================
private:
    std::string& buffer;
};
//=============================================================================
class BinaryReader
{
public:
    BinaryReader(const char* data, size_t size) : ptr(data), remaining(size) { }
    //=============================================================================
    explicit BinaryReader(const std::string& data) : ptr(data.data()), remaining(data.size()) { }
    //=============================================================================
    template <class T>
    bool
    readPod(T& value)
    {
        static_assert(std::is_trivially_copyable<T>::value, "readPod requires a POD type");
        if (remaining < sizeof(T)) {
            return false;
        }
        std::memcpy(&value, ptr, sizeof(T));
        ptr += sizeof(T);
        remaining -= sizeof(T);
        return true;
    }
    //=============================================================================
    bool
    readU8(uint8_t& value)
    {
        return readPod(value);
    }
    //=============================================================================
    bool
    readU64(uint64_t& value)
    {
        return readPod(value);
    }
    //=============================================================================
    bool
    readString(std::string& value)
    {
        uint64_t count = 0;
        if (!readU64(count)) {
            return false;
        }
        if (count > remaining) {
            return false;
        }
        value.assign(ptr, static_cast<size_t>(count));
        ptr += count;
        remaining -= static_cast<size_t>(count);
        return true;
    }
    //=============================================================================
    template <class T>
    bool
    readVector(std::vector<T>& values)
    {
        static_assert(std::is_trivially_copyable<T>::value, "readVector requires a POD element");
        uint64_t count = 0;
        if (!readU64(count)) {
            return false;
        }
        // guard against overflow and truncated buffers (sizeof(T) >= 1)
        if (count > remaining / sizeof(T)) {
            return false;
        }
        values.resize(static_cast<size_t>(count));
        if (count != 0) {
            const size_t bytes = static_cast<size_t>(count) * sizeof(T);
            std::memcpy(values.data(), ptr, bytes);
            ptr += bytes;
            remaining -= bytes;
        }
        return true;
    }
    //=============================================================================
    bool
    readStringVector(std::vector<std::string>& values)
    {
        uint64_t count = 0;
        if (!readU64(count)) {
            return false;
        }
        // each entry costs at least its 8-byte length prefix
        if (count > remaining / sizeof(uint64_t)) {
            return false;
        }
        values.clear();
        values.reserve(static_cast<size_t>(count));
        for (uint64_t k = 0; k < count; ++k) {
            std::string value;
            if (!readString(value)) {
                return false;
            }
            values.push_back(std::move(value));
        }
        return true;
    }
    //=============================================================================
    bool
    atEnd() const
    {
        return remaining == 0;
    }
    //=============================================================================
private:
    const char* ptr;
    size_t remaining;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
