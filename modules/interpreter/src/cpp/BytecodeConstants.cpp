//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BytecodeIO.hpp"
#include <algorithm>
#include <cstring>
//=============================================================================
namespace Nelson::BytecodeIO {
//=============================================================================
namespace {
    size_t
    elementSize(NelsonType type)
    {
        switch (type) {
        case NLS_DOUBLE:
        case NLS_MISSING_ARRAY:
            return sizeof(double);
        case NLS_SINGLE:
            return sizeof(single);
        case NLS_DCOMPLEX:
            return 2 * sizeof(double);
        case NLS_SCOMPLEX:
            return 2 * sizeof(single);
        case NLS_INT8:
        case NLS_UINT8:
        case NLS_LOGICAL:
            return 1;
        case NLS_INT16:
        case NLS_UINT16:
            return 2;
        case NLS_INT32:
        case NLS_UINT32:
            return 4;
        case NLS_INT64:
        case NLS_UINT64:
            return 8;
        case NLS_CHAR:
            return sizeof(charType);
        default:
            throw std::runtime_error("Unsupported bytecode constant type.");
        }
    }
    //=============================================================================
    bool
    isContainer(NelsonType type)
    {
        return type == NLS_CELL_ARRAY || type == NLS_STRING_ARRAY || type == NLS_STRUCT_ARRAY;
    }
    //=============================================================================
    std::vector<uint64_t>
    dimensionsOf(const ArrayOf& array)
    {
        std::vector<uint64_t> dimensions;
        for (indexType i = 0; i < array.nDims(); ++i) {
            dimensions.push_back(static_cast<uint64_t>(array.dimensionLength(static_cast<int>(i))));
        }
        return dimensions;
    }
    //=============================================================================
    void
    writeArray(BinaryWriter& writer, const ArrayOf& array, unsigned depth)
    {
        require(depth < 64, "Bytecode constant nesting limit exceeded.");
        require(!array.isSparse(), "Sparse runtime values cannot be bytecode constants.");
        const NelsonType type = array.dataClass();
        writer.writePod<uint32_t>(static_cast<uint32_t>(type));
        writer.writeVector(dimensionsOf(array));
        const size_t count = static_cast<size_t>(array.elementCount());
        if (isContainer(type)) {
            const auto fields = array.fieldNames();
            writer.writeStringVector(fields);
            const size_t multiplier = type == NLS_STRUCT_ARRAY ? fields.size() : 1;
            require(multiplier == 0 || count <= maxArtifactBytes / sizeof(ArrayOf) / multiplier);
            const auto* values = static_cast<const ArrayOf*>(array.dataPointer());
            for (size_t i = 0; i < count * multiplier; ++i) {
                writeArray(writer, values[i], depth + 1);
            }
        } else {
            const size_t width = elementSize(type);
            require(count <= maxArtifactBytes / width);
            std::string bytes;
            if (count != 0) {
                bytes.assign(static_cast<const char*>(array.dataPointer()), count * width);
            }
            writer.writeString(bytes);
        }
    }
    //=============================================================================
    ArrayOf
    readArray(Reader& reader, unsigned depth)
    {
        require(depth < 64, "Bytecode constant nesting limit exceeded.");
        const auto tag = read<uint32_t>(reader);
        require(tag < NLS_FUNCTION_HANDLE);
        const auto type = static_cast<NelsonType>(tag);
        const auto shape = readVector<uint64_t>(reader, 32);
        require(shape.size() >= 2 && shape.size() <= 32);
        size_t count = 1;
        std::vector<indexType> dimensions;
        for (uint64_t dimension : shape) {
            require(dimension <= maxArtifactBytes);
            require(count == 0 || dimension <= maxArtifactBytes / count);
            count *= static_cast<size_t>(dimension);
            dimensions.push_back(static_cast<indexType>(dimension));
        }
        const Dimensions dims(dimensions);
        if (isContainer(type)) {
            const auto fields = readStrings(reader);
            require(type == NLS_STRUCT_ARRAY || fields.empty());
            const size_t multiplier = type == NLS_STRUCT_ARRAY ? fields.size() : 1;
            require(multiplier == 0 || count <= maxArtifactBytes / sizeof(ArrayOf) / multiplier);
            reader.charge(count * multiplier * sizeof(ArrayOf));
            ArrayOf result(
                type, dims, ArrayOf::allocateArrayOf(type, count, fields, true), false, fields);
            auto* values = static_cast<ArrayOf*>(result.readWriteDataPointer());
            for (size_t i = 0; i < count * multiplier; ++i) {
                values[i] = readArray(reader, depth + 1);
                if (type == NLS_STRING_ARRAY) {
                    require(values[i].dataClass() == NLS_CHAR
                        || values[i].dataClass() == NLS_MISSING_ARRAY);
                }
            }
            return result;
        }
        const size_t width = elementSize(type);
        const auto bytes = readString(reader);
        require(count <= maxArtifactBytes / width && bytes.size() == count * width);
        if (type == NLS_LOGICAL) {
            require(
                std::all_of(bytes.begin(), bytes.end(), [](char v) { return v == 0 || v == 1; }));
        }
        reader.charge(count * width);
        ArrayOf result(type, dims, ArrayOf::allocateArrayOf(type, count));
        if (!bytes.empty()) {
            std::memcpy(result.readWriteDataPointer(), bytes.data(), bytes.size());
        }
        return result;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
writeConstants(BinaryWriter& writer, const ConstantPool& pool)
{
    writer.writeU64(pool.size());
    for (size_t i = 0; i < pool.size(); ++i) {
        const auto& constant = pool.get(static_cast<uint16_t>(i));
        if (const auto* array = std::get_if<ArrayOf>(&constant)) {
            writer.writeU8(0);
            writeArray(writer, *array, 0);
        } else if (const auto* name = std::get_if<std::string>(&constant)) {
            writer.writeU8(1);
            writer.writeString(*name);
        } else {
            throw std::runtime_error("Process pointers cannot be saved as bytecode constants.");
        }
    }
}
//=============================================================================
ConstantPool
readConstants(Reader& reader)
{
    ConstantPool pool;
    const auto count = read<uint64_t>(reader);
    require(count <= 65536);
    reader.charge(static_cast<size_t>(count) * sizeof(ConstantValue));
    for (uint64_t i = 0; i < count; ++i) {
        const auto type = read<uint8_t>(reader);
        require(type <= 1);
        const auto index
            = type == 0 ? pool.addArray(readArray(reader, 0)) : pool.addName(readString(reader));
        require(index == i, "Duplicate or inconsistent bytecode constant index.");
    }
    return pool;
}
//=============================================================================
} // namespace Nelson::BytecodeIO
//=============================================================================
