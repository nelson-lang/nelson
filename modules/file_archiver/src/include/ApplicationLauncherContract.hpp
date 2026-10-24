//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ApplicationArchiveFormat.hpp"
//=============================================================================
namespace Nelson::ApplicationLauncher {
constexpr size_t recordSize = 48;
constexpr std::array<uint8_t, 16> signature
    = { 'N', 'L', 'S', 'L', 'A', 'U', 'N', 'C', 'H', 'E', 'R', 'I', 'N', 'F', 'O', '1' };
enum class Kind : uint32_t
{
    Console = 0,
    Windowed = 1,
    Compiler = 2
};
//=============================================================================
struct Contract
{
    uint32_t format = 2;
    uint32_t startupInterface = 1;
    uint32_t archiveMinVersion = 1;
    uint32_t archiveMaxVersion = 3;
#if defined(__linux__) || defined(__APPLE__)
    uint32_t runtimeDiscoveryVersion = 3;
#else
    uint32_t runtimeDiscoveryVersion = 2;
#endif
    Kind kind = Kind::Console;
};
//=============================================================================
inline void
validate(const Contract& value)
{
    ApplicationArchive::require((value.format == 1 || value.format == 2)
            && value.startupInterface == 1 && value.archiveMinVersion == 1
            && (value.archiveMaxVersion == 2 || value.archiveMaxVersion == 3)
            && (value.runtimeDiscoveryVersion == 2 || value.runtimeDiscoveryVersion == 3)
            && static_cast<uint32_t>(value.kind) <= static_cast<uint32_t>(Kind::Compiler),
        "Unsupported application launcher contract; rebuild the native launchers.");
}
//=============================================================================
constexpr std::array<uint8_t, recordSize>
record(Kind kind)
{
    std::array<uint8_t, recordSize> bytes {};
    for (size_t i = 0; i < signature.size(); ++i) {
        bytes[i] = signature[i];
    }
    const Contract value;
    const uint32_t fields[] = { recordSize, value.format, value.startupInterface,
        value.archiveMinVersion, value.archiveMaxVersion, value.runtimeDiscoveryVersion,
        static_cast<uint32_t>(kind), 0 };
    for (size_t i = 0; i < 8; ++i) {
        for (size_t byte = 0; byte < 4; ++byte) {
            bytes[16 + i * 4 + byte] = static_cast<uint8_t>(fields[i] >> (8 * byte));
        }
    }
    return bytes;
}
//=============================================================================
inline Contract
decode(const uint8_t* bytes)
{
    using ApplicationArchive::readInteger;
    ApplicationArchive::require(std::equal(signature.begin(), signature.end(), bytes)
            && readInteger(bytes + 16, 4) == recordSize && readInteger(bytes + 44, 4) == 0,
        "Invalid application launcher contract.");
    Contract value { static_cast<uint32_t>(readInteger(bytes + 20, 4)),
        static_cast<uint32_t>(readInteger(bytes + 24, 4)),
        static_cast<uint32_t>(readInteger(bytes + 28, 4)),
        static_cast<uint32_t>(readInteger(bytes + 32, 4)),
        static_cast<uint32_t>(readInteger(bytes + 36, 4)),
        static_cast<Kind>(readInteger(bytes + 40, 4)) };
    validate(value);
    return value;
}
//=============================================================================
inline uint32_t
startupInterface(const std::array<uint8_t, recordSize>& bytes)
{
    // Read the embedded record itself so link-time optimization must retain it.
    const volatile uint8_t* data = bytes.data() + 24;
    uint32_t version = 0;
    for (size_t i = 0; i < 4; ++i) {
        version |= static_cast<uint32_t>(data[i]) << (8 * i);
    }
    return version;
}
//=============================================================================
} // namespace Nelson::ApplicationLauncher
//=============================================================================
