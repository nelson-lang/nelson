//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <array>
#include <ctime>
#include <string>
#include "ZipCentralDirectory.hpp"
#include "ArchivePath.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    constexpr uint32_t centralSignature = 0x02014b50;
    constexpr uint32_t endSignature = 0x06054b50;
    constexpr uint32_t zip64EndSignature = 0x06064b50;
    constexpr uint32_t zip64LocatorSignature = 0x07064b50;
    constexpr size_t endRecordSize = 22;
    constexpr size_t zip64LocatorSize = 20;
    constexpr size_t zip64EndSize = 56;
    constexpr size_t centralRecordSize = 46;
    constexpr size_t localHeaderSize = 30;
    constexpr uint16_t utf8Flag = 0x0800;
    constexpr uint16_t encryptedFlag = 0x0001;
    constexpr uint16_t aesMethod = 99;
    // Code points of CP437 bytes 0x80-0xFF, the ZIP default name encoding.
    constexpr std::array<char32_t, 128> cp437High = { 0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4,
        0x00E0, 0x00E5, 0x00E7, 0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
        0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9, 0x00FF, 0x00D6, 0x00DC,
        0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192, 0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1,
        0x00AA, 0x00BA, 0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB, 0x2591,
        0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556, 0x2555, 0x2563, 0x2551, 0x2557,
        0x255D, 0x255C, 0x255B, 0x2510, 0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E,
        0x255F, 0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567, 0x2568, 0x2564,
        0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B, 0x256A, 0x2518, 0x250C, 0x2588, 0x2584,
        0x258C, 0x2590, 0x2580, 0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
        0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229, 0x2261, 0x00B1, 0x2265,
        0x2264, 0x2320, 0x2321, 0x00F7, 0x2248, 0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2,
        0x25A0, 0x00A0 };
    // Where the central directory is and how far the archive is shifted in the file.
    struct DirectoryLocation
    {
        uint64_t start = 0;
        uint64_t size = 0;
        uint64_t count = 0;
        uint64_t shift = 0;
    };
} // namespace
//=============================================================================
[[noreturn]] static void
notZip(const std::string& message = "Not a valid ZIP archive.");
static uint64_t
le(const uint8_t* bytes, size_t length);
static void
readExactly(const ZipReadAt& readAt, uint64_t offset, uint8_t* buffer, size_t size);
static DirectoryLocation
locateDirectory(const ZipReadAt& readAt, uint64_t archiveSize);
static void
readExtraFields(const uint8_t* extra, size_t length, uint32_t compressed32, uint32_t size32,
    uint32_t offset32, ZipDirectoryEntry& entry, bool& hasNtfsTime, bool& hasUnixTime);
static time_t
dosTime(uint16_t time, uint16_t date);
static bool
isValidUtf8(const std::string& text);
static std::string
nameToUtf8(const std::string& raw, uint16_t flags);
static void
rejectOverlappingEntries(const std::vector<ZipDirectoryEntry>& entries);
//=============================================================================
std::vector<ZipDirectoryEntry>
readZipCentralDirectory(const ZipReadAt& readAt, uint64_t archiveSize)
{
    const DirectoryLocation location = locateDirectory(readAt, archiveSize);
    if (location.start > archiveSize || location.size > archiveSize - location.start) {
        notZip();
    }
    std::vector<uint8_t> directory(static_cast<size_t>(location.size));
    if (!directory.empty()) {
        readExactly(readAt, location.start, directory.data(), directory.size());
    }
    std::vector<ZipDirectoryEntry> entries;
    entries.reserve(static_cast<size_t>(std::min<uint64_t>(location.count, 1U << 20)));
    size_t position = 0;
    for (uint64_t k = 0; k < location.count; ++k) {
        if (directory.size() - position < centralRecordSize
            || le(directory.data() + position, 4) != centralSignature) {
            notZip();
        }
        const uint8_t* record = directory.data() + position;
        const auto nameLength = static_cast<size_t>(le(record + 28, 2));
        const auto extraLength = static_cast<size_t>(le(record + 30, 2));
        const auto commentLength = static_cast<size_t>(le(record + 32, 2));
        const size_t recordLength = centralRecordSize + nameLength + extraLength + commentLength;
        if (directory.size() - position < recordLength) {
            notZip();
        }
        ZipDirectoryEntry entry;
        ZipEntryInfo& info = entry.info;
        info.versionMadeBy = static_cast<uint16_t>(le(record + 4, 2));
        info.flags = static_cast<uint16_t>(le(record + 8, 2));
        info.method = static_cast<uint16_t>(le(record + 10, 2));
        info.crc32 = static_cast<uint32_t>(le(record + 16, 4));
        const auto compressed32 = static_cast<uint32_t>(le(record + 20, 4));
        const auto size32 = static_cast<uint32_t>(le(record + 24, 4));
        info.compressedSize = compressed32;
        info.size = size32;
        info.externalAttributes = static_cast<uint32_t>(le(record + 38, 4));
        const auto offset32 = static_cast<uint32_t>(le(record + 42, 4));
        entry.localOffset = offset32;
        const std::string rawName(
            reinterpret_cast<const char*>(record + centralRecordSize), nameLength);
        info.name = nameToUtf8(rawName, info.flags);
        entry.nameLength = nameLength;
        info.isEncrypted = (info.flags & encryptedFlag) != 0;
        bool hasNtfsTime = false;
        bool hasUnixTime = false;
        readExtraFields(record + centralRecordSize + nameLength, extraLength, compressed32, size32,
            offset32, entry, hasNtfsTime, hasUnixTime);
        if (!hasNtfsTime && !hasUnixTime) {
            info.modified
                = dosTime(static_cast<uint16_t>(le(record + 12, 2)), static_cast<uint16_t>(le(record + 14, 2)));
        }
        entry.localOffset += location.shift;
        const uint32_t unixMode = info.externalAttributes >> 16;
        const uint8_t host = static_cast<uint8_t>(info.versionMadeBy >> 8);
        const bool unixHost = host == 3 || host == 19;
        info.isDirectory = (!info.name.empty() && info.name.back() == '/')
            || (unixHost && (unixMode & 0170000) == 0040000)
            || (!unixHost && (info.externalAttributes & 0x10) != 0);
        info.isSymlink = unixHost && (unixMode & 0170000) == 0120000;
        entries.push_back(std::move(entry));
        position += recordLength;
    }
    rejectOverlappingEntries(entries);
    return entries;
}
//=============================================================================
static DirectoryLocation
locateDirectory(const ZipReadAt& readAt, uint64_t archiveSize)
{
    if (archiveSize < endRecordSize) {
        notZip();
    }
    // The end record is in the last 64 KiB plus its own size (comment included).
    const uint64_t window = std::min<uint64_t>(archiveSize, 0xFFFF + endRecordSize);
    std::vector<uint8_t> tail(static_cast<size_t>(window));
    readExactly(readAt, archiveSize - window, tail.data(), tail.size());
    size_t found = tail.size();
    for (size_t k = tail.size() - endRecordSize + 1; k-- > 0;) {
        if (le(tail.data() + k, 4) == endSignature) {
            found = k;
            break;
        }
    }
    if (found == tail.size()) {
        notZip();
    }
    const uint64_t endPosition = archiveSize - window + found;
    const uint8_t* end = tail.data() + found;
    DirectoryLocation location;
    location.count = le(end + 10, 2);
    location.size = le(end + 12, 4);
    uint64_t offset = le(end + 16, 4);
    uint64_t directoryEnd = endPosition;
    if (endPosition >= zip64LocatorSize) {
        uint8_t locator[zip64LocatorSize];
        readExactly(readAt, endPosition - zip64LocatorSize, locator, sizeof(locator));
        if (le(locator, 4) == zip64LocatorSignature) {
            // ZIP64: the record sits just before its locator.
            const uint64_t recordPosition = endPosition - zip64LocatorSize - zip64EndSize;
            if (endPosition < zip64LocatorSize + zip64EndSize) {
                notZip();
            }
            uint8_t record[zip64EndSize];
            readExactly(readAt, recordPosition, record, sizeof(record));
            if (le(record, 4) != zip64EndSignature) {
                notZip();
            }
            location.count = le(record + 32, 8);
            location.size = le(record + 40, 8);
            offset = le(record + 48, 8);
            directoryEnd = recordPosition;
        }
    }
    // Bytes before the archive (an executable in front of it) shift every offset.
    if (location.size > directoryEnd || offset > directoryEnd - location.size) {
        notZip();
    }
    location.start = directoryEnd - location.size;
    location.shift = location.start - offset;
    // Each record takes at least 46 bytes.
    if (location.count > location.size / centralRecordSize + 1) {
        notZip();
    }
    return location;
}
//=============================================================================
static void
readExtraFields(const uint8_t* extra, size_t length, uint32_t compressed32, uint32_t size32,
    uint32_t offset32, ZipDirectoryEntry& entry, bool& hasNtfsTime, bool& hasUnixTime)
{
    ZipEntryInfo& info = entry.info;
    size_t position = 0;
    while (length - position >= 4) {
        const auto id = static_cast<uint16_t>(le(extra + position, 2));
        const auto fieldLength = static_cast<size_t>(le(extra + position + 2, 2));
        position += 4;
        if (length - position < fieldLength) {
            break;
        }
        const uint8_t* field = extra + position;
        if (id == 0x0001) {
            // ZIP64: 8-byte values, only for the fields saturated in the record.
            size_t at = 0;
            auto next = [&](uint64_t& value) {
                if (fieldLength - at >= 8) {
                    value = le(field + at, 8);
                    at += 8;
                }
            };
            if (size32 == 0xFFFFFFFFU) {
                next(info.size);
            }
            if (compressed32 == 0xFFFFFFFFU) {
                next(info.compressedSize);
            }
            if (offset32 == 0xFFFFFFFFU) {
                next(entry.localOffset);
            }
        } else if (id == 0x9901 && fieldLength >= 7 && info.method == aesMethod) {
            // WinZip AES: the real method follows the strength.
            info.method = static_cast<uint16_t>(le(field + 5, 2));
        } else if (id == 0x000a && fieldLength >= 32 && le(field + 4, 2) == 1
            && le(field + 6, 2) >= 24) {
            // NTFS times: modification time in 100 ns units since 1601.
            const uint64_t ticks = le(field + 8, 8);
            constexpr uint64_t unixEpoch = 116444736000000000ULL;
            if (ticks >= unixEpoch) {
                info.modified = static_cast<time_t>((ticks - unixEpoch) / 10000000ULL);
                hasNtfsTime = true;
            }
        } else if (id == 0x5455 && fieldLength >= 5 && (field[0] & 1) != 0 && !hasNtfsTime) {
            // Extended timestamp: seconds since 1970.
            info.modified = static_cast<time_t>(static_cast<int32_t>(le(field + 1, 4)));
            hasUnixTime = true;
        }
        position += fieldLength;
    }
}
//=============================================================================
static void
rejectOverlappingEntries(const std::vector<ZipDirectoryEntry>& entries)
{
    // Entries sharing archive bytes are the trick of non-recursive zip bombs.
    std::vector<size_t> order(entries.size());
    for (size_t k = 0; k < order.size(); ++k) {
        order[k] = k;
    }
    std::sort(order.begin(), order.end(), [&](size_t a, size_t b) {
        return entries[a].localOffset < entries[b].localOffset;
    });
    for (size_t k = 1; k < order.size(); ++k) {
        const ZipDirectoryEntry& previous = entries[order[k - 1]];
        // Shortest local header: 30 bytes plus the name.
        const uint64_t previousEnd = previous.localOffset + localHeaderSize
            + previous.nameLength + previous.info.compressedSize;
        if (entries[order[k]].localOffset < previousEnd) {
            throw ZipArchiveError(ZipErrorCode::OverlappingEntries,
                "ZIP entries share the same data.", entries[order[k]].info.name);
        }
    }
}
//=============================================================================
static void
readExactly(const ZipReadAt& readAt, uint64_t offset, uint8_t* buffer, size_t size)
{
    if (readAt(offset, buffer, size) != size) {
        notZip();
    }
}
//=============================================================================
static uint64_t
le(const uint8_t* bytes, size_t length)
{
    uint64_t value = 0;
    for (size_t k = length; k-- > 0;) {
        value = (value << 8) | bytes[k];
    }
    return value;
}
//=============================================================================
static time_t
dosTime(uint16_t time, uint16_t date)
{
    std::tm parts = { };
    parts.tm_year = ((date >> 9) & 0x7F) + 80;
    parts.tm_mon = ((date >> 5) & 0x0F) - 1;
    parts.tm_mday = date & 0x1F;
    parts.tm_hour = (time >> 11) & 0x1F;
    parts.tm_min = (time >> 5) & 0x3F;
    parts.tm_sec = (time & 0x1F) * 2;
    parts.tm_isdst = -1;
    const time_t value = std::mktime(&parts);
    return value == static_cast<time_t>(-1) ? 0 : value;
}
//=============================================================================
static bool
isValidUtf8(const std::string& text)
{
    size_t i = 0;
    while (i < text.size()) {
        const auto c = static_cast<unsigned char>(text[i]);
        size_t extra = 0;
        if (c < 0x80) {
            extra = 0;
        } else if ((c & 0xE0) == 0xC0 && c >= 0xC2) {
            extra = 1;
        } else if ((c & 0xF0) == 0xE0) {
            extra = 2;
        } else if ((c & 0xF8) == 0xF0 && c <= 0xF4) {
            extra = 3;
        } else {
            return false;
        }
        if (extra > 0 && i + extra >= text.size()) {
            return false;
        }
        for (size_t k = 1; k <= extra; ++k) {
            if ((static_cast<unsigned char>(text[i + k]) & 0xC0) != 0x80) {
                return false;
            }
        }
        i += extra + 1;
    }
    return true;
}
//=============================================================================
// Names without the UTF-8 flag are CP437, unless they already are valid UTF-8.
static std::string
nameToUtf8(const std::string& raw, uint16_t flags)
{
    if ((flags & utf8Flag) != 0 || isValidUtf8(raw)) {
        return raw;
    }
    std::string out;
    out.reserve(raw.size() * 2);
    for (char ch : raw) {
        const auto c = static_cast<unsigned char>(ch);
        appendUtf8(out, c < 0x80 ? static_cast<char32_t>(c) : cp437High[c - 0x80]);
    }
    return out;
}
//=============================================================================
static void
notZip(const std::string& message)
{
    throw ZipArchiveError(ZipErrorCode::NotAZipArchive, message);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
