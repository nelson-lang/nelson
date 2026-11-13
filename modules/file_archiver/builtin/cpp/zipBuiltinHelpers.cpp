//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::FileArchiverGateway {
//=============================================================================
void
raiseZipError(const ZipArchiveError& error)
{
    const std::wstring entry = utf8_to_wstring(error.entry());
    switch (error.code()) {
    case ZipErrorCode::EntryNotFound:
        Error(L"Nelson:io:zip:entryNotFound", L"ZIP entry not found: %s", entry);
        break;
    case ZipErrorCode::UnsupportedCompression:
        Error(L"Nelson:io:zip:unsupportedCompression",
            L"Compression method not supported by this build for entry: %s", entry);
        break;
    case ZipErrorCode::EncryptedEntry:
        Error(L"Nelson:io:zip:encryptedEntry",
            L"Encrypted entry needs a password ('Password' option): %s", entry);
        break;
    case ZipErrorCode::WrongPassword:
        Error(L"Nelson:io:zip:wrongPassword", L"Wrong password for entry: %s", entry);
        break;
    case ZipErrorCode::CrcMismatch:
        Error(L"Nelson:io:zip:checksumMismatch", L"Checksum mismatch for entry: %s", entry);
        break;
    case ZipErrorCode::ReadFailed:
    case ZipErrorCode::EntryTooLarge:
    case ZipErrorCode::EntryTruncated:
        Error(L"Nelson:io:zip:cannotRead", L"Cannot read ZIP entry: %s", entry);
        break;
    case ZipErrorCode::SizeLimitExceeded:
        Error(L"Nelson:io:zip:tooLarge", L"ZIP entry exceeds MaxSize: %s", entry);
        break;
    case ZipErrorCode::EntryStillOpen:
        Error(
            L"Nelson:io:zip:entryOpen", L"Close the file identifier of ZIP entry %s first.", entry);
        break;
    case ZipErrorCode::WriteFailed:
        Error(L"Nelson:io:zip:cannotWrite", L"Cannot write ZIP entry: %s", entry);
        break;
    case ZipErrorCode::InvalidState:
        Error(L"Nelson:io:zip:invalidState", L"%s", utf8_to_wstring(error.what()));
        break;
    case ZipErrorCode::DuplicateEntry:
        Error(
            L"Nelson:io:zip:duplicateEntry", L"The archive already has an entry named: %s", entry);
        break;
    case ZipErrorCode::OverlappingEntries:
        Error(L"Nelson:io:zip:overlappingEntries",
            L"ZIP entries share the same data (zip bomb): %s", entry);
        break;
    case ZipErrorCode::InvalidArgument:
        Error(L"Nelson:io:zip:invalidArgument", L"%s", utf8_to_wstring(error.what()));
        break;
    case ZipErrorCode::OpenFailed:
        Error(L"Nelson:io:zip:cannotOpen", L"Cannot open ZIP archive: %s", entry);
        break;
    case ZipErrorCode::NotAZipArchive:
    default:
        Error(L"Nelson:io:zip:notZipArchive", L"Not a valid ZIP archive: %s", entry);
        break;
    }
    throw error;
}
//=============================================================================
uint64_t
archiveId(const ArrayOf& value)
{
    return static_cast<uint64_t>(value.asDoubleScalar());
}
//=============================================================================
std::vector<uint8_t>
byteVector(const ArrayOf& value)
{
    if (value.dataClass() == NLS_UINT8) {
        const auto* data = static_cast<const uint8_t*>(value.dataPointer());
        return std::vector<uint8_t>(data, data + value.elementCount());
    }
    if (value.isEmpty()) {
        return { };
    }
    const std::string text = value.asUtf8String();
    return std::vector<uint8_t>(text.begin(), text.end());
}
//=============================================================================
ZipWriteOptions
writeOptions(const ArrayOf& compression, const ArrayOf& level)
{
    ZipWriteOptions options;
    const std::string name = compression.asUtf8String();
    if (!ZipArchive::compressionFromName(name, options.method)) {
        Error(L"Nelson:io:zip:unknownCompression",
            L"Compression must be 'store', 'deflate', 'zstd', 'xz', 'bzip2' or 'lzma': %s",
            utf8_to_wstring(name));
    }
    if (!ZipArchive::supportsCompression(static_cast<uint16_t>(options.method))) {
        Error(L"Nelson:io:zip:unsupportedCompression",
            L"Compression method not supported by this build for entry: %s", utf8_to_wstring(name));
    }
    const double value = level.isEmpty() ? -1 : level.asDoubleScalar();
    const bool isZstd = options.method == ZipCompression::Zstd;
    const double minimum = isZstd ? 1 : 0;
    const double maximum = 9;
    if (value != -1 && (value != static_cast<int>(value) || value < minimum || value > maximum)) {
        Error(L"Nelson:io:zip:invalidLevel", L"Level must be an integer from %d to %d.",
            static_cast<int>(minimum), static_cast<int>(maximum));
    }
    options.level = static_cast<int>(value);
    return options;
}
//=============================================================================
ArrayOf
stringColumn(const wstringVector& values)
{
    Dimensions column(values.size(), 1);
    return ArrayOf::stringArray(values, column);
}
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
