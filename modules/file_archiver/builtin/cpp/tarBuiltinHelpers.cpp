//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::FileArchiverGateway {
//=============================================================================
void
raiseTarError(const ZipArchiveError& error, const std::wstring& source)
{
    const std::wstring entry = utf8_to_wstring(error.entry());
    switch (error.code()) {
    case ZipErrorCode::EntryNotFound:
        Error(L"Nelson:io:tar:entryNotFound", L"tar entry not found: %s", entry);
        break;
    case ZipErrorCode::UnsupportedCompression:
        Error(L"Nelson:io:tar:unsupportedCompression",
            L"This compression is not available in this build: %s", source);
        break;
    case ZipErrorCode::ReadFailed:
    case ZipErrorCode::EntryTruncated:
        Error(L"Nelson:io:tar:cannotRead", L"Cannot read tar data: %s",
            entry.empty() ? source : entry);
        break;
    case ZipErrorCode::EncryptedEntry:
        Error(L"Nelson:io:archive:encryptedEntry",
            L"Encrypted entry needs a password ('Password' option): %s", entry);
        break;
    case ZipErrorCode::WrongPassword:
        Error(L"Nelson:io:archive:wrongPassword", L"Wrong password for entry: %s", entry);
        break;
    case ZipErrorCode::CrcMismatch:
        Error(L"Nelson:io:archive:checksumMismatch", L"Checksum mismatch for entry: %s", entry);
        break;
    case ZipErrorCode::UnsupportedEncryption:
        Error(L"Nelson:io:archive:unsupportedEncryption",
            L"The encryption of this entry is not supported: %s", entry);
        break;
    case ZipErrorCode::SizeLimitExceeded:
        Error(L"Nelson:io:tar:tooLarge", L"tar entry exceeds MaxSize: %s", entry);
        break;
    case ZipErrorCode::EntryStillOpen:
        Error(
            L"Nelson:io:tar:entryOpen", L"Close the file identifier of tar entry %s first.", entry);
        break;
    case ZipErrorCode::DuplicateEntry:
        Error(
            L"Nelson:io:tar:duplicateEntry", L"The archive already has an entry named: %s", entry);
        break;
    case ZipErrorCode::OpenFailed:
        Error(L"Nelson:io:tar:cannotOpen", L"Cannot open tar archive: %s", source);
        break;
    case ZipErrorCode::NotAZipArchive:
        Error(L"Nelson:io:tar:notTarArchive", L"Not a tar archive: %s", source);
        break;
    case ZipErrorCode::WriteFailed:
        Error(L"Nelson:io:tar:cannotWrite", L"Cannot write tar archive: %s",
            entry.empty() ? source : entry);
        break;
    case ZipErrorCode::InvalidState:
    case ZipErrorCode::InvalidArgument:
    default:
        Error(L"Nelson:io:tar:invalidState", L"%s", utf8_to_wstring(error.what()));
        break;
    }
    throw error;
}
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
