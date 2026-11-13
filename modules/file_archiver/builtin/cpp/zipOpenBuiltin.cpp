//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipOpenBuiltin.hpp"
#include "zipBuiltinHelpers.hpp"
#include "ZipArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// id = __zip_open__(source, mode[, password[, encryption]]): file name, uint8 bytes ('r') or
// [] for a new in-memory archive; the password decrypts entries when they are read and, in
// write modes, encrypts the new files with encryption ('aes256' by default).
ArrayOfVector
Nelson::FileArchiverGateway::zipOpenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    const std::string mode = argIn[1].asUtf8String();
    uint64_t id = 0;
    try {
        if (argIn[0].dataClass() == NLS_UINT8) {
            if (mode != "r") {
                Error(L"Nelson:io:zip:memoryMode",
                    L"Archive bytes can only be opened with mode 'r'.");
            }
            id = ZipArchiveSessions::openMemory(byteVector(argIn[0]));
        } else if (argIn[0].isEmpty() && argIn[0].isNumeric()) {
            if (mode != "w") {
                Error(L"Nelson:io:zip:newMemoryMode", L"A new in-memory archive needs mode 'w'.");
            }
            id = ZipArchiveSessions::createInMemory();
        } else {
            id = ZipArchiveSessions::openFile(argIn[0].asWideString(), mode);
        }
        if (argIn.size() > 2) {
            const std::string password = argIn[2].asUtf8String();
            ZipArchive& archive = ZipArchiveSessions::get(id);
            archive.setPassword(password);
            if (mode != "r") {
                ZipEncryption encryption = ZipEncryption::Aes256;
                if (argIn.size() > 3
                    && !ZipArchive::encryptionFromName(argIn[3].asUtf8String(), encryption)) {
                    ZipArchiveSessions::close(id);
                    Error(L"Nelson:io:zip:unknownEncryption",
                        L"Encryption must be 'aes256', 'aes192', 'aes128' or 'zipcrypto'.");
                }
                archive.setEncryption(encryption, password);
            }
        }
    } catch (const ZipArchiveError& error) {
        if (error.code() == ZipErrorCode::OpenFailed
            || error.code() == ZipErrorCode::NotAZipArchive) {
            const std::wstring source
                = argIn[0].dataClass() == NLS_UINT8 ? L"<bytes>" : argIn[0].asWideString();
            if (error.code() == ZipErrorCode::OpenFailed) {
                Error(L"Nelson:io:zip:cannotOpen", L"Cannot open ZIP archive: %s", source);
            }
            Error(L"Nelson:io:zip:notZipArchive", L"Not a valid ZIP archive: %s", source);
        }
        raiseZipError(error);
    }
    ArrayOfVector retval;
    retval << ArrayOf::doubleScalar(static_cast<double>(id));
    return retval;
}
//=============================================================================
